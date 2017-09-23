﻿namespace Hint

open Coordinates
open Cells
open Games
open Commands

type Progress = {
    Game: Game
    CellsSwept: Set<int>
    CellsFlagged: Set<int>
}

type NextActions = {
    CellsToSweep: Set<int>
    CellsToFlag: Set<int>
}

type Strategy = Progress -> NextActions -> NextActions
type Apply = Game -> NextActions -> Progress

module Hint =
    let isPotentialMine (cell:Cell) =
        cell.State = CellState.Hidden || cell.State = CellState.Flagged

    let isHidden (cell:Cell) =
        cell.State = CellState.Hidden

    let isFlagged (cell:Cell) =
        cell.State = CellState.Flagged

    let isExposed (cell:Cell) =
        cell.State = CellState.Exposed

    let selectIndex (cell:Cell) =
        cell.Coords.Index

    let expand (game:Game) (indexes:Set<int>) =
        let includeNeighborCells cell =
            Game.getNeighborCells cell game
            |> Seq.append [cell]
        indexes
            |> Seq.map (Game.getCell game)
            |> Seq.collect includeNeighborCells
            |> Seq.filter isExposed
            |> Seq.map selectIndex
            |> Set.ofSeq

    let flagsSurroundingCell game flags index =
        let cell = Game.getCell game index
        match (cell.State, cell.SurroundingCount) with
        | (CellState.Exposed, Some count) ->
            let neighbors = Game.getNeighborCells cell game
            let potentialMines = Seq.filter isPotentialMine neighbors
            match Seq.length potentialMines = count with
            | true ->
                neighbors
                |> Seq.filter isHidden
                |> Seq.map selectIndex
                |> Set.ofSeq
                |> Set.union flags
            | false -> flags
        | _ -> flags

    let flagStrategy (progress:Progress) (nextActions:NextActions) =
        let neighbors = expand progress.Game progress.CellsSwept
        let findFlags = flagsSurroundingCell progress.Game
        let flags =
            neighbors
            |> Seq.fold findFlags Set.empty
            |> Set.union nextActions.CellsToFlag
        { nextActions with CellsToFlag = flags }

    let setFlags game =
        let pairs = Map.toSeq game.Cells
        let indexes = Seq.map (fun (x, _) -> x) pairs
        let folder = flagsSurroundingCell game
        let flags = Seq.fold folder Set.empty indexes
        let flagIndexes = Set.toSeq flags
        let setFlag = fun g index -> Flag.flagByIndex index g
        let folded = Seq.fold setFlag game flagIndexes
        folded

    let safeSurroundingCell game safe index =
        let cell = Game.getCell game index
        match (cell.State, cell.SurroundingCount) with
        | (CellState.Exposed, Some count) ->
            let neighbors = Game.getNeighborCells cell game
            let flaggedMines = Seq.filter isFlagged neighbors
            match Seq.length flaggedMines = count with
            | true ->
                neighbors
                |> Seq.filter isHidden
                |> Seq.map selectIndex
                |> Set.ofSeq
                |> Set.union safe
            | false -> safe
        | _ -> safe

    let clearSafeCells game =
        let pairs = Map.toSeq game.Cells
        let indexes = Seq.map (fun (x, _) -> x) pairs
        let folder = safeSurroundingCell game
        let safe = Seq.fold folder Set.empty indexes
        let safeIndexes = Set.toSeq safe
        let clearCell = fun g index -> Sweep.sweepByIndex index g
        let folded = Seq.fold clearCell game safeIndexes
        folded

    let apply (game:Game) (nextActions:NextActions) =
        let sweepActions =
            nextActions.CellsToSweep
            |> Seq.map Sweep.sweepByIndex
        let flagActions =
            nextActions.CellsToFlag
            |> Seq.map Flag.flagByIndex
        let actions = Seq.append sweepActions flagActions
        let endGame = Seq.reduce (>>) actions game
        {
            Game = endGame
            CellsSwept = nextActions.CellsToSweep
            CellsFlagged = nextActions.CellsToFlag
        }

    let afterSweep x y game =
        game |> setFlags |> clearSafeCells

    let afterSweepAllHiddenNeighbors x y game =
        game |> setFlags |> clearSafeCells
