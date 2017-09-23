namespace Hint

open Coordinates
open Cells
open Games
open Commands

module Hint =
    let isPotentialMine (cell:Cell) =
        cell.State = CellState.Hidden || cell.State = CellState.Flagged

    let isHidden (cell:Cell) =
        cell.State = CellState.Hidden

    let isFlagged (cell:Cell) =
        cell.State = CellState.Flagged

    let selectIndex (cell:Cell) =
        cell.Coords.Index

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

    let afterSweep x y game =
        game |> setFlags |> clearSafeCells

    let afterSweepAllHiddenNeighbors x y game =
        game |> setFlags |> clearSafeCells
