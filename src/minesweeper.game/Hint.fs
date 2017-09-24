namespace Hint

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
    let private isPotentialMine (cell:Cell) =
        cell.State = CellState.Hidden || cell.State = CellState.Flagged

    let private isHidden (cell:Cell) =
        cell.State = CellState.Hidden

    let private isFlagged (cell:Cell) =
        cell.State = CellState.Flagged

    let private isExposed (cell:Cell) =
        cell.State = CellState.Exposed

    let private selectIndex (cell:Cell) =
        cell.Coords.Index

    let private withExposedNeighbors game indexes =
        let includeNeighborCells cell =
            Game.getNeighborCells cell game
            |> Seq.append [cell]
        indexes
            |> Seq.map (Game.getCell game)
            |> Seq.collect includeNeighborCells
            |> Seq.filter isExposed
            |> Seq.map selectIndex
            |> Set.ofSeq

    let private withNeighborsOfZeros game indexes =
        let neighborsOfZeros (cell:Cell) =
            match (cell.State, cell.SurroundingCount) with
            | (CellState.Exposed, Some 0) ->
                Game.getNeighborCells cell game
            | _ -> Seq.empty<Cell>
        let rec expand previousGeneration previousIndexes =
            let notInSet cell = not (Set.contains cell.Coords.Index previousIndexes)
            let nextGeneration =
                previousGeneration
                    |> Seq.collect neighborsOfZeros
                    |> Seq.filter notInSet
            match Seq.isEmpty nextGeneration with
            | false -> expand nextGeneration (Set.union previousIndexes (Set.ofSeq (Seq.map selectIndex nextGeneration)))
            | true -> previousIndexes
        expand (Seq.map (Game.getCell game) indexes) indexes

    let private flagsSurroundingCell game flags index =
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

    let private flagStrategy progress nextActions =
        let neighbors = withExposedNeighbors progress.Game progress.CellsSwept
        let findFlags = flagsSurroundingCell progress.Game
        let flags =
            neighbors
            |> Seq.fold findFlags Set.empty
            |> Set.union nextActions.CellsToFlag
        { nextActions with CellsToFlag = flags }

    let private safeSurroundingCell game safe index =
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

    let private safeStrategy progress nextActions =
        let findSafe = safeSurroundingCell progress.Game
        let safe =
            progress.CellsSwept
            |> Seq.fold findSafe Set.empty
            |> Set.union nextActions.CellsToSweep
        { nextActions with CellsToSweep = safe }

    let private allStrategies progress =
        {
            CellsToSweep = Set.empty
            CellsToFlag = Set.empty
        }
        |> flagStrategy progress
        |> safeStrategy progress

    let private apply game nextActions =
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
            CellsSwept = withNeighborsOfZeros endGame nextActions.CellsToSweep
            CellsFlagged = nextActions.CellsToFlag
        }

    let rec private run progress =
        let nextActions = allStrategies progress
        match (Set.isEmpty nextActions.CellsToFlag) && (Set.isEmpty nextActions.CellsToSweep) with
        | false -> apply progress.Game nextActions |> run
        | true -> progress.Game

    let afterSweep x y game =
        let index = Coordinates.getArrayIndex x y game.GameSize
        let progress =
            {
                Game = game
                CellsSwept =
                    [index]
                    |> Set.ofSeq
                    |> withNeighborsOfZeros game
                CellsFlagged = Set.empty
            }
        run progress

    let afterSweepAllHiddenNeighbors x y game =
        let index = Coordinates.getArrayIndex x y game.GameSize
        let progress =
            {
                Game = game
                CellsSwept =
                    [index]
                    |> Set.ofSeq
                    |> withExposedNeighbors game
                    |> withNeighborsOfZeros game
                CellsFlagged = Set.empty
            }
        run progress

    let afterFlag x y game =
        let index = Coordinates.getArrayIndex x y game.GameSize
        let progress =
            {
                Game = game
                CellsSwept = Set.empty
                CellsFlagged = Set.ofSeq [index]
            }
        run progress
