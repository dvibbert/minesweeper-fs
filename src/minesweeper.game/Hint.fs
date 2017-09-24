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

    let withExposedNeighbors (game:Game) (indexes:Set<int>) =
        let includeNeighborCells cell =
            Game.getNeighborCells cell game
            |> Seq.append [cell]
        indexes
            |> Seq.map (Game.getCell game)
            |> Seq.collect includeNeighborCells
            |> Seq.filter isExposed
            |> Seq.map selectIndex
            |> Set.ofSeq

    let withNeighborsOfZeros (game:Game) (indexes:Set<int>) =
        let neighborsOfZeros (cell:Cell) =
            match (cell.State, cell.SurroundingCount) with
            | (CellState.Exposed, Some 0) ->
                Game.getNeighborCells cell game
            | _ -> Seq.empty<Cell>
        let rec expand (previousGeneration:seq<Cell>) (previousIndexes:Set<int>) =
            let notInSet (cell:Cell) = not (Set.contains cell.Coords.Index previousIndexes)
            let nextGeneration =
                previousGeneration
                    |> Seq.collect neighborsOfZeros
                    |> Seq.filter notInSet
            match Seq.isEmpty nextGeneration with
            | false -> expand nextGeneration (Set.union previousIndexes (Set.ofSeq (Seq.map selectIndex nextGeneration)))
            | true -> previousIndexes
        expand (Seq.map (Game.getCell game) indexes) indexes

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
        let neighbors = withExposedNeighbors progress.Game progress.CellsSwept
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

    let allStrategies (progress:Progress) =
        {
            CellsToSweep = Set.empty
            CellsToFlag = Set.empty
        }
        |> flagStrategy progress

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
            CellsSwept = withNeighborsOfZeros game nextActions.CellsToSweep
            CellsFlagged = nextActions.CellsToFlag
        }

    let rec run (progress:Progress) =
        let nextActions = allStrategies progress
        match (Set.isEmpty nextActions.CellsToFlag) && (Set.isEmpty nextActions.CellsToSweep) with
        | false -> run (apply progress.Game nextActions)
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
