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

    let flagsSurroundingCell game flags (cell:Cell) =
        match (cell.State, cell.SurroundingCount) with
        | (CellState.Exposed, Some count) ->
            let neighbors = Game.getNeighborCells cell game
            let potentialMines = Seq.filter isPotentialMine neighbors
            let hiddenNeighbors = Seq.filter isHidden neighbors
            match Seq.length potentialMines = count with
            | true -> Set.union flags (Set.ofSeq hiddenNeighbors)
            | false -> flags
        | _ -> flags

    let setFlags game =
        let pairs = Map.toSeq game.Cells
        let cells = Seq.map (fun (_, x) -> x) pairs
        let folder = flagsSurroundingCell game
        let flags = Seq.fold folder Set.empty cells
        let indexes = Seq.map (fun c -> c.Coords.Index) (Set.toSeq flags)
        let setFlag = fun g index -> Flag.flagByIndex index g
        let folded = Seq.fold setFlag game indexes
        folded

    let safeSurroundingCell game safe (cell:Cell) =
        match (cell.State, cell.SurroundingCount) with
        | (CellState.Exposed, Some count) ->
            let neighbors = Game.getNeighborCells cell game
            let flaggedMines = Seq.filter isFlagged neighbors
            let hiddenNeighbors = Seq.filter isHidden neighbors
            match Seq.length flaggedMines = count with
            | true -> Set.union safe (Set.ofSeq hiddenNeighbors)
            | false -> safe
        | _ -> safe

    let clearSafeCells game =
        let pairs = Map.toSeq game.Cells
        let cells = Seq.map (fun (_, x) -> x) pairs
        let folder = safeSurroundingCell game
        let flags = Seq.fold folder Set.empty cells
        let indexes = Seq.map (fun c -> c.Coords.Index) (Set.toSeq flags)
        let clearCell = fun g index -> Sweep.sweepByIndex index g
        let folded = Seq.fold clearCell game indexes
        folded

    let afterSweep x y game =
        game |> setFlags |> clearSafeCells

    let afterSweepAllHiddenNeighbors x y game =
        game |> setFlags |> clearSafeCells
