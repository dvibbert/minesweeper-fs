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

    let flagsSurroundingCell (game:Game) (flags:Set<Cell>) (cell:Cell) =
        match (cell.State, cell.SurroundingCount) with
        | (CellState.Exposed, Some count) ->
            let neighbors = Game.getNeighborCells cell game
            let potentialBombs = Seq.filter isPotentialMine neighbors
            let hiddenNeighbors = Seq.filter isHidden neighbors
            match Seq.length potentialBombs = count with
            | true -> Set.union flags (Set.ofSeq hiddenNeighbors)
            | false -> flags
        | _ -> flags

    let setFlags game:Game =
        let pairs = Map.toSeq game.Cells
        let cells = Seq.map (fun (_, x) -> x) pairs
        let folder = flagsSurroundingCell game
        let flags = Seq.fold folder Set.empty cells
        let indexes = Seq.map (fun c -> c.Coords.Index) (Set.toSeq flags)
        let setFlag = fun g index -> Game.setCellState index Flagged g
        let folded = Seq.fold setFlag game indexes
        folded

    let afterSweep x y game =
        setFlags game

