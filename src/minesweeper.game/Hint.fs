namespace Hint

open Coordinates
open Cells
open Games
open Commands

module Hint =
    let flagsSurroundingCell game flags cell =
        match cell.SurroundingCount with
        | Some count ->
            let neighbors = Game.getNeighborCells cell game
            match Seq.length neighbors = count with
            | true -> Set.union flags (Set.ofSeq neighbors)
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

