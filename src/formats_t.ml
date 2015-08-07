(* Auto-generated from "formats.atd" *)


type cell = { x: int; y: int }

type unit_t = { members: cell list; pivot: cell }

type commands = string

type output = { problemId: int; seed: int; tag: string; solution: commands }

type output_l = output list

type input = {
  id: int;
  units: unit_t list;
  width: int;
  height: int;
  filled: cell list;
  sourceLength: int;
  sourceSeeds: int list
}
