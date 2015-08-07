type move_dir = E | W | SE | SW
type turn_dir = CW | CCW


(* North-west tile is at coordinate 0,0.
   Then, going South-West increments by 1,0
               South-East increments by 0,1 *)
module Cell =
struct
  type t = int * int
  let compare = compare

  let of_coord (column, row) =
    if row mod 2 = 0 then
      (row/2-column, row/2+column)
    else
      ((row-1)/2-column, (row-1)/2+column+1)

  let to_coord (sw, se) =
    ((se-sw)/2,sw+se)

  let (+) (sw1,se1) (sw2,se2) = (sw1+sw2, se1+se2)
  let (-) (sw1,se1) (sw2,se2) = (sw1-sw2, se1-se2)
  let (~-) (sw,se) = (-sw, -se)

  let rotate dir (sw, se) =
    match dir with
    | CW -> Pervasives.(se+sw, -sw)
    | CCW -> Pervasives.(-se, sw+se)

  let delta_of_move dir =
    match dir with
    | E -> (-1, +1)
    | W -> (+1, -1)
    | SE -> (0, +1)
    | SW -> (+1, 0)
end

module CSet =
struct
  module M = Set.Make(Cell)
  include M

  let map f s =
    let l = List.map f (elements s) in
    List.fold_left (fun acc e -> add e acc) empty l
end

type config =
  { full_cells: CSet.t;
    unit_cells: CSet.t;
    unit_pivot: Cell.t;
    rng_state: Int32.t;
    problem: Formats_t.input }

exception Invalid_conf

let check_unit_bounds conf =
  let open Formats_t in
  CSet.iter (fun cell ->
    let (c, r) = Cell.to_coord cell in
    if c < 0 || c >= conf.problem.width ||
       r < 0 || r >= conf.problem.height ||
       CSet.mem cell conf.full_cells then
      raise Invalid_conf)
    conf.unit_cells

let move dir conf =
  let delta = Cell.delta_of_move dir in
  let res =
    { conf with
      unit_cells = CSet.map Cell.((+) delta) conf.unit_cells;
      unit_pivot = Cell.(delta + conf.unit_pivot) }
  in
  check_unit_bounds res;
  res

let rotate dir conf =
  let res =
    { conf with
      unit_cells = CSet.map
        Cell.(fun c -> conf.unit_pivot + rotate dir (c-conf.unit_pivot))
        conf.unit_cells }
  in
  check_unit_bounds res;
  res

(* let lock conf = *)
(*   let conf = { *)
(*     conf with *)
(*       full_cells = CSet.union conf.full_cells  *)
(*   } *)
