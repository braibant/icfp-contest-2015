open Formats_t

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
    let rec aux acc = function
      | [] -> acc
      | t::q ->
        match f t with
        | None -> aux acc q
        | Some t -> aux (t::acc) q
    in
    of_list (aux [] (elements s))
end

type config =
  { full_cells: CSet.t;
    unit_cells: CSet.t;
    unit_pivot: Cell.t;
    rng_state: Int32.t;
    unit_no: int;
    problem: Formats_t.input }

exception Invalid_conf

let check_unit_bounds conf =
  if not (CSet.is_empty (CSet.inter conf.full_cells conf.unit_cells)) then
    raise Invalid_conf;
  CSet.iter (fun cell ->
    let (c, r) = Cell.to_coord cell in
    if c < 0 || c >= conf.problem.width || r < 0 || r >= conf.problem.height then
      raise Invalid_conf)
    conf.unit_cells

let move dir conf =
  let delta = Cell.delta_of_move dir in
  let res =
    { conf with
      unit_cells = CSet.map Cell.(fun c -> Some(c + delta)) conf.unit_cells;
      unit_pivot = Cell.(delta + conf.unit_pivot) }
  in
  check_unit_bounds res;
  res

(* TODO : loops *)
let rotate dir conf =
  let res =
    { conf with
      unit_cells = CSet.map
        Cell.(fun c -> Some (conf.unit_pivot + rotate dir (c-conf.unit_pivot)))
        conf.unit_cells }
  in
  check_unit_bounds res;
  res

let spawn_unit conf =
  let rng = Int32.(to_int (logand (shift_right_logical conf.rng_state 16) 0xEFFFl)) in
  let unit_id = rng mod (List.length conf.problem.units) in
  let unit = List.nth conf.problem.units unit_id in
  let shift_y = -List.fold_left (fun acc c -> min acc c.y) (1000000) unit.members in
  let min_x = List.fold_left (fun acc c -> min acc c.x) (1000000) unit.members in
  let max_x = List.fold_left (fun acc c -> max acc c.x) (-1000000) unit.members in
  let shift_x = ((conf.problem.width-max_x+1)-min_x+1000000)/2-500000 in
  let unit = {
    members = List.map (fun {x; y} -> {x=x+shift_x; y=y+shift_y}) unit.members;
    pivot = {x=unit.pivot.x+shift_x;y=unit.pivot.y+shift_y}
  } in
  let res = { conf with
    unit_cells =
      CSet.of_list (List.map (fun {x;y} -> Cell.of_coord (x,y)) unit.members);
    unit_pivot = Cell.of_coord (unit.pivot.x, unit.pivot.y);
    rng_state = Int32.(add (mul 1103515245l conf.rng_state) 12345l);
    unit_no = conf.unit_no + 1 }
  in
  check_unit_bounds conf;
  res

let lock conf =
  let conf = ref {
    conf with full_cells = CSet.union conf.full_cells conf.unit_cells }
  in
  for r = !conf.problem.height-1 downto 0 do
    let rec is_full c =
      if c < 0 then true
      else CSet.mem (Cell.of_coord (c, r)) !conf.full_cells && is_full (c-1)
    in
    if is_full (!conf.problem.width-1) then
      begin
        conf := { !conf with
          full_cells =
            CSet.map (fun c ->
              let (c', r') = Cell.to_coord c in
              if r' = r then None
              else if r' < r then Some (Cell.of_coord (c', r'+1))
              else Some c)
              !conf.full_cells
        }
      end
  done;
  spawn_unit !conf

let width config = config.problem.Formats_t.width
let height config = config.problem.Formats_t.height

let init (i : Formats_t.input) : config = assert false
