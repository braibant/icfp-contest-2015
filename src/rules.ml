open Formats_t

type move_dir = E | W | SE | SW
type turn_dir = CW | CCW
type action =
| Turn of turn_dir
| Move of move_dir
| Nop

(* North-west tile is at coordinate 0,0.
   Then, going South-West increments by 1,0
               South-East increments by 0,1 *)
module Cell =
struct
  type t = int * int
  let compare (a:t) (b:t) = compare a b

  let of_coord (column, row) =
    (row asr 1-column, (row+1) asr 1+column)

  let to_coord (sw, se) =
    ((se-sw) asr 1,sw+se)

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
    problem: Formats_t.input;

    score: int;
    ls_old: int;

    commands : action list;
  }

let width config = config.problem.Formats_t.width
let height config = config.problem.Formats_t.height

exception Invalid_conf
exception End of int *  action list

let check_unit_bounds conf =
  if not (CSet.is_empty (CSet.inter conf.full_cells conf.unit_cells)) then
    raise Invalid_conf;
  CSet.iter (fun cell ->
    let (c, r) = Cell.to_coord cell in
    if c < 0 || c >= width conf || r < 0 || r >= height conf then
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
  let shift_x = ((width conf-max_x+1)-min_x) asr 1 in
  let unit = {
    members = List.map (fun {x; y} -> {x=x+shift_x; y=y+shift_y}) unit.members;
    pivot = {x=unit.pivot.x+shift_x;y=unit.pivot.y+shift_y}
  } in
  let conf = { conf with
    unit_cells =
      CSet.of_list (List.map (fun {x;y} -> Cell.of_coord (x,y)) unit.members);
    unit_pivot = Cell.of_coord (unit.pivot.x, unit.pivot.y);
    rng_state = Int32.(add (mul 1103515245l conf.rng_state) 12345l);
    unit_no = conf.unit_no + 1 }
  in
  if conf.unit_no = conf.problem.sourceLength then
    raise (End (conf.score, List.rev conf.commands))
  else
    try check_unit_bounds conf; conf
    with Invalid_conf -> raise (End (conf.score, List.rev conf.commands))

let lock conf =
  let size = CSet.cardinal conf.unit_cells in
  let conf = ref {
    conf with full_cells = CSet.union conf.full_cells conf.unit_cells }
  in
  let ls = ref 0 in
  for r = height !conf-1 downto 0 do
    let rec is_full c =
      if c < 0 then true
      else CSet.mem (Cell.of_coord (c, r)) !conf.full_cells && is_full (c-1)
    in
    if is_full (width !conf-1) then
      begin
        incr ls;
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
  let conf = !conf and ls = !ls in
  let points = size + 100*(1+ls)*ls/2 in
  let lines_bonus = if conf.ls_old > 1 then ((conf.ls_old-1)*points/10) else 0 in
  let conf = {
    conf with
      ls_old = ls;
      score = conf.score + points + lines_bonus
  } in
  spawn_unit conf

let init pb seed_id =
  let conf =
    { full_cells = CSet.of_list (List.map (fun {x;y} -> Cell.of_coord (x,y)) pb.filled);
      unit_cells = CSet.empty;
      unit_pivot = (0, 0);
      rng_state = Int32.of_int (List.nth pb.sourceSeeds seed_id);
      unit_no = -1;
      problem = pb;
      ls_old = 0;
      score = 0;
      commands = []}
  in
  spawn_unit conf

let action_of_char = function
  | 'p' | '\''| '!' | '.' | '0' | '3' -> Move W
  | 'b' | 'c' | 'e' | 'f' | 'y' | '2' -> Move E
  | 'a' | 'g' | 'h' | 'i' | 'j' | '4' -> Move SW
  | 'l' | 'm' | 'n' | 'o' | ' ' | '5' -> Move SE
  | 'd' | 'q' | 'r' | 'v' | 'z' | '1' -> Turn CW
  | 'k' | 's' | 't' | 'u' | 'w' | 'x' -> Turn CCW
  | '\t'| '\n'| '\r' -> Nop
  | _ -> assert false

(* play an action, without adding the corresponding command *)
let play_action conf = function
  | Move dir ->
    begin
      try move dir conf
      with Invalid_conf -> lock conf
    end
  | Turn dir ->
    begin
      try rotate dir conf
      with Invalid_conf -> lock conf
    end
  | Nop -> conf

(* add the command that correspond to an action *)
let play_action conf command =
  let conf = play_action conf command in
  {conf with commands = command :: conf.commands}

let play_game commands pb seed_id =
  let conf = ref (init pb seed_id) in
  try
    String.iter (fun c -> conf := play_action !conf (action_of_char c)) commands;
    assert false
  with End (score, _) -> score
