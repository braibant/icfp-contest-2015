open Formats_t

type move_dir = E | W | SE | SW
type turn_dir = CW | CCW
type action =
| Turn of turn_dir
| Move of move_dir

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

(* TODO : loops *)

type config =
  { full_cells: Bitv.t;
    unit_cells: Bitv.t;
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

module HashableConfig =
struct
  type t = config
  let equal config1 config2 =
    (config1.full_cells, config1.unit_cells, config1.unit_pivot, config1.unit_no) =
    (config2.full_cells, config2.unit_cells, config2.unit_pivot, config2.unit_no)

  let hash config =
    Hashtbl.hash (config.full_cells, config.unit_cells,
                  config.unit_pivot, config.unit_no)
end
module HashConfig = Hashtbl.Make(HashableConfig)

let bit_of_coord conf (x,y) = x+y*width conf
let bit_of_cell conf cell = bit_of_coord conf (Cell.to_coord cell)

let coord_of_bit conf bit =
  let w = width conf in
  let r = bit/w in
  (bit-r*w, r)
let cell_of_bit conf bit = Cell.of_coord (coord_of_bit conf bit)

let create_bitv conf = Bitv.create (width conf*height conf) false

type invalid_kind = int
let invalid_overlap = 1
let invalid_bottom = 2
let invalid_leftright = 4
let invalid_up = 8
let valid = 0
exception Invalid_conf of invalid_kind
exception End of int *  action list

let unit_overlap conf =
  not (Bitv.all_zeros (Bitv.bw_and conf.full_cells conf.unit_cells))

let check_cell conf cell =
  let c, r = Cell.to_coord cell in
  let k = 0 in
  let k = if c < 0 || c >= width conf then k lor invalid_leftright else k in
  let k = if r < 0 then k lor invalid_up else k in
  let k = if r >= height conf then k lor invalid_bottom else k in
  k

let move dir conf =
  let delta = Cell.delta_of_move dir in
  let unit_cells = create_bitv conf in
  let ik = ref valid in
  Bitv.iteri_true (fun bit ->
    let newcell = Cell.(cell_of_bit conf bit + delta) in
    let ik' = check_cell conf newcell in
    ik := !ik lor ik';
    if ik' = valid then
      Bitv.set unit_cells (bit_of_cell conf newcell) true)
    conf.unit_cells;
  let conf =
    { conf with
      unit_cells;
      unit_pivot = Cell.(delta + conf.unit_pivot);
      commands = Move dir::conf.commands }
  in
  if unit_overlap conf then ik := !ik lor invalid_overlap;
  if !ik = valid then conf
  else raise (Invalid_conf !ik)

let rotate dir conf =
  let unit_cells = create_bitv conf in
  let ik = ref valid in
  Bitv.iteri_true (fun bit ->
    let newcell = Cell.(conf.unit_pivot + rotate dir (cell_of_bit conf bit-conf.unit_pivot)) in
    let ik' = check_cell conf newcell in
    ik := !ik lor ik';
    if ik' = valid then
      Bitv.set unit_cells (bit_of_cell conf newcell) true)
    conf.unit_cells;
  let conf = { conf with
    unit_cells;
    commands = Turn dir::conf.commands }
  in
  if unit_overlap conf then ik := !ik lor invalid_overlap;
  if !ik = valid then conf
  else raise (Invalid_conf !ik)

let spawn_unit conf act =
  let rng = Int32.(to_int (logand (shift_right_logical conf.rng_state 16) 0x7FFFl)) in
  let unit_id = rng mod (List.length conf.problem.units) in
  let unit = List.nth conf.problem.units unit_id in
  let shift_y = -List.fold_left (fun acc c -> min acc c.y) (1000000) unit.members in
  let min_x = List.fold_left (fun acc c -> min acc c.x) (1000000) unit.members in
  let max_x = List.fold_left (fun acc c -> max acc c.x) (-1000000) unit.members in
  let shift_x = (width conf-max_x-min_x-1) asr 1 in
  let unit = {
    members = List.map (fun {x; y} -> {x=x+shift_x; y=y+shift_y}) unit.members;
    pivot = {x=unit.pivot.x+shift_x;y=unit.pivot.y+shift_y}
  } in
  let conf = { conf with
    unit_cells =
      Bitv.of_list_with_length
        (List.map (fun {x;y} -> bit_of_coord conf (x,y)) unit.members)
        (width conf*height conf);
    unit_pivot = Cell.of_coord (unit.pivot.x, unit.pivot.y);
    rng_state = Int32.(add (mul 1103515245l conf.rng_state) 12345l);
    unit_no = conf.unit_no + 1;
    commands =
      match act with
      | None -> conf.commands
      | Some act -> act::conf.commands }
  in
  if conf.unit_no = conf.problem.sourceLength then
    raise (End (conf.score, List.rev conf.commands))
  else
    if unit_overlap conf
    then raise (End (conf.score, List.rev conf.commands))
    else conf

let lock conf act =
  let size = ref 0 in
  Bitv.iteri_true (fun _ -> incr size) conf.unit_cells;
  let size = !size in
  let conf = ref {
    conf with full_cells = Bitv.bw_or conf.full_cells conf.unit_cells }
  in
  let ls = ref 0 in
  for r = height !conf-1 downto 0 do
    let rec is_full c =
      if c < 0 then true
      else Bitv.get !conf.full_cells (bit_of_coord !conf (c, r)) && is_full (c-1)
    in
    if is_full (width !conf-1) then
      begin
        incr ls;
        let full_cells = create_bitv !conf in
        Bitv.iteri_true (fun bit ->
          let c', r' = coord_of_bit !conf bit in
          if r' = r then ()
          else if r' < r then Bitv.set full_cells (bit_of_coord !conf (c', r'+1)) true
          else Bitv.set full_cells (bit_of_coord !conf (c', r')) true)
          !conf.full_cells;
        conf := { !conf with full_cells }
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
  spawn_unit conf act

let init pb ~seed_id =
  let conf =
    { full_cells = Bitv.create 0 false;
      unit_cells = Bitv.create 0 false;
      unit_pivot = (0, 0);
      rng_state = Int32.of_int (List.nth pb.sourceSeeds seed_id);
      unit_no = -1;
      problem = pb;
      ls_old = 0;
      score = 0;
      commands = []}
  in
  let conf = { conf with
    full_cells =
      Bitv.of_list_with_length
        (List.map (fun {x;y} -> bit_of_coord conf (x,y)) pb.filled)
        (width conf*height conf) }
  in
  spawn_unit conf None

let action_of_char = function
  | 'p' | '\''| '!' | '.' | '0' | '3' -> Some (Move W)
  | 'b' | 'c' | 'e' | 'f' | 'y' | '2' -> Some (Move E)
  | 'a' | 'g' | 'h' | 'i' | 'j' | '4' -> Some (Move SW)
  | 'l' | 'm' | 'n' | 'o' | ' ' | '5' -> Some (Move SE)
  | 'd' | 'q' | 'r' | 'v' | 'z' | '1' -> Some (Turn CW)
  | 'k' | 's' | 't' | 'u' | 'w' | 'x' -> Some (Turn CCW)
  | '\t'| '\n'| '\r' -> None
  | _ -> assert false

let play_action conf act =
  match act with
  | Move dir ->
    begin
      try move dir conf
      with Invalid_conf _ -> lock conf (Some act)
    end
  | Turn dir ->
    begin
      try rotate dir conf
      with Invalid_conf _ -> lock conf (Some act)
    end

let play_game commands pb seed_id =
  let conf = ref (init pb ~seed_id) in
  try
    String.iter (fun c ->
      match action_of_char c with
      | None -> ()
      | Some act -> conf := play_action !conf act) commands;
    assert false
  with End (score, _) -> score


let check_game commands pb seed_id =
  let conf = ref (init pb seed_id) in
  let history = HashConfig.create 17 in
  let valid = ref true in
  try
    String.iter (fun c ->
      match action_of_char c with
      | None -> ()
      | Some act ->
        valid := !valid && not (HashConfig.mem history !conf);
        HashConfig.add history !conf ();
        conf := play_action !conf act) commands;
    !valid
  with End (score, _) -> !valid
