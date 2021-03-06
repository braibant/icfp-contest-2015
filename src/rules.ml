open Formats_t

(* type move_dir = E | W | SE | SW *)
(* type turn_dir = CW | CCW *)
type action =
  | E | W | SE | SW | CW | CCW

(* North-west tile is at coordinate 0,0.
   Then, going South-West increments by 1,0
               South-East increments by 0,1 *)
module Cell :
sig
  type t = int * int
  val compare : t -> t -> int
  val of_coord : int * int -> t
  val to_coord : t -> int * int
  val make : int * int -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( ~- ) : t -> t
  val rotate : action -> t -> t
  val delta_of_move : action -> t
end
=
struct
  type t = int * int
  let make i = i
  let compare ((a1,a2):t) ((b1,b2):t) =
    let cmp = compare a1 b1 in
    if cmp = 0
    then compare a2 b2
    else cmp

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
    | _ -> assert false

  let delta_of_move dir =
    match dir with
    | E -> (-1, +1)
    | W -> (+1, -1)
    | SE -> (0, +1)
    | SW -> (+1, 0)
    | _ -> assert false
end

module Piece = struct

  type t = {content : Cell.t array; mutable sorted: bool}

  let cells t = t.content

  let rec equal_vect (a: Cell.t array) b i n =
    if i = n
    then true
    else
      let (xa,ya) = a.(i) in
      let (xb,yb) = b.(i) in
      xa == xb && ya == yb && equal_vect a b (i + 1) n


  let equal a b =
    if not a.sorted then (Array.sort Cell.compare a.content; a.sorted <- true);
    if not b.sorted then (Array.sort Cell.compare b.content; b.sorted <- true);
    equal_vect a.content b.content 0 (Array.length a.content)

  (* independent of the order of elements in the array *)
  let hash a =
    let r = ref 0 in
    let a = a.content in
    for i = 0 to Array.length a - 1 do
      let x,y = a.(i) in
      r := !r + x lsl 8 + y
    done;
    !r

  let length a = Array.length a.content

  let of_list l =
    let a = Array.of_list l in
    Array.sort Cell.compare a;
    {content = a; sorted = true}

  let empty = {content = [| |]; sorted = true}
end

type config =  {
  full_cells : Bitv.t;
  unit_cells : Piece.t;
  unit_pivot : Cell.t;
  rng_state : Int32.t;
  unit_no : int;
  unit_id : int;
  score : int;
  ls_old : int;
  commands : action list ;      (* in reverse order *)

  mutable mark : int;
}

type data =
  {
    input: Formats_t.input;
    width : int;
    height : int;
    coord_of_bit : (int * int) array;
    cell_of_bit : Cell.t array;
    units: (Piece.t * Cell.t) array;
    time: float;                   (* time in second allocated for one seed. *)
  }

module Data = struct
  let coord_of_bit width bit =
    let w = width in
    let r = bit/w in
    (bit-r*w, r)

  let cell_of_bit width bit : Cell.t =
    Cell.of_coord (coord_of_bit width bit)

  let bit_of_coord width (x,y) = x+y*width

  let center_unit width height unit =
    let shift_y =
      let dy = -List.fold_left (fun acc c -> min acc c.y) (1000000) unit.members in
      fun {x; y} -> Cell.(to_coord (of_coord (x, y) + make (dy asr 1, (succ dy) asr 1)))
    in
    let members = List.map shift_y unit.members
    and pivot = shift_y unit.pivot in
    let min_x = List.fold_left (fun acc c -> min acc (fst c)) (1000000) members in
    let max_x = List.fold_left (fun acc c -> max acc (fst c)) (-1000000) members in
    let shift_x =
      let dx = (width-max_x-min_x-1) asr 1 in
      fun (x, y) -> (x+dx, y)
    in
    let members = List.map shift_x members
    and pivot = shift_x pivot in
    let members = Piece.of_list ( List.map Cell.of_coord members)
    and pivot = Cell.of_coord pivot in
    (members, pivot)
end

let build_data ~time input =
  let height =  input.Formats_t.height in
  let width =  input.Formats_t.width in
  {input;
   height;
   width;
   coord_of_bit = Array.init (width * height) (Data.coord_of_bit width);
   cell_of_bit = Array.init (width * height) (Data.cell_of_bit width);
   units = Array.map (Data.center_unit width height) (Array.of_list input.units);
   time
  }

(** Memoized data  *)
let width data = data.width
let height data = data.height
let coord_of_bit data bit = data.coord_of_bit.(bit)
let bit_of_coord data (x,y) = x+y*data.width
let cell_of_bit data bit = data.cell_of_bit.(bit)
let bit_of_cell data (sw,se) =
  let x = (se-sw) asr 1 in
  let y = sw+se in
  x+y*data.width
let create_bitv data = Bitv.create (data.width*data.height) false
let number_of_units data = Array.length data.units
let get_unit data id = data.units.(id)
let units data = data.units
let source_length data = data.input.sourceLength
let time data = data.time

module HashableConfig =
struct
  type t = config
  let equal config1 config2 =
    config1.unit_no = config2.unit_no
    && config1.unit_pivot = config2.unit_pivot
    && Bitv.equal config1.full_cells config2.full_cells
    && Piece.equal config1.unit_cells config2.unit_cells
    (* && Bitv.equal config1.unit_cells config2.unit_cells *)


  let hash config =
    Hashtbl.hash (Bitv.hash config.full_cells,
                  Piece.hash config.unit_cells,
                  config.unit_pivot, config.unit_no)

end
module HashConfig = Hashtbl.Make(HashableConfig)


type invalid_kind = int
let invalid_overlap = 1
let invalid_bottom = 2
let invalid_leftright = 4
let invalid_up = 8
let valid = 0
exception Invalid_conf of invalid_kind
exception End of int *  action list


let unit_overlap_flag = ref false
let unit_overlap data conf =
  let r = unit_overlap_flag in
  r := false;
  let n = Bitv.length conf.full_cells in
  let unit = conf.unit_cells.Piece.content in
  let full = conf.full_cells in
  for i = 0 to Array.length unit - 1 do
    let cell = unit.(i) in
    let bit = bit_of_cell data cell in
    if 0 <= bit && bit < n
    then r:= !r || Bitv.get full (bit_of_cell data cell);
  done;
  !r
  (* not (Bitv.all_zeros (Bitv.bw_and conf.full_cells conf.unit_cells)) *)

let check_cell data cell =
  let c, r = Cell.to_coord cell in
  (* let c = (se-sw) asr 1 in *)
  (* let r = sw+se in *)
  let k = 0 in
  let k = if c < 0 || c >= width data then k lor invalid_leftright else k in
  let k = if r < 0 then k lor invalid_up else k in
  let k = if r >= height data then k lor invalid_bottom else k in
  k

let cells_of_bitv data bitv =
  let l = ref [] in
  Bitv.iteri_true (fun bit ->  l := cell_of_bit data bit :: !l)
    bitv;
  !l

let ik = ref valid
(* let move_cell data delta cell = *)
let move data dir conf =
  let delta = Cell.delta_of_move dir in
  ik := valid;
  let old = conf.unit_cells.Piece.content in
  let n = (Array.length old) in
  let unit_cells = Array.make n (0,0) in
  for i = 0 to  n - 1 do
    let cell = old.(i) in
    let newcell =       Cell.(cell + delta) in
    let ik' = check_cell data newcell in
    ik := !ik lor ik';
    unit_cells.(i) <- newcell
  done;
  (* Array.map (move_cell data delta) conf.unit_cells in *)
  let conf =
    { conf with
      unit_cells = Piece.{content = unit_cells; sorted = false};
      unit_pivot = Cell.(delta + conf.unit_pivot);
      commands = dir::conf.commands }
  in
  if unit_overlap data conf then ik := !ik lor invalid_overlap;
  if !ik = valid then conf
  else raise (Invalid_conf !ik)


let move_back data dir conf =
  let delta = Cell.delta_of_move dir in
  ik := valid;
  let unit_cells = Array.map (fun cell ->
      let newcell =  Cell.(cell - delta) in
      let ik' = check_cell data newcell in
      ik := !ik lor ik';
      newcell
    )
      conf.unit_cells.Piece.content in
  let conf =
    { conf with
      unit_cells = Piece.{content = unit_cells; sorted = false};
      unit_pivot = Cell.(conf.unit_pivot - delta);
      commands = [] }
  in
  if unit_overlap data conf then ik := !ik lor invalid_overlap;
  if !ik = valid then conf
  else raise (Invalid_conf !ik)

let rotate_unit data unit pivot dir =
  let ik = ref valid in
  let unit_cells = Array.map (fun cell ->
      let newcell = Cell.(pivot + rotate dir (cell-pivot)) in
      let ik' = check_cell data newcell in
      ik := !ik lor ik';
      newcell)
    unit.Piece.content
  in
  !ik, Piece.{content = unit_cells; sorted = false}

let rotate data dir conf =
  let is_valid, unit_cells = rotate_unit data conf.unit_cells conf.unit_pivot dir in
  let ik = ref is_valid in
  let conf = { conf with unit_cells; commands = dir::conf.commands }
  in
  if unit_overlap data conf then ik := !ik lor invalid_overlap;
  if !ik = valid then conf
  else raise (Invalid_conf !ik)

(* may return invalid positions *)
let rotate_unit data bitv pivot dir =
  let is_valid, cells = rotate_unit data  bitv pivot dir in
  cells

let rotate_back data dir conf =
  let conf = match dir with
    | CW -> rotate data CCW conf
    | CCW -> rotate data CW conf
    | _ -> assert false
  in
  { conf with commands = [] }

let spawn_unit data conf act =
  let rng = Int32.(to_int (logand (shift_right_logical conf.rng_state 16) 0x7FFFl)) in
  let unit_id = rng mod (number_of_units data) in
  let unit_cells, unit_pivot = get_unit data unit_id in
  let conf = { conf with
               unit_cells; unit_pivot;
               rng_state = Int32.(add (mul 1103515245l conf.rng_state) 12345l);
               unit_no = conf.unit_no + 1;
               unit_id;
               commands =
                 match act with
                 | None -> conf.commands
                 | Some act -> act::conf.commands }
  in
  if conf.unit_no = data.input.sourceLength then
    raise (End (conf.score, List.rev conf.commands))
  else
  if unit_overlap data conf
  then raise (End (conf.score, List.rev conf.commands))
  else conf

let lock data conf act =
  let size = Piece.length  conf.unit_cells in
  let full_cells = Bitv.copy conf.full_cells in
  Array.iter (fun cell -> Bitv.set full_cells (bit_of_cell data cell) true) conf.unit_cells.Piece.content;
  let conf = ref {conf with full_cells = full_cells}  in
  let ls = ref 0 in
  for r = 0 to  height data -1 do
    let rec is_full c =
      if c < 0 then true
      else Bitv.get !conf.full_cells (bit_of_coord data (c, r)) && is_full (c-1)
    in
    if is_full (width data-1) then
      begin
        incr ls;
        let full_cells = create_bitv data in
        Bitv.iteri_true (fun bit ->
            let c', r' = coord_of_bit data bit in
            if r' = r then ()
            else if r' < r then Bitv.set full_cells (bit_of_coord data (c', r'+1)) true
            else Bitv.set full_cells (bit_of_coord data (c', r')) true)
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
  spawn_unit data conf act


let init pb ~seed_id  ~time =
  let conf =
    { full_cells = Bitv.create 0 false;
      unit_cells = Piece.empty;
      unit_pivot = Cell.make (0, 0);
      rng_state = Int32.of_int (List.nth pb.sourceSeeds seed_id);
      unit_no = -1;
      unit_id = -1;
      ls_old = 0;
      score = 0;
      commands = [];
      mark = 0}
  in
  let data = build_data  ~time pb in
  let conf = { conf with
               full_cells =
                 Bitv.of_list_with_length
                   (List.map (fun {x;y} -> bit_of_coord data (x,y)) pb.filled)
                   (width data*height data) }
  in
  data, spawn_unit data conf None

let action_of_char = function
  | 'p' | '\''| '!' | '.' | '0' | '3' -> Some ( W)
  | 'b' | 'c' | 'e' | 'f' | 'y' | '2' -> Some ( E)
  | 'a' | 'g' | 'h' | 'i' | 'j' | '4' -> Some ( SW)
  | 'l' | 'm' | 'n' | 'o' | ' ' | '5' -> Some ( SE)
  | 'd' | 'q' | 'r' | 'v' | 'z' | '1' -> Some ( CW)
  | 'k' | 's' | 't' | 'u' | 'w' | 'x' -> Some ( CCW)
  | '\t'| '\n'| '\r' -> None
  | _ -> assert false

let play_action data conf act =
  match act with
  | (E | W | SE | SW) as dir ->
    begin
      try move data dir conf
      with Invalid_conf _ -> lock data conf (Some act)
    end
  | (CW | CCW) as dir ->
    begin
      try rotate data dir conf
      with Invalid_conf _ -> lock data conf (Some act)
    end

let play_str commands data config =
  let conf = ref config in
  String.iter (fun c ->
    match action_of_char c with
      | None -> ()
      | Some act -> conf := play_action data !conf act) commands;
  !conf


let check_game commands pb  seed_id =
  let data, conf = (init pb ~time:1000. ~seed_id) in
  let conf = ref conf in
  let history = HashConfig.create 17 in
  let valid = ref true in
  try
    String.iter (fun c ->
      match action_of_char c with
      | None -> ()
      | Some act ->
        valid := !valid && not (HashConfig.mem history !conf);
        HashConfig.add history !conf ();
        conf := play_action data !conf act) commands;
    !valid
  with End (score, _) -> !valid
