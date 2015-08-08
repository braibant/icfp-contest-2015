type move_dir = E | W | SE | SW
type turn_dir = CW | CCW
type action =
| Turn of turn_dir
| Move of move_dir

module Cell :
  sig
    type t =  int * int
    val compare : t -> t -> int
    val of_coord : int * int -> t
    val to_coord : t -> int * int
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( ~- ) : t -> t
    val rotate : turn_dir -> t -> t
    val delta_of_move : move_dir -> t
  end

type config = private {
  full_cells : Bitv.t;
  unit_cells : Bitv.t;
  unit_pivot : Cell.t;
  rng_state : Int32.t;
  unit_no : int;
  problem : Formats_t.input;

  score : int;
  ls_old : int;
  commands : action list ;      (* in reverse order *)
}

val width : config -> int
val height : config -> int

(* Only full_cells, unit_cells, unit_pivot, unit_no *)
module HashableConfig : Hashtbl.HashedType with type t = config
module HashConfig : Hashtbl.S with type key = config

val bit_of_coord: config -> int*int -> int
val bit_of_cell: config -> Cell.t -> int
val coord_of_bit: config -> int -> int*int
val cell_of_bit: config -> int -> Cell.t

val action_of_char : char -> action option

type invalid_kind = int
val invalid_overlap : invalid_kind
val invalid_bottom : invalid_kind
val invalid_leftright : invalid_kind
val invalid_up : invalid_kind
val valid : invalid_kind
exception Invalid_conf of invalid_kind
exception End of int * action list

(** {2 Internal functions}  *)
val move : move_dir -> config -> config
val rotate : turn_dir -> config -> config

(** {2 Main functions }  *)

(** Play an action to update a config *)
val play_action : config -> action -> config

(** Play a complete game  *)
val play_game : Formats_t.commands -> Formats_t.input -> int -> int

(** Check a complete game *)
val check_game : Formats_t.commands -> Formats_t.input -> int -> bool

(** Initialize the game  *)
val init : Formats_t.input -> seed_id:int -> config
