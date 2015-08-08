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
  score : int;
  ls_old : int;
  commands : action list ;      (* in reverse order *)
}

type data

val width : data -> int
val height : data -> int
val bit_of_coord: data -> int*int -> int
val bit_of_cell: data -> Cell.t -> int
val coord_of_bit: data -> int -> int*int
val cell_of_bit: data -> int -> Cell.t
val create_bitv: data -> Bitv.t


(* Only full_cells, unit_cells, unit_pivot, unit_no *)
module HashableConfig : Hashtbl.HashedType with type t = config
module HashConfig : Hashtbl.S with type key = config


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
val move : data -> move_dir -> config -> config
val rotate : data -> turn_dir -> config -> config
val check_cell : data -> Cell.t -> int

(** {2 Main functions }  *)

(** Play an action to update a config *)
val play_action : data -> config -> action -> config

(** Play a complete game  *)
val play_game : Formats_t.commands -> Formats_t.input -> int -> int

(** Check a complete game *)
val check_game : Formats_t.commands -> Formats_t.input -> int -> bool

(** Initialize the game  *)
val init : Formats_t.input -> seed_id:int -> data * config
