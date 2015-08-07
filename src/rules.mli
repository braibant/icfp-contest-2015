type move_dir = E | W | SE | SW
type turn_dir = CW | CCW
type action =
| Turn of turn_dir
| Move of move_dir
| Nop

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

module CSet :
sig
  include module type of Set.Make(Cell) with type elt = Cell.t
  val map : (elt -> elt option) -> t -> t
end

type config = private {
  full_cells : CSet.t;
  unit_cells : CSet.t;
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
val action_of_char : char -> action

exception Invalid_conf
exception End of int

(** {2 Internal functions, do not update the commands field}  *)
val move : move_dir -> config -> config
val rotate : turn_dir -> config -> config
val lock : config -> config

(** {2 Main functions }  *)

(** Play an action to update a config *)
val play_action : config -> action -> config

(** Play a complete game  *)
val play_game : Formats_t.commands -> Formats_t.input -> int -> int

(** Initialize the game  *)
val init : Formats_t.input -> int -> config
