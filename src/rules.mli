type move_dir = E | W | SE | SW
type turn_dir = CW | CCW
type action =
| Turn of turn_dir
| Move of move_dir
| Nop


module Cell :
  sig
    type t = private int * int
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
    include module type of Set.Make(Cell)
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
  ls_old : int
}

val width : config -> int
val height : config -> int

exception Invalid_conf
exception End of int

val move : move_dir -> config -> config
val rotate : turn_dir -> config -> config
val lock : config -> config
val init : Formats_t.input -> int -> config

val action_of_char : char -> action
val play_action : config -> action -> config
val play_game : Formats_t.commands -> Formats_t.input -> int -> int
