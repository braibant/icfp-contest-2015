type move_dir = E | W | SE | SW
type turn_dir = CW | CCW
module Cell :
  sig
    type t = int * int
    val compare : 'a -> 'a -> int
    val of_coord : int * int -> int * int
    val to_coord : int * int -> int * int
    val ( + ) : int * int -> int * int -> int * int
    val ( - ) : int * int -> int * int -> int * int
    val ( ~- ) : int * int -> int * int
    val rotate : turn_dir -> int * int -> int * int
    val delta_of_move : move_dir -> int * int
  end

module CSet :
  sig
    include module type of Set.Make(Cell)
    val map : (elt -> elt option) -> t -> t
  end
type config = {
  full_cells : CSet.t;
  unit_cells : CSet.t;
  unit_pivot : Cell.t;
  rng_state : Int32.t;
  unit_no : int;
  problem : Formats_t.input;
}
exception Invalid_conf
val check_unit_bounds : config -> unit
val move : move_dir -> config -> config
val rotate : turn_dir -> config -> config
