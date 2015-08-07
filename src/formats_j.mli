(* Auto-generated from "formats.atd" *)


type cell = Formats_t.cell = { x: int; y: int }

type unit_t = Formats_t.unit_t = { members: cell list; pivot: cell }

type commands = Formats_t.commands

type output = Formats_t.output = {
  problemId: int;
  seed: int;
  tag: int;
  solution: commands
}

type input = Formats_t.input = {
  id: int;
  units: unit_t list;
  width: int;
  height: int;
  filled: cell list;
  sourceLength: int;
  sourceSeeds: int list
}

val write_cell :
  Bi_outbuf.t -> cell -> unit
  (** Output a JSON value of type {!cell}. *)

val string_of_cell :
  ?len:int -> cell -> string
  (** Serialize a value of type {!cell}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_cell :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> cell
  (** Input JSON data of type {!cell}. *)

val cell_of_string :
  string -> cell
  (** Deserialize JSON data of type {!cell}. *)

val write_unit_t :
  Bi_outbuf.t -> unit_t -> unit
  (** Output a JSON value of type {!unit_t}. *)

val string_of_unit_t :
  ?len:int -> unit_t -> string
  (** Serialize a value of type {!unit_t}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_unit_t :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> unit_t
  (** Input JSON data of type {!unit_t}. *)

val unit_t_of_string :
  string -> unit_t
  (** Deserialize JSON data of type {!unit_t}. *)

val write_commands :
  Bi_outbuf.t -> commands -> unit
  (** Output a JSON value of type {!commands}. *)

val string_of_commands :
  ?len:int -> commands -> string
  (** Serialize a value of type {!commands}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_commands :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> commands
  (** Input JSON data of type {!commands}. *)

val commands_of_string :
  string -> commands
  (** Deserialize JSON data of type {!commands}. *)

val write_output :
  Bi_outbuf.t -> output -> unit
  (** Output a JSON value of type {!output}. *)

val string_of_output :
  ?len:int -> output -> string
  (** Serialize a value of type {!output}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_output :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> output
  (** Input JSON data of type {!output}. *)

val output_of_string :
  string -> output
  (** Deserialize JSON data of type {!output}. *)

val write_input :
  Bi_outbuf.t -> input -> unit
  (** Output a JSON value of type {!input}. *)

val string_of_input :
  ?len:int -> input -> string
  (** Serialize a value of type {!input}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_input :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> input
  (** Input JSON data of type {!input}. *)

val input_of_string :
  string -> input
  (** Deserialize JSON data of type {!input}. *)

