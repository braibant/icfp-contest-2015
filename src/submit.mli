val log: filename:string -> Formats_t.input -> Formats_t.output_l -> unit
val publish : Formats_t.input -> Formats_t.output_l -> unit

val main : submit:bool -> score:int -> version:string -> Formats_t.input -> Formats_t.output_l ->  unit
val utc_tag : unit -> string
