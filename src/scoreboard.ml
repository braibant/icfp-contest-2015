type event = Formats_t.event
type scoreboard = Formats_t.scoreboard

let read () =
  let channel = open_in "outputs/scoreboard" in
  let lexer = Yojson.init_lexer () in
  let lexbuf = Lexing.from_channel channel in
  let scoreboard =
    try Formats_j.read_scoreboard lexer lexbuf
    with _ -> []
  in
  close_in channel;
  scoreboard

let write scoreboard =
  let channel = open_out "outputs/scoreboard" in
  let s = Formats_j.string_of_scoreboard scoreboard in
  Printf.fprintf channel "%s%!" s;
  close_out channel

let map f = write (f (read ()))
