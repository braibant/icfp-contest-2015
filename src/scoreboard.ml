type event = Formats_t.event
type scoreboard = Formats_t.scoreboard

let read () =
  try
    let channel = open_in "outputs/scoreboard" in
    let lexer = Yojson.init_lexer () in
    let lexbuf = Lexing.from_channel channel in
    let scoreboard = Formats_j.read_scoreboard lexer lexbuf in
    close_in channel;
    scoreboard
  with _ -> []

let write scoreboard =
  let channel = open_out "outputs/scoreboard" in
  let s = Formats_j.string_of_scoreboard scoreboard in
  Printf.fprintf channel "%s%!" s;
  close_out channel
