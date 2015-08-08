type event = Formats_t.event
type scoreboard = Formats_t.scoreboard

open Formats_t
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

let best_score scoreboard (problem: Formats_t.input) =
  List.fold_left (fun acc (id,{score;submitted}) ->
      if problem.id = id  && submitted
      then max score acc
      else acc
    ) 0 scoreboard

let tabulate (scoreboard: scoreboard) : (int * int * string) list =
  let h = Hashtbl.create 17 in
  let add h key element =
    let old =
      if Hashtbl.mem h key
      then Hashtbl.find h key
      else []
    in Hashtbl.replace h key (element::old)
  in
  List.iter (fun (id,event) -> add h id event) scoreboard;
  Hashtbl.fold (fun id scores acc ->
      let open Formats_t in
      let scores = List.map (fun({score;timestamp}) -> score,timestamp) scores in
      let score,tag = List.fold_left (fun (score_acc, tag_acc) (score,tag) ->
          if score_acc < score
          then (score,tag)
          else (score_acc, tag_acc)
        ) (0,"") scores  in
      (id,score,tag)::acc) h []
;;

let display scoreboard =
  let table = tabulate scoreboard in
  let table = List.sort (fun (a,_,_) (b,_,_) -> Pervasives.compare  a b) table in
  let lines : string list = List.map (fun (id,score,tag) ->
      Printf.sprintf "|%6i: %6i        %s|" id score tag) table in
  let max_length = List.fold_left (fun acc x -> max (String.length x) acc) 0 lines in
  let line = String.make max_length '-' in
  String.concat "\n" (line::lines@[line])
