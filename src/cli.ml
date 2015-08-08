let () = Sys.catch_break true

(* solver's code *)
type options =
  {filenames : string list;
   number: int option;
   memory : int option;
   phrase_of_power : string option;
   submit: bool}

let open_in filename =
  let lexer = Yojson.init_lexer () in
  let channel = Pervasives.open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let problem = Formats_j.read_input lexer lexbuf in
  close_in channel;
  problem

let make_output problem seed solution tag =
  let open Formats_t in
  {
    problemId = problem.id;
    seed;
    tag;
    solution
  }

let options filenames number memory phrase_of_power submit =
  {filenames; number; memory; phrase_of_power; submit}


(** Interactive *)
let interactive filename options tag prefix : unit =
  let problem = open_in filename in
  let n = (List.length problem.Formats_t.sourceSeeds) in
  let seed =
    if n > 1
    then begin
      Printf.printf "Number of seeds:%i\nSeed number (1 - %i)?" n n;
      int_of_string (read_line ()) - 1
    end
    else 0
  in
  let config = Rules.init problem seed in
  let score, commands = Simulator.interactive ~prefix config in
  let solution = Oracle.empower commands in
  let output = make_output problem seed solution tag in
  let submit = n = 1 && options.submit in
  Submit.main ~score ~submit ~version:"interactive" problem [output]

let interactive ({filenames; number; memory; phrase_of_power} as options) prefix =
  let tag = String.concat " " ["int"; (Submit.utc_tag ()) ]in
  List.iter (fun f -> interactive f options tag prefix) filenames

(** AI  *)

let ai_f filename options tag =
  let problem = open_in filename in
  let solve seed_id seed =
    Printf.printf "Problem %i, seed %i (%i/%i)-- length %i\n%!"
      problem.Formats_t.id
      seed
      seed_id
      (List.length problem.Formats_t.sourceSeeds -1 )
      problem.Formats_t.sourceLength;
    let state = ref (Rules.init problem seed_id) in
    let n = ref 0 in
    try
      while true do
        incr n;
        if !n mod 10 = 0
        then Printf.printf "Turn %i/%i\n%!" !n       problem.Formats_t.sourceLength;
        state := Ia1.play !state
      done;
      assert false
    with Rules.End (score,commands) ->
      Printf.printf "Final score : %d\n" score;
      let solution = Oracle.empower commands in
      let output = make_output problem seed solution tag in
      output, score
  in
  let outputs = List.mapi solve problem.Formats_t.sourceSeeds in

  let score = List.fold_left (fun acc (_,score) -> acc + score) 0 outputs in
  let score = score / (List.length problem.Formats_t.sourceSeeds) in
  let outputs = List.map fst outputs in

  Submit.main ~score ~submit:options.submit ~version:Ia1.version problem outputs

let ai ({filenames; number; memory; phrase_of_power} as options) =
  let tag = String.concat " " ["main"; (Submit.utc_tag ()) ]in
  let rec main file =
    try ai_f file options tag
    with Sys.Break ->
      if !Ia1.max_depth > 0
      then (decr Ia1.max_depth; main file)
      else (Printf.printf "Cannot decrease max_depth, skipping...\n%!")
  in
  List.iter main filenames

let nuke options =
  let tag = Submit.utc_tag () in
  let filenames = Sys.readdir "problems" |> Array.to_list |> List.sort Pervasives.compare in
  let filenames = List.map (fun s -> "problems/" ^ s) filenames in
  let default_max_depth = !Ia1.max_depth in
  let rec main file =
    try ai_f file options tag
    with Sys.Break ->
      if !Ia1.max_depth > 0
      then (decr Ia1.max_depth; main file)
      else (Printf.printf "Cannot decrease max_depth, skipping...\n%!")
  in
  List.iter (fun file -> Ia1.max_depth := default_max_depth; main file) filenames

let score () =
  Printf.printf "Score board\n%s\n" @@ Scoreboard.display (Scoreboard.read ())

(* Cmdliner code *)
open Cmdliner

let filenames =
  let doc = "File containing JSON encoded input." in
  Arg.(value & opt_all (file) [] & info ["f"]  ~doc)

let number =
  let doc = "Time limit, in seconds, to produce output." in
  Arg.(value & opt (some int) None & info ["t"] ~doc)

let memory =
  let doc = "Memory limit, in megabytes, to produce output." in
  Arg.(value & opt (some int) None & info ["m"] ~doc)

let phrase_of_power =
  let doc = "Phrase of power, as quoted string." in
  Arg.(value & opt (some string) None & info ["p"] ~doc)

let submit =
  let doc = "Submit the output to the scoring server" in
  Arg.(value & flag & info ["score"] ~doc)

let prefix =
  let doc = "String to be played interactively" in
  Arg.(value & opt (string) "" & info ["prefix"] ~doc)

let options_t =
  Term.(pure options $ filenames $ number $ memory $ phrase_of_power $ submit)

(* Interavtive mode *)
let interactive_t =
  Term.(pure interactive $ options_t $ prefix)

let interactive_info =
  let doc = "Our mighty interactive solver" in
  Term.info "int" ~doc

(* AI mode *)
let ai_t =
  Term.(pure ai $ options_t)

let ai_info =
  let doc = "Our mighty automated solver" in
  Term.info "ai" ~doc

(* Nuke mode *)
let nuke_t =
  Term.(pure nuke $ options_t)

let nuke_info =
  let doc = "Our mighty automated solver (nuke all problems)" in
  Term.info "nuke" ~doc

(* Show score *)
let show_t =
  Term.(pure score $ pure ())

let show_info =
  let doc = "Our score" in
  Term.info "score" ~doc

let commands =
  [
    interactive_t, interactive_info;
    ai_t, ai_info;
    nuke_t, nuke_info;
    show_t, show_info
  ]

let default = interactive_t, interactive_info

let () = match Term.eval_choice default  commands with `Error _ -> exit 1 | _ -> exit 0
