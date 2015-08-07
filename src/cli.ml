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
    seed = List.nth problem.sourceSeeds seed;
    tag;
    solution
  }

let options filenames number memory phrase_of_power submit =
  {filenames; number; memory; phrase_of_power; submit}


(** Interactive *)
let interactive filename options tag =
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
  let score, commands = Simulator.interactive config in
  let solution = Oracle.empower commands in
  let output = make_output problem seed solution tag in
  Submit.main ~submit:options.submit problem [output]

let interactive ({filenames; number; memory; phrase_of_power} as options)  =
  let tag = String.concat " " ["int"; (Submit.utc_tag ()) ]in
  List.iter (fun f -> ignore (interactive f options tag)) filenames

(** AI  *)

let ai filename options tag =
  let problem = open_in filename in
  let solve seed =
    Printf.printf "Problem %i, seed %i/%i -- length %i\n%!"
      problem.Formats_t.id
      (seed + 1)
      (List.length problem.Formats_t.sourceSeeds)
      problem.Formats_t.sourceLength;
    let state = ref (Rules.init problem seed) in
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
      make_output problem seed solution tag
  in
  let outputs = List.map solve problem.Formats_t.sourceSeeds in
  Submit.main ~submit:options.submit problem outputs

let ai ({filenames; number; memory; phrase_of_power} as options) =
  let tag = String.concat " " ["main"; (Submit.utc_tag ()) ]in
  List.iter (fun f -> ignore (ai f options tag)) filenames

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

let options_t =
  Term.(pure options $ filenames $ number $ memory $ phrase_of_power $ submit)

(* Interavtive mode *)
let interactive_t =
  Term.(pure interactive $ options_t)

let interactive_info =
  let doc = "Our mighty interactive solver" in
  Term.info "int" ~doc

(* AI mode *)
let ai_t =
  Term.(pure ai $ options_t)

let ai_info =
  let doc = "Our mighty automated solver" in
  Term.info "ai" ~doc

let commands =
  [
    interactive_t, interactive_info;
    ai_t, ai_info
  ]

let default = interactive_t, interactive_info

let () = match Term.eval_choice default  commands with `Error _ -> exit 1 | _ -> exit 0
