(* solver's code *)

type options =
  {number: int option;
   memory : int option;
   phrase_of_power : string option}

let solve filename options =
  let lexer = Yojson.init_lexer () in
  let channel = Pervasives.open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let input = Formats_j.read_input lexer lexbuf in
  close_in channel;
  input

let main filenames number memory phrase_of_power =
  let options = {number; memory; phrase_of_power} in
  List.iter (fun f -> ignore (solve f options)) filenames;

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

let main_t =
  Term.(pure main $ filenames $ number $ memory $ phrase_of_power)

let main_info =
  let doc = "Our mighty solver" in
  Term.info "main" ~doc

let () = match Term.eval (main_t, main_info) with `Error _ -> exit 1 | _ -> exit 0
