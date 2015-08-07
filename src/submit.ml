open Formats_t

let check input outputs =
  List.for_all
    (fun seed -> List.exists (fun i -> i.seed = seed) outputs)
    input.sourceSeeds
  &&
  List.for_all (fun output -> input.id = output.problemId) outputs
  (* maybe check that there is no cycle ? *)

let make_output_dir id =
  let path = Printf.sprintf "outputs/%i" id in
  match Sys.is_directory path with
    true -> ()
  | false -> invalid_arg @@ Printf.sprintf "%s: %s is not a directory"
      __MODULE__
      path
  | exception _ -> Unix.mkdir path 0o755

let utc_tag () =
  let open Unix in
  let time = gmtime (time ()) in
  let date = Printf.sprintf "%02i-%02i-%02iT:%02i:%02i:%02iZ"
      (time.tm_year + 1900)
      (time.tm_mon + 1)
      time.tm_mday
      time.tm_hour
      time.tm_min
      time.tm_sec
  in
  date


let log input outputs =
  make_output_dir input.id;
  let date = utc_tag () in
  let channel = open_out @@ Printf.sprintf "outputs/%i/%s.json" input.id date in
  let output = Formats_j.string_of_output_l outputs in
  Printf.fprintf channel "%s\n%!" output;
  close_out channel

let publish ~token ~team_id data =
  Curl.global_init  Curl.CURLINIT_GLOBALALL;
  let c = Curl.init () in
  let url = Printf.sprintf "https://davar.icfpcontest.org/teams/%s/solutions" team_id in
  Curl.set_url c url;
  Curl.set_userpwd c token;
  Curl.set_post c true;
  Curl.set_httpheader c  ["Content-Type: application/json"];
  (* Curl.set_postfieldsize c (String.length data); *)
  Curl.set_postfields c data;
  Curl.perform c;
  Curl.cleanup c;
  Curl.global_cleanup ()

let publish input outputs =
  publish Global.token Global.team_id (Formats_j.string_of_output_l outputs)

let main ~submit input outputs =
  log input outputs;
  if submit then publish input outputs else ();
