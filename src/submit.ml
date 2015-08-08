open Formats_t

let check input outputs =
  [
    "All source seeds appear in outputs",
    List.for_all
      (fun seed -> List.exists (fun i -> i.seed = seed) outputs)
      input.sourceSeeds;

    "All outputs have the right problem id",
    List.for_all (fun output -> input.id = output.problemId) outputs;

    "Outputs are not circular",
    begin let seed_id = ref (-1) in
    List.for_all (fun output ->
        incr seed_id;
        Rules.check_game output.solution input !seed_id
      ) outputs end;

    "Outputs have the correct seed (output[i].seed = seeds.[i])",
    begin let seed_id = ref (-1) in
    List.for_all (fun output ->
        incr seed_id;
        (List.nth input.sourceSeeds !seed_id = output.seed)
      ) outputs end;
  ]

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


let log ~filename input outputs =
  make_output_dir input.id;
  let channel = open_out @@ Printf.sprintf "outputs/%i/%s.json" input.id filename in
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

let best_score scoreboard problem =
  List.fold_left (fun acc {id;score} ->
      if problem.id = id
      then max score acc
      else acc
    ) min_int scoreboard

let main ~submit ~score input outputs  =
  let checks = check input outputs in
  let descr = List.map (fun (s,b) ->
      if b
      then Printf.sprintf "  %s: OK" s
      else Printf.sprintf "  %s: FAIL" s
    ) checks |> String.concat "\n"
  in

  if List.for_all (snd) checks
  then Printf.printf "Check OK!\n"
  else Printf.printf "Check failed!\n%s\n" descr;

  let timestamp =  utc_tag () in

  log timestamp input outputs;

  let scoreboard = Scoreboard.read () in
  let event = Formats_t.{
      id = input.id;
      timestamp;
      outputs;
      submitted = submit;
      score;
    } in
  Scoreboard.write (event::scoreboard);

  if submit && best_score scoreboard input < event.score
  then publish input outputs else ();
