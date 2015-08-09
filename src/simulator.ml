exception Quit

type mode = | Power        (* feed the letters directly *)
            | Sane         (* keep our sanity  *)

let mysleep n =
  let start = Unix.gettimeofday() in
  let rec delay t =
    try
      ignore (Unix.select [] [] [] t)
    with Unix.Unix_error(Unix.EINTR, _, _) ->
      let now = Unix.gettimeofday() in
      let remaining = start +. n -. now in
      if remaining > 0.0 then delay remaining in
  delay n

let interactive ~prefix data config  =
  Display.init data config;
  Printf.printf "Command:\n \
                 ESC: quit\n \
                 +/-: resize the tiles\n \
                 v: switch to sane mode\n \
                 p: switch to power mode\n \
                 q: end of game\n \
                 %!";

  let state = ref config in
  let continue = ref true in
  let mode = ref Sane in

  let react c =
    (* Printf.printf "react %c\n%!" c; *)
    match c with
    | c when int_of_char c = 27 -> continue := false
    | 'p' -> mode := Power
    | 'v' -> mode := Sane
    | '+' ->
      let s = Display.size () in
      Display.resize (s + 1);
      Display.show data !state
    | '-' ->
      let s = Display.size () in
      Display.resize (s - 1);
      Display.show data !state
    | c when !mode = Power->
      begin match Rules.action_of_char c with
      | None -> ()
      | Some act -> state := Rules.play_action data !state act
      end
    | 'j' -> state := Rules.play_action  data !state Rules.( W)
    | 'l' -> state := Rules.play_action  data !state Rules.( E)
    | 'i' -> state := Rules.play_action  data !state Rules.( CW)
    | 'k' -> state := Rules.play_action  data !state Rules.( CCW)
    | 'u' -> state := Rules.play_action  data !state Rules.( SW)
    | 'o' -> state := Rules.play_action  data !state Rules.( SE)
    | 'a' -> state := Ia1.play  data !state
    | 'q' -> raise (Rules.End (!state.Rules.score, List.rev (!state.Rules.commands)))
    | _ -> ()
  in
  String.iter
    (fun c ->
      (* Printf.printf "step: %c\n%!" c; *)
      Display.show data !state;
      begin match Rules.action_of_char c with
      | None -> ()
      | Some act -> state := Rules.play_action data !state act
      end;
      ignore (Graphics.wait_next_event [Graphics.Key_pressed]).Graphics.key;
      (* mysleep 0.5; *)
    ) prefix;
  try while !continue do
      Display.show data !state;
      react (Graphics.wait_next_event [Graphics.Key_pressed]).Graphics.key;
    done;
    Printf.printf "Interrupted\n%!"; raise Exit
  with
    Rules.End (score,commands) ->
    Printf.printf "Final score : %d\n" score;
    score, commands
