exception Quit

type mode = | Power        (* feed the letters directly *)
            | Sane         (* keep our sanity  *)

let interactive config  =
  Display.init config;
  Printf.printf "Command:\n \
                 ESC: quit\n \
                 +/-: resize the tiles\n \
                 v: switch to sane mode\n \
                 p: switch to power mode\n \
                 %!";

  let state = ref config in
  let continue = ref true in
  let mode = ref Sane in
  let react = function
    | c when int_of_char c = 27 -> continue := false
    | 'p' -> mode := Power
    | 'v' -> mode := Sane
    | '+' ->
      let s = Display.size () in
      Display.resize (s + 1);
      Display.show !state
    | '-' ->
      let s = Display.size () in
      Display.resize (s - 1);
      Display.show !state
    | c when !mode = Power->
      state := Rules.play_action !state (Rules.action_of_char c);
    | 'j' -> state := Rules.play_action !state Rules.(Move W)
    | 'l' -> state := Rules.play_action !state Rules.(Move E)
    | 'i' -> state := Rules.play_action !state Rules.(Turn CW)
    | 'k' -> state := Rules.play_action !state Rules.(Turn CCW)
    | 'u' -> state := Rules.play_action !state Rules.(Move SW)
    | 'o' -> state := Rules.play_action !state Rules.(Move SE)
    | 'a' -> state := Ia1.play !state
    | _ -> ()
  in

  try while !continue do
      Display.show !state;
      react (Graphics.wait_next_event [Graphics.Key_pressed]).Graphics.key;
    done;
    Printf.printf "Interrupted\n%!"; raise Exit
  with
    Rules.End (score,commands) ->
    Printf.printf "Final score : %d\n" score;
    score, commands
