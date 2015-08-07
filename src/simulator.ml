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
    | _ -> ()

  in

  while !continue do
    Display.show !state;
    try
      react (Graphics.wait_next_event [Graphics.Key_pressed]).Graphics.key;
    with
      Rules.End _ -> continue := false
  done;
  let score = !state.Rules.score in
  let commands = List.rev ((!state).Rules.commands) in
  score, commands
