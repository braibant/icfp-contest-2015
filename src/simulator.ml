exception Quit

let interactive config  =
  Display.init config;
  Printf.printf "Command:\n \
    ESC: quit\n \
    +/-: resize the tiles\n \
                %!";

  let state = ref config in
  let continue = ref true in
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
    | c -> state := Rules.play_action !state (Rules.action_of_char c);

  in

  while !continue do
    Display.show !state;
    react (Graphics.wait_next_event [Graphics.Key_pressed]).Graphics.key;
  done
