exception Quit
let interactive config  =

  Printf.printf "Command:\n \
    ESC: quit\n \
                ";

  let state = ref config in
  let continue = ref true in
  let react = function
    | c when int_of_char c = 27 -> continue := false
    | c -> state := Rules.apply c !state;
  in

  while !continue do
    Display.show !state;
    react (Graphics.wait_next_event [Graphics.Key_pressed]).Graphics.key;
  done
