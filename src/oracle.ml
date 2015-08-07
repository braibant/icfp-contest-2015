
let tokens =
  let open Rules in
  function
  | Move W   -> [ 'p' ;'\'';'!' ;'.' ;'0' ;'3' ]
  | Move E   -> [ 'b' ;'c' ;'e' ;'f' ;'y' ;'2' ]
  | Move SW  -> [ 'a' ;'g' ;'h' ;'i' ;'j' ;'4' ]
  | Move SE  -> [ 'l' ;'m' ;'n' ;'o' ;' ' ;'5' ]
  | Turn CW  -> [ 'd' ;'q' ;'r' ;'v' ;'z' ;'1' ]
  | Turn CCW -> [ 'k' ;'s' ;'t' ;'u' ;'w' ;'x' ]
  | Nop -> ['\t'; '\n'; '\r']

let empower actions =
  let n = List.length actions in
  let a = Array.of_list actions in
  String.init n (fun i -> List.hd (tokens a.(i)))
