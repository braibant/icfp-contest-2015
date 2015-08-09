open Rules

let find_reaching_states data dst =
  let seen = HashConfig.create 17 in
  let todo = Queue.create () in
  Queue.push dst todo;
  while not (Queue.is_empty todo) do
    let conf = Queue.pop todo in
    if HashConfig.mem seen conf then ()
    else
      begin
        HashConfig.add seen conf ();
        let insert_action act =
          try
            let node =
              match act with
              | Turn dir -> rotate_back data dir conf
              | Move dir -> move_back data dir conf
            in
            Queue.push node todo
          with Invalid_conf _ -> ()
        in
        insert_action (Move E);
        insert_action (Move W);
        insert_action (Move SE);
        insert_action (Move SW);
        insert_action (Turn CW);
        insert_action (Turn CCW)
      end
  done;
  seen

(* (\* bonus_pwph, pwdh : increasing size order *\) *)
(* let rec find_powerpath data src dst reaching forbidden_conf bonus_pwph pwph = *)
(*   if not (HashConfig.mem src reaching) then None *)
(*   else *)
(*     begin *)
(*       let rollback_forbidden_conf = ref (fun () -> ()) in *)
(*       let try_actions acts = *)
(*         try *)
(*           let conf = *)
(*             List.fold_left (fun conf act -> *)
(*               let conf = *)
(*                 match act with *)
(*                 | Move dir -> move data dir conf *)
(*                 | Turn dir -> rotate data dir conf *)
(*               in *)
(*               if HashConfig.mem forbidden_conf conf then *)
(*                 raise (Invalid_conf 0) *)
(*               else *)
(*                 begin *)
(*                   HashConfig.add forbidden_conf conf (); *)
(*                   rollback_forbidden_conf := *)
(*                     (let rb = !rollback_forbidden_conf in *)
(*                      fun () -> HashConfig.remove forbidden_conf conf; rb ()) *)
(*                 end; *)
(*               conf) *)
(*               src acts *)
(*           in *)
          

(*     end *)

let tokens =
  let open Rules in
  function
  | Move W   -> [ 'p' ;'\'';'!' ;'.' ;'0' ;'3' ]
  | Move E   -> [ 'b' ;'c' ;'e' ;'f' ;'y' ;'2' ]
  | Move SW  -> [ 'a' ;'g' ;'h' ;'i' ;'j' ;'4' ]
  | Move SE  -> [ 'l' ;'m' ;'n' ;'o' ;' ' ;'5' ]
  | Turn CW  -> [ 'd' ;'q' ;'r' ;'v' ;'z' ;'1' ]
  | Turn CCW -> [ 'k' ;'s' ;'t' ;'u' ;'w' ;'x' ]

let empower actions prefix =
  let n = List.length actions in
  let a = Array.of_list actions in
  String.init n (fun i ->
    let tok = tokens a.(i) in
    if i < String.length prefix && List.mem prefix.[i] tok
    then prefix.[i]
    else List.hd (tokens a.(i)))
