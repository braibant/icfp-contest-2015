open Rules

let tokens = function
  | Move W   -> [ 'p' ;'\'';'!' ;'.' ;'0' ;'3' ]
  | Move E   -> [ 'b' ;'c' ;'e' ;'f' ;'y' ;'2' ]
  | Move SW  -> [ 'a' ;'g' ;'h' ;'i' ;'j' ;'4' ]
  | Move SE  -> [ 'l' ;'m' ;'n' ;'o' ;' ' ;'5' ]
  | Turn CW  -> [ 'd' ;'q' ;'r' ;'v' ;'z' ;'1' ]
  | Turn CCW -> [ 'k' ;'s' ;'t' ;'u' ;'w' ;'x' ]

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

let rec sort_by_prio l = List.sort (fun (_, _, a) (_, _, b) -> b-a) l

let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let drop_last s = String.sub s 0 (String.length s-1)

exception Found of
    string list *
      (Rules.action list * string * int) list *
      (Rules.action list * string * int) list

(* Return *reversed* list of strings. *)
let rec find_powerpath data conf dst reaching forbidden_conf bonus_pwph pwph simple_acts =
  if not (HashConfig.mem reaching conf) then None
  else
    begin
      let rec rollback_forbidden_conf = ref rollback_forbidden_conf_init
      and rollback_forbidden_conf_init () =
        rollback_forbidden_conf := rollback_forbidden_conf_init
      in
      let try_actions acts actsstr bonus_pwph pwph =
        let rec play_acts conf = function
          | [] -> Some conf
          | act::qact ->
            match
              match act with
              | Move dir -> move data dir conf
              | Turn dir -> rotate data dir conf
            with
            | exception (Invalid_conf _ as e) ->
              if qact = [] && HashableConfig.equal conf dst then None
              else raise e
            | conf ->
              if HashConfig.mem forbidden_conf conf then raise (Invalid_conf 0)
              else
                begin
                  HashConfig.add forbidden_conf conf ();
                  rollback_forbidden_conf :=
                    (let rb = !rollback_forbidden_conf in
                     fun () -> HashConfig.remove forbidden_conf conf; rb ());
                  play_acts conf qact
                end;
        in
        (* let conf0 = conf in *)
        match play_acts conf acts with
        | None ->
          !rollback_forbidden_conf ();
          (* let conf = play_str (drop_last actsstr) data conf in *)
          (* assert (HashableConfig.equal conf dst); *)
          raise (Found ([actsstr], bonus_pwph, pwph))
        | Some conf ->
          (* let conf' = play_str actsstr data conf0 in *)
          (* assert (HashableConfig.equal conf' conf); *)
          let res =
            find_powerpath data conf dst reaching forbidden_conf
              bonus_pwph pwph simple_acts
          in
          !rollback_forbidden_conf ();
          begin match res with
          | None -> ()
          | Some (acts, bonus_pwph, pwph) ->
            (* let s = String.concat "" (actsstr::acts) in *)
            (* let conf = play_str (drop_last s) data conf0 in *)
            (* assert (HashableConfig.equal conf dst); *)

            raise (Found (actsstr::acts, bonus_pwph, pwph))
          end
        | exception (Invalid_conf _) ->
          !rollback_forbidden_conf ()
      in
      try
        let rec try_bonus_pwph seen_l = function
          | [] -> ()
          | (acts, actsstr, _prio) as t::q ->
            try_actions acts actsstr (List.rev_append seen_l q) (sort_by_prio (t::pwph));
            try_bonus_pwph (t::seen_l) q
        in
        try_bonus_pwph [] bonus_pwph;
        List.iter (fun (acts, actsstr, _prio) ->
          try_actions acts actsstr bonus_pwph pwph)
          pwph;
        List.iter (fun (acts, actsstr) -> try_actions [acts] actsstr bonus_pwph pwph)
          (shuffle simple_acts);
        None
      with Found (acts, bonus_pwph, pwph) -> Some (acts, bonus_pwph, pwph)
    end

let rec find_powerpath_game data src acts acts_accu bonus_pwph pwph simple_acts =
  let next, acts_next =
    let rec aux conf = function
      | [] -> assert false
      | t::q ->
        match
          match t with
          | Move dir -> move data dir conf
          | Turn dir -> rotate data dir conf
        with
        | exception (Invalid_conf _) -> conf, t::q
        | conf -> aux conf q
    in
    aux src acts
  in
  let forbidden_conf = HashConfig.create 17 in
  HashConfig.add forbidden_conf src ();
  match
    find_powerpath data src next (find_reaching_states data next) forbidden_conf
      bonus_pwph pwph simple_acts
  with None -> assert false
  | Some (acts, bonus_pwph, pwph) ->
    let actsstr = String.concat "" acts in
    let conf = play_str (drop_last actsstr) data src in
    assert (HashableConfig.equal conf next);
    begin
      try match action_of_char (actsstr.[String.length actsstr-1]) with
      | Some (Move dir) -> ignore (move data dir next); assert false
      | Some (Turn dir) -> ignore (rotate data dir next); assert false
      | None -> assert false
      with Invalid_conf _ -> ()
    end;
    let acts_accu = List.rev_append acts acts_accu in
    match acts_next with
    | [] -> assert false
    | t::acts_next ->
      match lock data next (Some t) with
      | next ->
        find_powerpath_game data next acts_next acts_accu bonus_pwph pwph simple_acts
      | exception (End _) -> String.concat "" (List.rev acts_accu)

let empower_prefix actions prefix =
  let n = List.length actions in
  let a = Array.of_list actions in
  String.init n (fun i ->
    let tok = tokens a.(i) in
    if i < String.length prefix && List.mem prefix.[i] tok
    then prefix.[i]
    else List.hd (tokens a.(i)))

let gen_pw pw prio =
  let l = ref [] in
  for i = 0 to String.length pw - 1 do
    match Rules.action_of_char pw.[i] with
    | None -> ()
    | Some a -> l := a::!l
  done;
  (List.rev !l, pw, prio)

let empower_power actions data conf =
  Random.init 18;
  let pw =
    List.map (fun (pw, prio) -> gen_pw pw prio)
      ["ia! ia! ", 20;
       "ei!", 10; "ia! ia!", 8; "r'lyeh", 7;
       "yuggoth", 6; "yogsothoth", 4; "hastur", 9]
  in
  let simple_act =
    [Move W,"p"; Move E,"b"; Turn CW,"d"; Turn CCW,"k"; Move SW,"a"; Move SE,"l"]
  in
  find_powerpath_game data conf actions []
    pw [] simple_act
