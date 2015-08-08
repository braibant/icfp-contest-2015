open Rules

type full_conf =
| Cont of config
| End of int

let find_reachable_states_mem =
  Rules.HashConfig.create 17
let find_reachable_states init =
  try Rules.HashConfig.find find_reachable_states_mem init
  with Not_found ->
    let seen = Rules.HashConfig.create 17 in
    let todo = Queue.create () in
    Queue.push (init, []) todo;
    let ends = ref [] in
    while not (Queue.is_empty todo) do
      let (conf, path) = Queue.pop todo in
      if Rules.HashConfig.mem seen conf then ()
      else
        begin
          Rules.HashConfig.add seen conf ();
          let lock_action = ref None in
          let insert_action act =
            try
              let node =
                match act with
                | Turn dir -> (rotate dir conf, act::path)
                | Move dir -> (move dir conf, act::path)
              in
              Queue.push node todo
            with Invalid_conf ik ->
              if ik land (invalid_overlap lor invalid_bottom) <> 0 then
                lock_action := Some act
          in
          insert_action (Move E);
          insert_action (Move W);
          insert_action (Move SE);
          insert_action (Move SW);
          insert_action (Turn CW);
          insert_action (Turn CCW);
          begin match !lock_action with
          | None -> ()
          | Some act ->
            let conf =
              try Cont (play_action conf act)
              with Rules.End (score, _path) -> End (score)
            in
            ends := (conf, List.rev (act::path))::!ends end
        end
    done;
    assert (!ends <> []);
    Rules.HashConfig.replace find_reachable_states_mem init !ends;
    !ends

let heuristic_score conf =
  match conf with
  | End sc -> (sc-10000)*10000
  | Cont conf ->
    let sc = ref (conf.score*10000) in
    Bitv.iteri_true (fun bit ->
      sc := !sc + snd (Rules.coord_of_bit conf bit))
      conf.full_cells;
    !sc

exception TooMuchColl
let best_heuristic_score_hsize = 10007
let best_heuristic_score_mem = Array.make best_heuristic_score_hsize None
let n_coll = ref 0
let rec best_heuristic_score conf = function
  | 0 -> heuristic_score conf
  | depth ->
    match conf with
    | End _ -> heuristic_score conf
    | Cont conf ->
      let h = Rules.HashableConfig.hash conf mod best_heuristic_score_hsize in
      match best_heuristic_score_mem.(h) with
      | Some (conf', sc) when Rules.HashableConfig.equal conf conf' -> sc
      | bucket ->
        if bucket <> None then
          begin
            incr n_coll;
            if !n_coll > 100 then raise TooMuchColl
          end;
        let next = find_reachable_states conf in
        let res =
          List.fold_left (fun acc (conf, _) ->
            let score = best_heuristic_score conf (depth-1) in
            max acc score) min_int next
        in
        best_heuristic_score_mem.(h) <- Some (conf, res);
        res

let rec play conf =
  HashConfig.clear find_reachable_states_mem;
  let next = find_reachable_states conf in
  let conf_end = ref conf in
  begin try
    for depth = 0 to 10 do
      Array.fill best_heuristic_score_mem 0 best_heuristic_score_hsize None;
      let (_, path) =
        List.fold_left (fun ((scoremax, pathmax) as acc) (conf,path) ->
          let score = best_heuristic_score conf depth in
          if score <= scoremax then acc
          else (score, path)
        ) (min_int, []) next
      in
      conf_end := List.fold_left play_action conf path
    done
  with TooMuchColl -> () end;
  !conf_end
