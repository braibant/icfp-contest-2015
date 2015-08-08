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
    let next = Rules.HashConfig.create 17 in
    let best_ends = ref (-1, []) in
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
            try
              Rules.HashConfig.replace next (play_action conf act)
                (List.rev (act::path))
            with Rules.End (score, _path) ->
              if score > fst !best_ends then
                best_ends := (score, List.rev (act::path))
          end
        end
    done;
    let next = Rules.HashConfig.fold (fun conf path acc -> (Cont conf,path)::acc) next [] in
    let next =
      if fst !best_ends >= 0 then (End (fst !best_ends), snd !best_ends)::next else next
    in
    assert (next <> []);
    Rules.HashConfig.replace find_reachable_states_mem init next;
    next

let heuristic_score conf =
  match conf with
  | End sc -> (sc-10000)*10000
  | Cont conf ->
    let sc = ref (conf.score*10000) in
    Bitv.iteri_true (fun bit ->
      let h = Rules.height conf - snd (Rules.coord_of_bit conf bit) in
      sc := !sc - h*h)
      conf.full_cells;
    !sc

let best_heuristic_score_mem =
  Rules.HashConfig.create 17
let rec best_heuristic_score conf = function
  | 0 -> heuristic_score conf
  | depth ->
    match conf with
    | End _ -> heuristic_score conf
    | Cont conf ->
      try
        Rules.HashConfig.find best_heuristic_score_mem conf
      with Not_found ->
        let next = find_reachable_states conf in
        let res =
          List.fold_left (fun acc (conf, _) ->
            let score = best_heuristic_score conf (depth-1) in
            max acc score) min_int next
        in
        Rules.HashConfig.replace best_heuristic_score_mem conf res;
        res

let rec play conf =
  let next = find_reachable_states conf in
  let (_, path) =
    List.fold_left (fun ((scoremax, pathmax) as acc) (conf,path) ->
      let score = best_heuristic_score conf 0 in
      if score <= scoremax then acc
      else (score, path)
    ) (min_int, []) next
  in
  HashConfig.clear best_heuristic_score_mem;
  HashConfig.clear find_reachable_states_mem;
  List.fold_left play_action conf path
