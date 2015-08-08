let version = "0.4-jh"
let max_depth = ref 0

open Rules

type full_conf =
| Cont of config
| End of int

let find_reachable_states_mem =
  Rules.HashConfig.create 17

let find_reachable_states data init =
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
                | Turn dir -> (rotate data dir conf, act::path)
                | Move dir -> (move data dir conf, act::path)
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
              Rules.HashConfig.replace next (play_action data conf act)
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


let heuristic_score data config =
  match config with
  | End sc -> (sc-10000)*10000
  | Cont config ->
    int_of_float (Heuristics.meta Heuristics.hand_tuned data config)


let best_heuristic_score_mem =
  Rules.HashConfig.create 17

let rec best_heuristic_score data conf = function
  | 0 -> heuristic_score data conf
  | depth ->
    match conf with
    | End _ -> heuristic_score data conf
    | Cont conf ->
      try
        Rules.HashConfig.find best_heuristic_score_mem conf
      with Not_found ->
        let next = find_reachable_states data conf in
        let res =
          List.fold_left (fun acc (conf, _) ->
              let score = best_heuristic_score data conf (depth-1) in
              max acc score) min_int next
        in
        Rules.HashConfig.replace best_heuristic_score_mem conf res;
        res

let rec play data conf =
  let next = find_reachable_states data conf in
  let (_, path) =
    List.fold_left (fun ((scoremax, pathmax) as acc) (conf,path) ->
      let score = best_heuristic_score data conf !max_depth in
      if score <= scoremax then acc
      else (score, path)
    ) (min_int, []) next
  in
  HashConfig.clear best_heuristic_score_mem;
  HashConfig.clear find_reachable_states_mem;
  List.fold_left (play_action data) conf path
