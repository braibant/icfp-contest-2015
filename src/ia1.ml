let version = "0.4-jh"
let max_depth = ref 20


open Rules

type full_conf =
| Cont of config
| End of int

type t = (full_conf * Rules.action list) list

let find_reachable_states_mem =
  Rules.HashConfig.create 17

let find_reachable_states_mark =
  ref 0

let clear_old_elements () =
  let q = Queue.create () in
  Rules.HashConfig.iter (fun config _ ->
      if config.Rules.mark < !find_reachable_states_mark
      then Queue.add config q;
    ) find_reachable_states_mem;
  Queue.iter (Rules.HashConfig.remove find_reachable_states_mem) q

let seen = Hashtbl.create 17
let todo = Queue.create ()
let next = Rules.HashConfig.create 17
let best_ends = ref (-1, [])
let lock_action = ref None
let insert_action data conf path act =
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

let find_reachable_states data init =
  try
    let r = Rules.HashConfig.find find_reachable_states_mem init in
    init.Rules.mark <- !find_reachable_states_mark;
    r
  with Not_found ->
    Hashtbl.clear seen;
    Queue.clear todo;
    Queue.push (init, []) todo;
    Rules.HashConfig.clear next;
    best_ends := (-1, []);
    while not (Queue.is_empty todo) do
      let (conf, path) = Queue.pop todo in
      if Hashtbl.mem seen (conf.unit_pivot, conf.unit_cells) then ()
      else
        begin
          Hashtbl.add seen (conf.unit_pivot, conf.unit_cells) ();
          lock_action := None;
          insert_action data conf path (Move E);
          insert_action data conf path (Move W);
          insert_action data conf path (Move SE);
          insert_action data conf path (Move SW);
          insert_action data conf path (Turn CW);
          insert_action data conf path (Turn CCW);
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
    init.Rules.mark <- !find_reachable_states_mark;
    next


let heuristic_score data config =
  match config with
  | End sc -> (sc-10000)*10000
  | Cont config ->
    Heuristics.simple data config


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
        Printf.printf "At depth %i, %i possible choices\n%!" (!max_depth - depth) (List.length next);
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


(* compute the best outcome in the given set *)
let pick_best data (next:t) : int * Rules.action list =
  List.fold_left (fun ((scoremax, pathmax) as acc) (conf,path) ->
      let score = heuristic_score data conf in
      if score <= scoremax then acc
      else (score, path))
    (min_int, []) next

(* increase the depth of a list of possible moves (i.e., apply
   find_reachable_states when possible), while keeping the path
   identical. *)
let increase_depth data (l: t) : t =
  let r = ref [] in
  List.iter (fun (conf, path) ->
      match conf with
      | End _ -> r := (conf, path) :: !r
      | Cont conf ->
        List.iter
          (fun (c,_) -> r := (c,path) :: !r)
          (find_reachable_states data conf)
    ) l;
  !r

(* given a list of (full_conf, action list), finds the n best
   ones.  *)
let best_candidates score ~keeping l =
  let module T =
  struct
    type t = int * full_conf * Rules.action list
    let compare (a,_,_) (b,_,_) =
      Pervasives.compare (a: int)  b
  end
  in
  let module Q = Binary_heap.Make(T) in
  let add q conf path =
    Q.add q (score conf, conf, path)
  in
  let q = Q.create 1000 in
  List.iter (fun (conf, path) -> add q conf path) l;
  Q.pop_n q keeping
  |> List.map (fun (_,conf,path) -> conf, path)


let dedup : t -> t =
  let seen = Rules.HashConfig.create 17 in
  let best_ends = ref (-1, []) in
  fun (next:t) ->
    let l = ref [] in
    List.iter (fun (conf, path) ->
        match conf with
        | End score ->
          if score > fst !best_ends then
            best_ends := (score, path);
        | Cont c' ->
          if Rules.HashConfig.mem seen c'
          then ()
          else
            begin
              Rules.HashConfig.add seen c' ();
              l := (conf,path) :: !l
            end)
      next;
    let result =
      if fst !best_ends >= 0 then (End (fst !best_ends), snd !best_ends):: !l else !l
    in
    Rules.HashConfig.clear seen;
    best_ends := (-1,[]);
    result

let breadth data next ~depth ~keeping =
  (* assume that next has no duplicates in it. *)
  let rec breadth ~depth ~keeping next : (int * Rules.action list)=
    match depth with
    | 0 ->
      (* let next = best_candidates (markov data ~depth:10) ~keeping next in *)
      pick_best data next
    | depth ->
      let depth = depth - 1 in
      (* Printf.printf "Size %i (keeping %i)\n%!" (List.length next) keeping; *)
      let next = best_candidates (heuristic_score data) ~keeping:(keeping + 2*depth) next in
      let next = increase_depth data next in
      let next = dedup next in
      breadth next ~depth ~keeping
  in
  breadth  ~depth ~keeping next

let rec play data conf =
  let next = find_reachable_states data conf in
  let (_,path) = breadth data next !max_depth 10 in
  (* HashConfig.clear find_reachable_states_mem; *)
  clear_old_elements ();
  incr find_reachable_states_mark;
  List.fold_left (play_action data) conf path
