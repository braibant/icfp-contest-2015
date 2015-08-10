open Rules

type full_conf =
| Cont of config
| End of int

type t = (full_conf * Rules.action list) list

let version = "0.4-jh"
let max_depth = ref 15
let keeping = ref 10

(* used to fine tune the parameters, based on how fast we go *)
let total_time = ref 0.
let turns = ref 0

let find_reachable_states_mem =
  Rules.HashConfig.create 17

let find_reachable_states_mark =
  ref 0

let reset () =
  Rules.HashConfig.reset find_reachable_states_mem;
  find_reachable_states_mark := 0;
  total_time := 0.;
  turns := 0


(* ************************************************************************ *)

let clear_old_elements () =
  let q = Queue.create () in
  Rules.HashConfig.iter (fun config _ ->
      if config.Rules.mark < !find_reachable_states_mark
      then Queue.add config q;
    ) find_reachable_states_mem;
  Queue.iter (Rules.HashConfig.remove find_reachable_states_mem) q


(* Hash type for a complete piece *)
module HashablePiece = struct
  type t = Piece.t * (int * int)

  let equal : t -> t -> bool = fun (av, (ax,ay)) (bv, (bx,by)) ->
    (ax: int) = bx && (ay : int) = by && Rules.Piece.equal av bv

  let hash (av,(ax,ay)) =
    (Rules.Piece.hash av lsl 32 + ax  lsl 16 + ay) land max_int
end
module HashPiece = Hashtbl.Make(HashablePiece)

(* Hash type that looks only for the full cells. *)
module HashableConfigPiece = struct
  type t = Rules.config

  let equal a b = Bitv.equal a.full_cells b.full_cells
  let hash a =  Bitv.hash a.full_cells
end
module HashConfigPiece= Hashtbl.Make(HashableConfigPiece);;

module WithPath = struct

  let seen = HashPiece.create 17
  let todo = Queue.create ()
  let next = HashConfigPiece.create 17
  let best_ends = ref (-1, [])
  let lock_action = ref None
  let insert_action data conf path act =
    try
      let node =
        match act with
        | (CW | CCW) as dir -> (rotate data dir conf, act::path)
        | dir -> (move data dir conf, act::path)
      in
      Queue.push node todo
    with Invalid_conf ik ->
      if ik land (invalid_overlap lor invalid_bottom) <> 0 then
        lock_action := Some act

  let find_reachable_states data init =
    (* do not attempt to memoize, this function is only called at top
       level *)
    HashPiece.clear seen;
    Queue.clear todo;
    Queue.push (init, []) todo;
    HashConfigPiece.clear next;
    best_ends := (-1, []);
    while not (Queue.is_empty todo) do
      let (conf, path) = Queue.pop todo in
      let piece = ( conf.unit_cells,conf.unit_pivot) in
      if HashPiece.mem seen piece then ()
      else
        begin
          HashPiece.add seen piece ();
          lock_action := None;
          insert_action data conf path ( E);
          insert_action data conf path ( W);
          insert_action data conf path ( SW);
          insert_action data conf path ( SE);
          insert_action data conf path ( CW);
          insert_action data conf path ( CCW);
          begin match !lock_action with
            | None -> ()
            | Some act ->
              let path = act :: path in
              try
                HashConfigPiece.replace next (play_action data conf act) path
              with Rules.End (score, _path) ->
                if score > fst !best_ends then
                  best_ends := (score, path)
          end
        end
    done;
    let next = HashConfigPiece.fold (fun conf path acc -> (Cont conf,path)::acc) next [] in
    let next =
      if fst !best_ends >= 0 then (End (fst !best_ends), snd !best_ends)::next else next
    in
    assert (next <> []);
    next

end

module WithoutPath = struct
  let seen = HashPiece.create 17
  let todo = Queue.create ()
  let next = HashConfigPiece.create 17
  let best_ends = ref (-1)
  let lock_action = ref None
  let insert_action data conf act =
    try
      let node =
      match act with
      | (CW | CCW) as dir -> (rotate data dir conf)
      | dir -> (move data dir conf)
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
      HashPiece.clear seen;
      Queue.clear todo;
      Queue.push (init) todo;
      HashConfigPiece.clear next;
      best_ends := (-1);
      while not (Queue.is_empty todo) do
        let conf = Queue.pop todo in
        let piece = ( conf.unit_cells,conf.unit_pivot) in
        if HashPiece.mem seen piece then ()
        else
          begin
            HashPiece.add seen piece ();
            lock_action := None;
            insert_action data conf ( E);
            insert_action data conf ( W);
            insert_action data conf ( SW);
            insert_action data conf ( SE);
            insert_action data conf ( CW);
            insert_action data conf ( CCW);
            begin match !lock_action with
              | None -> ()
              | Some act ->
                try HashConfigPiece.replace next (play_action data conf act) ()
                with Rules.End (score, _path) ->
                  if score >  !best_ends then
                    best_ends := (score)
            end
          end
      done;
      let next = HashConfigPiece.fold (fun conf _ acc -> (Cont conf)::acc) next [] in
      let next =
        if  !best_ends >= 0 then End (!best_ends)::next else next
      in
      assert (next <> []);
      Rules.HashConfig.replace find_reachable_states_mem init next;
      init.Rules.mark <- !find_reachable_states_mark;
      next
end

let find_reachable_states = WithPath.find_reachable_states
let find_reachable_states_without_path = WithoutPath.find_reachable_states

let heuristic_score data config =
  match config with
  | End sc -> (sc-10000)*10000
  | Cont config ->
    Heuristics.simple data config

(* ************************************************************************ *)


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
          (fun c -> r := (c,path) :: !r)
          (find_reachable_states_without_path data conf)
    ) l;
  !r

(* given a list of (full_conf, action list), finds the n best
   ones.  *)
module T =
  struct
    type t = int * full_conf * Rules.action list
    let compare (a,_,_) (b,_,_) =
      Pervasives.compare (a: int)  b
  end
module Q = Binary_heap.Make(T)
let best_candidates score ~keeping l =
  let q = Q.create 1000 in
  let add (conf,path) =
    Q.add q (score conf, conf, path)
  in
  List.iter (add) l;
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
      Printf.printf "Size %i (keeping %i)\n%!" (List.length next) keeping;
      let next = best_candidates (heuristic_score data) ~keeping:(keeping + 2*depth) next in
      let next = increase_depth data next in
      let next = dedup next in
      breadth next ~depth ~keeping
  in
  breadth  ~depth ~keeping next

(* ************************************************************************ *)

let  play data conf =
  let start = Unix.gettimeofday () in
  let next = find_reachable_states data conf in
  let (_,path) = breadth data next !max_depth !keeping in
  clear_old_elements ();
  let spent = Unix.gettimeofday () -. start in

  (* increment the total time *)
  total_time := !total_time +. spent;
  incr turns;
  if !turns > 0 then
    begin
    let mean = !total_time /. float !turns  in
    Printf.printf "Mean time per turn %f, max_depth %i\n%!" mean !max_depth
    end;

  let open Rules in
  if !turns > 2
  then
    begin
      if !total_time +. float (Rules.source_length data  - !turns) *. spent > Rules.time data
         && !max_depth > 5
      then begin

        Printf.printf "decr (%i)" !max_depth; decr max_depth;
      end;
      if !total_time +. float (Rules.source_length data - !turns) *. spent <  0.8 *. Rules.time data
         && !max_depth < 20
      then
        begin
          Printf.printf "incr (%i)" !max_depth; incr max_depth;
        end;
    end;

  incr find_reachable_states_mark;
  List.fold_left (play_action data) conf (List.rev path)
