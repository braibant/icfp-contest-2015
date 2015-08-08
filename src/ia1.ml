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


(* (\* For each conf, store an array of bitvectors, with one bitvector for *)
(*    each line. All the bits of the cells in the line are true.*\) *)
(* let lines : (int * int, _) Hashtbl.t = Hashtbl.create 17 *)

(* let lines_for_conf conf = *)
(*   let height = (Rules.height conf) in *)
(*   let width = (Rules.width conf) in *)
(*   let key = (height, width) in *)
(*   try Hashtbl.find lines key *)
(*   with _ -> *)
(*     let l = *)
(*       Array.init height *)
(*         (fun i -> *)
(*            let b = Rules.create_bitv conf in *)
(*            for j = 0 to width - 1 do *)
(*              Bitv.set b (Rules.bit_of_coord conf (j,i)) true *)
(*            done; *)
(*            b *)
(*         )  in *)
(*     Hashtbl.add lines key l; *)
(*     l *)

(* let popcount bitv = *)
(*   let r = ref 0 in *)
(*   Bitv.iteri_true (fun _ -> incr r) bitv; *)
(*   !r *)

(* let heuristic_line conf = *)
(*   let lines = lines_for_conf conf in *)
(*   let score = ref 0 in *)
(*   Array.iter (fun line -> *)
(*       let elements = popcount (Bitv.bw_and line conf.full_cells) in *)
(*       (\* we want to max elements *\) *)
(*       score := !score + elements * elements *)
(*     ) lines; *)
(*   !score *)

let heuristic_line data config =
  let height = Rules.height data in
  let table = Array.make height 0 in
  Bitv.iteri_true (fun bit ->
      let line = snd @@ Rules.coord_of_bit data bit in
      table.(line) <- table.(line) + 1
    ) config.full_cells;
  Array.fold_left (fun acc elements -> acc + elements * elements) 0 table

let heuristic_score data config =
  match config with
  | End sc -> (sc-10000)*10000
  | Cont config ->
    let sc = ref (config.score*10000) in
    Bitv.iteri_true (fun bit ->
      let h = Rules.height data - snd (Rules.coord_of_bit data bit) in
      sc := !sc - h*h)
      config.full_cells;
    sc := !sc + heuristic_line data config;
    let delta = [(0, 1);(1, 0);(0, -1);(-1, 0);(1,-1);(-1,1)] in
    (* Bitv.iteri_true (fun bit -> *)
    (*   let cell = cell_of_bit data bit in *)
    (*   let neigh = List.map (Cell.(+) cell) delta in *)
    (*   let neigh = List.filter (fun c -> check_cell data c = 0) neigh in *)
    (*   let neig_empty = List.filter (fun c -> *)
    (*     not (Bitv.get config.full_cells (bit_of_cell data c))) *)
    (*     neigh *)
    (*   in *)
    (*   if List.length neig_empty <= 1 then sc := !sc - 40; *)
    (*   if List.length neig_empty = 2 then sc := !sc - 10; *)
    (* ) (Bitv.bw_not config.full_cells); *)
    !sc


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
