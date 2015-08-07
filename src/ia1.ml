open Rules

type node = {
  config: config;
  rot: int;
  path: action list
}

module Node =
struct
  type t = node
  let compare a b =
    Pervasives.compare (a.rot, a.config.unit_pivot)
      (b.rot, b.config.unit_pivot)
end

module NSet = Set.Make(Node)

type full_conf =
| Cont of config
| End of int

let find_reachable_states_mem =
  Rules.HashConfig.create 17
let find_reachable_states init =
  try Rules.HashConfig.find find_reachable_states_mem init
  with Not_found ->
    let seen = ref NSet.empty in
    let todo = Queue.create () in
    Queue.push { config=init; rot=0; path=[] } todo;
    let ends = ref [] in
    while not (Queue.is_empty todo) do
      let node = Queue.pop todo in
      if NSet.mem node !seen then ()
      else
        begin
          seen := NSet.add node !seen;
          let lock_action = ref None in
          let insert_action act =
            try
              let node =
                match act with
                | Turn dir ->
                  { config = rotate dir node.config;
                    rot = ((match dir with CW -> 1 | CCW -> 5)+node.rot) mod 6;
                    path = act::node.path }
                | Move dir ->
                  { config = move dir node.config;
                    rot = node.rot;
                    path = act::node.path }
                | Nop -> assert false
              in
              Queue.push node todo
            with Invalid_conf -> lock_action := Some act
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
              try Cont (play_action node.config act)
              with Rules.End (score, _path) -> End (score)
            in
            ends := (conf, List.rev (act::node.path))::!ends end
        end
    done;
    Rules.HashConfig.replace find_reachable_states_mem init !ends;
    !ends

let euristic_score conf =
  match conf with
  | End sc -> (sc-1000)*10000
  | Cont conf ->
    let sc = ref (conf.score*10000) in
    Bitv.iteri_true (fun bit ->
      sc := !sc + snd (Rules.coord_of_bit conf bit))
      conf.full_cells;
    !sc

let best_euristic_score_mem =
  Rules.HashConfig.create 17
let rec best_euristic_score conf = function
  | 0 -> euristic_score conf
  | depth ->
    match conf with
    | End _ -> euristic_score conf
    | Cont conf ->
      try
        Rules.HashConfig.find best_euristic_score_mem conf
      with Not_found ->
        let next = find_reachable_states conf in
        let res =
          List.fold_left (fun acc (conf, _) ->
            let score = best_euristic_score conf (depth-1) in
            max acc score) (-1) next
        in
        Rules.HashConfig.replace best_euristic_score_mem conf res;
        res

let rec play conf =
  let ends = find_reachable_states conf in
  let (_, path) =
    List.fold_left (fun ((scoremax, pathmax) as acc) (conf,path) ->
      let score = best_euristic_score conf 0 in
      if score <= scoremax then acc
      else (score, path)
    ) (-1, []) ends
  in
  HashConfig.clear best_euristic_score_mem;
  HashConfig.clear find_reachable_states_mem;
  List.fold_left play_action conf path