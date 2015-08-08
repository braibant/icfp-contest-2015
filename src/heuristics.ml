open Rules

(* \sum_i table.(i)^2 *)
let heuristic_line data config =
  let height = Rules.height data in
  let table = Array.make height 0 in
  Bitv.iteri_true (fun bit ->
      let line = snd @@ Rules.coord_of_bit data bit in
      table.(line) <- table.(line) + 1
    ) config.full_cells;
  Array.fold_left (fun acc elements -> acc + elements * elements) 0 table

(* ~ cells with empty neighbours *)
let delta = [(0, 1);(1, 0);(0, -1);(-1, 0);(1,-1);(-1,1)]
let neigh cell =  List.map (Cell.(+) cell) delta
let heuristic_neighbours data config =
  let sc = ref 0 in
  Bitv.iteri_true (fun bit ->
      let cell = cell_of_bit data bit in
      let neigh = neigh cell in
      let neigh = List.filter (fun c -> check_cell data c = 0) neigh in
      let neig_empty = List.filter (fun c ->
          not (Bitv.get config.full_cells (bit_of_cell data c)))
          neigh
      in
      if List.length neig_empty <= 1 then sc := !sc - 40;
      if List.length neig_empty = 2 then sc := !sc - 10;
    ) (Bitv.bw_not config.full_cells);
  !sc

(* ~ \sum_cell height(cell)^2 *)
let heuristic_base data config =
  let sc = ref 0 in
  let height = Rules.height data in
  Bitv.iteri_true (fun bit ->
      let h = height - snd (Rules.coord_of_bit data bit) in
      sc := !sc - h*h)
    config.full_cells;
  !sc

let score data config =
  config.score

let signals =
  [|
    heuristic_line, "heuristic line";
    heuristic_neighbours, "heuristic neighbours";
    heuristic_base, "heuristic base";
    score, "score"
  |]

let simple data config =
  heuristic_base data config + score data config + heuristic_line data config

let meta weights data config : int =
  let acc = ref 0. in
  Array.iteri (fun i (x,l) ->
      if abs_float weights.(i) < 0.01
      then ()
      else acc := !acc +. weights.(i) *. float (x data config)) signals;
  int_of_float !acc

(* random vector *)
let init () =
  Array.map (fun _ -> Random.float 10. -. 5.) signals

let hand_tuned =
  [| 1.0; 0.0; 1.0; 1.0|]