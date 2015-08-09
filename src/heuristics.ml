open Rules

(* \sum_i table.(i)^2 *)
let heuristic_line data config =
  let height = Rules.height data in
  let table = Array.make height 0 in
  Bitv.iteri_true (fun bit ->
      let line = snd @@ Rules.coord_of_bit data bit in
      table.(line) <- table.(line) + 1
    ) config.full_cells;
  let sc = ref 0 in
  Array.iteri (fun i elements -> sc := !sc + elements * elements + elements * i) table;
  !sc

(* ~ cells with empty neighbours *)
let delta = [(0, 1);(1, 0);(0, -1);(-1, 0);(1,-1);(-1,1)]
let neigh cell =  List.map (Cell.(+) cell) delta
let heuristic_neighbours data config =
  let sc = ref 0 in
  let n = ref 0 in
  Bitv.iteri_true (fun bit ->
      n := 0;                   (* number of full neighbours *)
      let cell = cell_of_bit data bit in
      let neigh = neigh cell in
      List.iter (fun x ->
          if check_cell data x = 0
          then
            if Bitv.get config.full_cells (bit_of_cell data x)
            then incr n
            else ()
          else incr n          (* if the cell is not valid, count is a a neighbour *)
        ) neigh;
      if 5 <= !n  then sc := !sc - 10;
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
  config.score  * 10_000

let signals =
  [|
    heuristic_line, "heuristic line";
    heuristic_neighbours, "heuristic neighbours";
    heuristic_base, "heuristic base";
    score, "score"
  |]


(* https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/

   the bumpiness measure the absolute value of the difference between
   the number of elements in each column. We want to minimize this
   value (hence, the score is negative)

*)
let heuristic_bumpiness data config =
  (* compute the number of elements in  *)
  let sc = ref 0 in
  let width = Rules.width data in
  let table = Array.make width 0 in
  (* let height = Rules.height data in *)
  Bitv.iteri_true (fun bit ->
      let col = fst (Rules.coord_of_bit data bit) in
      table.(col) <- table.(col) + 1;
    )
    config.full_cells;
  for i = 1 to width - 1 do
    sc := !sc - abs (table.(i) - table.(i - 1));
  done;
  !sc

let simple data config =
  (* 0 *)
  + heuristic_base data config
  + score data config +
  + heuristic_line data config
  (* + heuristic_neighbours data config *)
  (* + heuristic_bumpiness data config *)

(* let medium data config = *)
(*   score data config *)
(*   + heuristic_line data config *)
(*   + heuristic_neighbours data config *)

(* http://www.cs.cmu.edu/afs/cs/project/ACRL/www/TetrisReports/Breelyn_Eric_Don_Project.pdf *)




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
