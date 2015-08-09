let open_in filename =
  let lexer = Yojson.init_lexer () in
  let channel = Pervasives.open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let problem = Formats_j.read_input lexer lexbuf in
  close_in channel;
  problem

let make_output problem seed solution tag =
  let open Formats_t in
  {
    problemId = problem.id;
    seed;
    tag;
    solution
  }

let single filename options heuristic tag =
  let problem = open_in filename in
  let solve seed_id seed =
    Printf.printf "Problem %i, seed %i (%i/%i)-- length %i\n%!"
      problem.Formats_t.id
      seed
      seed_id
      (List.length problem.Formats_t.sourceSeeds -1 )
      problem.Formats_t.sourceLength;
    let data, init = Rules.init problem seed_id in
    let state = ref init in
    let n = ref 0 in
    try
      while true do
        incr n;
        (* if !n mod 10 = 0 *)
        (* then Printf.printf "Turn %i/%i\n%!" !n       problem.Formats_t.sourceLength; *)
        state := Ia1.play data !state
      done;
      assert false
    with Rules.End (score,commands) ->
      Printf.printf "Final score : %d\n" score;
      let solution = Oracle.empower_power commands data init in
      let output = make_output problem seed solution tag in
      output, score
  in
  let outputs = List.mapi solve problem.Formats_t.sourceSeeds in

  let score = List.fold_left (fun acc (_,score) -> acc + score) 0 outputs in
  let score = score / (List.length problem.Formats_t.sourceSeeds) in
  score

let all options weights tag : int =
  let filenames = Sys.readdir "problems" |> Array.to_list |> List.sort Pervasives.compare in
  let filenames = List.map (fun s -> "problems/" ^ s) filenames in
  List.fold_left (fun acc x -> acc + single x options weights tag) 0 filenames

let log weights score =
  Printf.printf
    "[| %s |] -> %i\n%!"
    (String.concat "; " (Array.to_list weights |> List.map string_of_float))
    score


let delta = 0.0000001

let partial_derivative f_xs f xs i xi =
  xs.(i) <- xi +. delta;
  let r = (f xs -. f_xs) /. delta in
  xs.(i) <- xi;
  r

let ( **. ) lambda vect = Array.map (fun x -> lambda *. x) vect
let ( ++. ) a b = Array.init (Array.length a) (fun i -> a.(i) +. b.(i))


let descend alpha beta f f' (lambda, xs, f_xs) =
  let xs_2 = xs ++. (-.lambda) **. (f' xs) in
  let f_xs_2 = f xs_2 in
  if f_xs_2 >= f_xs
  then
    alpha *. lambda, xs, f_xs
  else
    beta *.  lambda, xs_2, f_xs_2

let rec fixed_point f x =
  let f_x = f x in
  if f_x = x then x else fixed_point f f_x

let gradient_descent f f' xs =
  let (_,xs,_) = fixed_point (descend 0.5 1.1 f f') (delta, xs, f xs)
  in xs

let grad f xs =
  Array.mapi (partial_derivative (f xs) f xs) xs

let rosenbrock xs =
  let x,y = xs.(0), xs.(1) in
  (1.0 -. x)  ** 2. +. 100.0 *. (y -. x ** 2.) ** 2.

 (* let xs = [| 0.0; 0.0|] in gradient_descent rosenbrock (grad rosenbrock) xs *)

let meta options =
  let l = ref [] in
  let h = Heuristics.meta in
  for i = 0 to 200 do
    let weights = Heuristics.init () in
    let score = single "problems/problem_1.json" options (h weights) ""  in
    log weights score;
    l := (weights, score) :: !l
  done;
  let l = List.sort (fun a b -> Pervasives.compare (snd a) (snd b)) !l in
  List.iter (fun (w,s) -> log w s) l
