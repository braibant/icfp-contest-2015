(* look at the pieces, and try to see if they look like lines...  If
   we can build lines with them, then, apply a greedy algorithm *)

let positions data cell pivot =
  let open Rules in
  let f x =  rotate_unit data x pivot CW in
  [|
    cell;
    f cell;
    f (f cell);
    f (f (f cell));
    f (f (f (f cell)));
    f (f (f (f (f cell))));
  |]

(* is this piece (a bitv) horizontal? *)
let is_line data cells =
  let cells = Rules.cells_of_bitv data cells in
  match cells with
  | [] ->  false                (* we got an invalid position... *)
  | c::l -> let line = fst (Rules.Cell.to_coord c) in
    List.for_all (fun c -> fst (Rules.Cell.to_coord c) = line) l

(* computes the set of pieces that can be horizontal lines *)
let good_candidate data =
  let units = Rules.units data in
  Array.mapi (fun i (cells,pivot) ->
      let positions = positions data cells pivot in
      let found = ref [] in
      Array.iteri (fun k cells ->
          if is_line data cells
          then found := k :: !found;
        ) positions;
      !found
    ) units

(* let mask data size = *)
(*   let r = ref (Rules.line data (heigth data - 1)) in *)
(*   for i = 1 to size - 1 do *)
(*     r := Bitv.bw_and !r (Rules.(line data ((heigth data - 1) - i))) *)
(*   done; *)
(*   r *)

(* let mask =  *)


(*   let popcount_line data bitv line = *)
(*     let line = line in *)
(*     Bitv.pop (Bitv.bw_and bitv line) *)


(* let place_one candidates data config cc = *)
(*   if candidates.(config.Rules.unit_id) <> [] *)
(*   then (\* this is a unit that can make a line *\) *)
(*     begin *)
(*       let reachable_states = Ia1.find_reachable_states data config in *)
(*       let non_final, final = *)
(*         List.fold_left (fun (cont,final) (conf,path) -> match conf with *)
(*             | Ia1.Cont (c, pre) -> (c,path,pre)::cont, final *)
(*             | Ia1.End i -> cont, (i,path)::final *)
(*           ) ([],[]) reachable_states *)
(*       in *)
(*       assert false *)
(*     end *)
(*   else *)
(*     (\* call current continuation *\) *)
(*     cc data config *)
