open Rules

type t =
  | Full
  | Unit
  | Pivot
  | Void

  | Uninitialized               (* Not yet initialized *)

let s = ref 20;;  (* size of a tile (width and height) *)

let width = ref 0;;
let height = ref 0;;
let cur = ref [| |]

let draw_tile x y i =
  let open Graphics in
  let s = ! s in
  let s2 = 2*s in
  let s3 = 3*s in
  let hexagon = [| x + s, y;
                   x + s2, y - s;
                   x + s2, y - s2;
                   x + s, y -s3;
                   x, y -s2;
                   x, y-s;
                |]
  in
  begin match i with
    | Full ->
      set_color red;
      fill_poly hexagon;
    | Unit ->
      set_color blue;
      fill_poly hexagon;
    | Pivot ->
      moveto (x + s) (y - s);
      set_color black;
      lineto (x + s) (y - s2);
    | Void ->
      set_color background;
      fill_poly hexagon;
      set_color black;
      draw_poly hexagon;
    | Uninitialized -> assert false
  end

let draw_hex data x y i =
  let x',y' = Rules.Cell.to_coord (x,y) in
  let s = ! s in
  try
  if !cur.(x').(y') <> i then begin
    !cur.(x').(y') <- i;
    let cx = (y - x)*1* s in
    let cy= (x + y)*(2)*s in
    draw_tile cx ((!height * 2 * s - cy)+s) i
  end
  with _ -> ()

let draw_config data config =
  for i = 0 to !width - 1 do
    for j = 0 to !height - 1 do
      let (x,y) = Rules.Cell.of_coord (i,j) in
      draw_hex data x y Void;
    done;
  done;

  let f t (x,y)  =
    (* Printf.printf "x:%i  y:%i\n" x y; *)
    draw_hex data x y t in
  Bitv.iteri_true (fun bit -> f Full (Rules.cell_of_bit data bit)) config.full_cells;
  Array.iter (fun cell  -> f Unit cell) config.unit_cells;
  f Pivot config.unit_pivot
;;

let score_x = 100

let draw_score config =
  let w = Graphics.size_x () - score_x in
  let h = Graphics.size_y () / 2 in
  Graphics.set_color Graphics.background;
  Graphics.moveto w h;
  Graphics.fill_rect w h 100 100;
  Graphics.set_color Graphics.black;
  Graphics.moveto w h;
  Graphics.draw_string (Printf.sprintf "Score: %i" config.Rules.score)

let resize size =
  if size <= 0 then ()
  else
    begin
      s := size;
      cur := Array.init !width (fun _ -> Array.make !height Uninitialized);
      let s = !s in
      let w = 2 * s * !width + s + score_x in
      let h = 2 * s * !height + s / 3 + 10 in
      Graphics.resize_window w h;
      Graphics.synchronize ()
    end

let size () = !s

let init board =
  width := Rules.width board;
  height := Rules.height board;
  Graphics.open_graph " ";
  Graphics.auto_synchronize false;

  resize !s;
  draw_config board;
;;


let close () = Graphics.close_graph ();;

let show data config =
  draw_config data config;
  draw_score  config;
  Graphics.synchronize ();
