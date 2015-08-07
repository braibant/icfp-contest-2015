open Rules

type t =
  | Full
  | Unit
  | Pivot
  | Void

let s = 20;;  (* size of a tile (width and height) *)

let width = ref 0;;
let height = ref 0;;
let cur = ref [| |]


let draw_tile x y i =
  let open Graphics in
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
      draw_poly hexagon;
    | Pivot ->
      set_color blue;
      draw_poly hexagon;
      moveto (x + s) (y - s);
      set_color black;
      lineto (x + s) (y - s2);
    | Void ->
      set_color black;
      draw_poly hexagon;
  end

let draw_hex x y i =
  (* if !cur.(x).(y) <> i then begin *)
  (*   !cur.(x).(y) <- i; *)
    (* put y = 0 at the top *)
  let cx = (y - x)*1*s in
  let cy= (x + y)*(2)*s in

  draw_tile cx ((!height * 2 * s - cy)+s) i
;;

let draw_config config =
  for i = 0 to !width - 1 do
    for j = 0 to !height - 1 do
      let (x,y) = Rules.Cell.of_coord (i,j) in
      draw_hex x y Void;
    done;
  done;

  let f t ((x,y) : Rules.CSet.elt)  =
    Printf.printf "x:%i  y:%i\n" x y;
    draw_hex x y t in
  CSet.iter (f Full) config.full_cells;
  CSet.iter (f Unit) config.unit_cells;
  f Pivot config.unit_pivot
;;

let score_x = 100

let draw_score config =
  let w = Graphics.size_x () - score_x in
  let h = Graphics.size_y () / 2 in
  Graphics.moveto w h;
  Graphics.draw_string (Printf.sprintf "Score: %i" config.Rules.score)

let init board =
  width := Rules.width board;
  height := Rules.height board;
  cur := Array.init !width (fun _ -> Array.make !height Void);
  let w = 2 * s * !width + s + score_x in
  let h = 2 * s * !height + s / 3 + 10 in
  Graphics.open_graph (Printf.sprintf " %dx%d" w h);

  Graphics.auto_synchronize false;
  (* tiles := *)
  (*   Array.init 10 (fun i -> Graphics.make_image (Array.sub Tiles.raw (i*s) s)); *)
  draw_config board;
;;


let close () = Graphics.close_graph ();;

let show config =
  Graphics.clear_graph ();
  draw_config config;
  draw_score config;
  Graphics.synchronize ();
