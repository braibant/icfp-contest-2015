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
  let hexagon () =
    draw_poly [| x + s, y;
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
      hexagon ()
    | Unit ->
      set_color blue;
      hexagon ()
    | Pivot ->
      set_color blue;
      hexagon ();
      moveto (x + s) (y - s);
      set_color black;
      lineto (x + s) (y - s2);
    | Void ->
      assert false
  end

let draw_hex x y i =
  if !cur.(x).(y) <> i then begin
    !cur.(x).(y) <- i;
    (* put y = 0 at the top *)
    let y = !heigth - 1 - y in
    if y mod 2 = 0
    then draw_tile (x*s*2) (y*s*3) i
    else draw_tile ((x*2 - 1)*s) ((y*3 -1)*s) i
  end;
;;

let draw_config config =
  let f t (x,y)  = draw_hex x y t in
  CSet.iter (f Full) config.full_cells;
  CSet.iter (f Unit) config.unit_cells;
  f Pivot config.unit_pivot
;;


let init board =
  width := Rules.width board;
  height := Rules.height board;
  cur := Array.init !width (fun _ -> Array.make !height Void);
  let w = 2 * s * !width in
  let h = 3 * s * !height in
  Graphics.open_graph (Printf.sprintf " %dx%d" w h);
  Graphics.auto_synchronize false;
  (* tiles := *)
  (*   Array.init 10 (fun i -> Graphics.make_image (Array.sub Tiles.raw (i*s) s)); *)
  draw_config board;
;;

let close () = Graphics.close_graph ();;

let show config =
  draw_config config;
  Graphics.synchronize ();
