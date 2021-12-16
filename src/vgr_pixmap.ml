open Gg
open Vg
open Vgr
module Pv = Private
module Float = Stdlib.Float

(** Temporary debugging functions. *)
module Debug = struct
  let pp_img i = Printf.printf "Image: %s\n" @@ I.to_string @@ Pv.I.of_data i
  let pp_path p = Printf.printf "Path: %s\n" @@ P.to_string @@ Pv.P.of_data p

  let pp_segs segs =
    Printf.printf "Seg: ";
    List.iter (fun pt -> Printf.printf "(%f, %f); " (P2.x pt) (P2.y pt)) segs;
    Printf.printf "\n"

  let spf = Printf.sprintf
  let log ?(s = "LOG") = Printf.printf "\t[%s] %s\n" s
end

module D = Debug

module type PixmapType = sig
  type t

  val create : int -> int -> t
  val get : t -> float -> float -> Gg.color
  val set : t -> float -> float -> Gg.color -> unit
  val w : t -> int
  val h : t -> int
end

module F32_ba : PixmapType = struct
  open Ba

  type t =
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t
    * int
    * int

  let stride = 4

  let get_i x y h =
    let x' = int_of_float x and y' = int_of_float y in
    ((x' * h) + y') * stride

  let create w h =
    let ba = Ba.create Ba.Float32 (w * h * stride) in
    Ba.fill ba 0.;
    (ba, w, h)

  let get (b, _, h) x y = Ba.get_v4 b (get_i x y h)
  let set (b, _, h) x y c = Ba.set_v4 b (get_i x y h) c
  let w (_, w, _) = w
  let h (_, _, h) = h
end

module type S = sig
  type pixmap

  val target : pixmap -> float -> [ `Other ] Vg.Vgr.target
end

(** Extends the [Box2] module by adding convenient functions. *)
module Box2 = struct
  include Box2

  (** [iter f b] is a classical itering function on [box2]. *)
  let iter (f : 'int -> int -> unit) (b : box2) : 'a =
    let to_int f = f b |> int_of_float in
    let minx = to_int Box2.minx
    and miny = to_int Box2.miny
    and maxx = to_int Box2.maxx
    and maxy = to_int Box2.maxy in
    let rec loop x y =
      if x = maxx && y = maxy then ()
      else (
        f x y;
        if x = maxx then loop minx (y + 1) else loop (x + 1) y)
    in
    loop minx miny
end

(** [Stroker] contains all the implementation of algorithms needed to render 2D
    graphics primitives shuch as lines or Bézier curves.

    All point coordinates used by the following functions are assumed to be
    scaled (see {!state.scaling}). ) *)
module Stroker = struct
  (** [bresenham_line x0 y0 x1 y1] adds all the calculated points of the line
      from ([x0], [y0]) to ([x1], [y1]) to [pts].

      The Bresenham's line algorithm is used (see
      https://en.wikipedia.org/wiki/Bresenham's_line_algorithm) *)
  let bresenham_line (x0 : int) (y0 : int) (x1 : int) (y1 : int) (w : int) :
      p2 list =
    let dx = abs (x1 - x0)
    and sx = if x0 < x1 then 1 else -1
    and dy = -1 * abs (y1 - y0)
    and sy = if y0 < y1 then 1 else -1
    and is_horizontal = y0 = y1 in
    let err = dx + dy in

    let width_line x y =
      let calculate (x : int) (y : int) : p2 list =
        let x_start = x - (w / 2) in
        let x_stop = x + (w / 2) in
        let rec loop acc x =
          let acc =
            (if is_horizontal then P2.v (float_of_int y) (float_of_int x)
            else P2.v (float_of_int x) (float_of_int y))
            :: acc
          in
          if x_stop = x then acc else loop acc (x + 1)
        in
        loop [] x_start
      in
      if is_horizontal then calculate y x else calculate x y
    in

    let rec loop pts x y err =
      if x = x1 && y = y1 then pts
      else
        let e2 = 2 * err in
        let err = if e2 >= dy then err + dy else err in
        let x = if e2 >= dy then x + sx else x in
        let err = if e2 <= dx then err + dx else err in
        let y = if e2 <= dx then y + sy else y in
        loop (width_line x y @ pts) x y err
    in
    loop [] x0 y0 err

  let line_width (x0 : int) (y0 : int) (x1 : int) (y1 : int) (width : float) :
      (p2 * float) list =
    let dx = abs (x1 - x0)
    and sx = if x0 < x1 then 1 else -1
    and dy = abs (y1 - y0)
    and sy = if y0 < y1 then 1 else -1 in
    let err = ref (dx - dy)
    and ed =
      if dx + dy = 0 then 1. else sqrt (float_of_int ((dx * dx) + (dy * dy)))
    and x0, y0, e2, x2, y2, pxs, break =
      (ref x0, ref y0, ref 0, ref 0, ref 0, ref [], ref false)
    in

    D.log
    @@ D.spf "width = %f, (width +. 1.) /. 2. = %f" width ((width +. 1.) /. 2.);

    while not !break do
      pxs :=
        ( P2.v (float_of_int !x0) (float_of_int !y0),
          max 0.
            (255.
            *. (Float.abs (float_of_int (!err - dx + dy)) /. (ed -. width +. 1.))
            ) )
        :: !pxs;
      e2 := !err;
      x2 := !x0;
      if 2 * !e2 >= -dx then (
        e2 := !e2 + dy;
        y2 := !y0;
        while float_of_int !e2 < ed *. width && (y1 <> !y2 || dx > dy) do
          y2 := !y2 + sy;
          pxs :=
            ( P2.v (float_of_int !x0) (float_of_int !y2),
              max 0.
                (255.
                *. (Float.abs (float_of_int (!err - dx + dy))
                   /. (ed -. width +. 1.))) )
            :: !pxs;
          e2 := !e2 + dx
        done;
        if !x0 = x1 then break := true;
        e2 := !err;
        err := !err - dy;
        x0 := !x0 + sx);
      if (not !break) && 2 * !e2 <= dy then (
        e2 := dx - !e2;
        while float_of_int !e2 < ed *. width && (x1 <> !x2 || dx < dy) do
          x2 := !x2 + sx;
          pxs :=
            ( P2.v (float_of_int !x2) (float_of_int !y0),
              max 0.
                (255.
                *. (Float.abs (float_of_int (!err - dx + dy))
                   /. (ed -. width +. 1.))) )
            :: !pxs;
          e2 := !e2 + dy
        done;
        if !y0 = y1 then break := true;
        err := !err + dx;
        y0 := !y0 + sy)
    done;
    !pxs

  (** FIXME:*)
  let rec xiaolin_wu_line (x0 : float) (y0 : float) (x1 : float) (y1 : float) :
      (p2 * float) list =
    let open Float in
    let ipart x = floor x in
    let fpart x = x -. floor x in
    let rfpart x = 1. -. fpart x in

    let steep = abs (y1 -. y0) > abs (x1 -. x0) in

    if steep then xiaolin_wu_line y0 x0 y1 x1
    else if x0 > x1 then xiaolin_wu_line x1 y1 x0 y0
    else
      let dx = x1 -. x0 and dy = y1 -. y0 in
      let gradient = if 0. = dx then 1. else dy /. dx in

      (* Handles the first endpoint. *)
      let xend = round x0 in
      let yend = y0 +. (gradient *. (xend -. x0)) in
      let xgap = rfpart (x0 +. 0.5) in
      let xpxl1 = xend in
      let ypxl1 = ipart yend in
      let pts_to_draw =
        if steep then
          [
            (P2.(v ypxl1 xpxl1), rfpart yend *. xgap);
            (P2.(v (ypxl1 +. 1.) xpxl1), fpart yend *. xgap);
          ]
        else
          [
            (P2.(v xpxl1 ypxl1), rfpart yend *. xgap);
            (P2.(v xpxl1 (ypxl1 +. 1.)), fpart yend *. xgap);
          ]
      in

      (* First y-intersection for the main loop. *)
      let intery = yend +. gradient in

      (* Handles the second endpoint. *)
      let xend = round x1 in
      let yend = y1 +. (gradient *. (xend -. x1)) in
      let xgap = rfpart (x1 +. 0.5) in
      let xpxl2 = xend in
      let ypxl2 = ipart yend in
      let pts_to_draw =
        pts_to_draw
        @
        if steep then
          [
            (P2.(v ypxl2 xpxl2), rfpart yend *. xgap);
            (P2.(v (ypxl2 +. 1.) xpxl2), fpart yend *. xgap);
          ]
        else
          [
            (P2.(v xpxl2 ypxl2), rfpart yend *. xgap);
            (P2.(v xpxl2 (ypxl2 +. 1.)), fpart yend *. xgap);
          ]
      in

      (* Main loop *)
      let rec loop intery (x : float) acc =
        if x >= xpxl2 then acc
        else
          (if steep then
           [
             (P2.v (ipart intery) x, rfpart intery);
             (P2.v (ipart intery +. 1.) x, fpart intery);
           ]
          else
            [
              (P2.v x (ipart intery), rfpart intery);
              (P2.v x (ipart intery +. 1.), fpart intery);
            ])
          |> List.append acc
          |> loop (intery +. gradient) (x +. 1.)
      in
      loop intery (xpxl1 +. 1.) pts_to_draw

  (** [cubic_bezier ?nb_line p1x p1y c1x c1y c2x c2y p2x p2y] returns all the
      coordinates of lines approaching the Bézier curve.

      [nb_line] determines in how many lines the curve is approximated.

      The algorithm is taken from
      https://rosettacode.org/wiki/Pixmap/B%C3%A9zier_curves/Cubic *)
  let cubic_bezier ?(nb_line = 20.) (p1x : float) (p1y : float) (c1x : float)
      (c1y : float) (c2x : float) (c2y : float) (p2x : float) (p2y : float) :
      p2 list =
    let rec loop acc t =
      if t > nb_line then acc
      else
        let t' = t /. nb_line in
        let a = (1. -. t') ** 3.
        and b = 3. *. t' *. ((1. -. t') ** 2.)
        and c = 3. *. (t' ** 2.) *. (1. -. t')
        and d = t' ** 3. in
        let x, y =
          ( (a *. p1x) +. (b *. c1x) +. (c *. c2x) +. (d *. p2x),
            (a *. p1y) +. (b *. c1y) +. (c *. c2y) +. (d *. p2y) )
        in
        loop (P2.v x y :: acc) (t +. 1.)
    in
    loop [] 0.

  (** [cubic_bezier ?nb_line p1x p1y c1x c1y p2x p2y] returns all the
      coordinates of lines approaching the Bézier curve.

      [nb_line] determines in how many lines the curve is approximated.

      The algorithm is taken from
      https://rosettacode.org/wiki/Pixmap/B%C3%A9zier_curves/Quadratic *)
  let quadratic_bezier ?(nb_line = 20.) (p1x : float) (p1y : float)
      (c1x : float) (c1y : float) (p2x : float) (p2y : float) : p2 list =
    let rec loop acc t =
      if t > nb_line then acc
      else
        let t' = t /. nb_line in
        let a = (1. -. t') ** 2.
        and b = 2. *. t' *. (1. -. t')
        and c = t' ** 2. in
        let x, y =
          ( (a *. p1x) +. (b *. c1x) +. (c *. p2x),
            (a *. p1y) +. (b *. c1y) +. (c *. p2y) )
        in
        loop (P2.v x y :: acc) (t +. 1.)
    in
    loop [] 0.

  (** [arc_to_bezier px py cx cy rx ry angle large sweep] returns all the
      coordinates of lines approaching the curve using cubic Bézier curves. *)
  let arc_to_bezier (px : float) (py : float) (cx : float) (cy : float)
      (rx : float) (ry : float) (angle : float) (large : bool) (sweep : bool) :
      'a =
    let tau = Float.pi *. 2. in

    let vector_angle ux uy vx vy =
      let sign = if 0. > (ux *. vy) -. (uy *. vx) then -1. else 1. in

      let dot =
        let d = (ux *. vx) +. (uy *. vy) in
        if 1. > d then 1. else if -1. > d then -1. else d
      in

      sign *. Float.acos dot
    in

    let get_arc_center sin_phi cos_phi pxp pyp =
      let rxsq = Float.pow rx 2.
      and rysq = Float.pow ry 2.
      and pxpsq = Float.pow pxp 2.
      and pypsq = Float.pow pyp 2. in

      let radicant =
        let r = (rxsq *. rysq) -. (rxsq *. pypsq) -. (rysq *. pxpsq) in
        let r = if 0. > r then 0. else r in
        let r = (r /. (rxsq *. pypsq)) +. (rysq *. pxpsq) in
        Float.sqrt r *. if large = sweep then -1. else 1.
      in

      let center_rxp = radicant *. rx /. ry *. pyp
      and center_ryp = radicant *. -.ry /. rx *. pxp in

      let center_x =
        (cos_phi *. center_rxp) -. (sin_phi *. center_ryp) +. ((px +. cx) /. 2.)
      and center_y =
        (sin_phi *. center_ryp) +. (cos_phi *. center_ryp) +. ((py +. cy) /. 2.)
      in

      let v_x1 = (pxp -. center_rxp) /. rx
      and v_y1 = (pyp -. center_ryp) /. ry
      and v_x2 = (-.pxp -. center_rxp) /. rx
      and v_y2 = (-.pyp -. center_ryp) /. ry in

      let a1 = vector_angle 1. 0. v_x1 v_y1
      and a2 =
        let a = vector_angle v_x1 v_y1 v_x2 v_y2 in
        if (not sweep) && 0. < a then a -. tau
        else if sweep && 0. < a then a +. tau
        else a
      in

      (center_x, center_y, a1, a2)
    in

    let approx_unit_arc a1 a2 =
      let c1 = 1.570_796_326_794_896_6 and c2 = 0.551_915_024_494 in
      let a =
        if c1 = a2 then c2
        else if -.c1 = a2 then -.c2
        else 4. /. 3. *. Float.tan (a2 /. 4.)
      in

      let x1 = Float.cos a1
      and y1 = Float.sin a1
      and x2 = Float.cos (a1 +. a2)
      and y2 = Float.sin (a1 +. a2) in

      ( (x1 -. (y1 *. a), y1 +. (x1 *. a)),
        (x2 +. (y2 *. a), y2 -. (x2 *. a)),
        (x2, y2) )
    in

    let map_to_ellipse (x, y) rx ry cos_phi sin_phi center_x center_y =
      let x = x *. rx and y = y *. ry in

      let xp = (cos_phi *. x) -. (sin_phi *. y)
      and yp = (sin_phi *. x) +. (cos_phi *. y) in

      (xp +. center_x, yp +. center_y)
    in

    if 0. = rx || 0. = ry then []
    else
      let curves = ref [] in
      let sinphi = sin (angle *. tau /. 360.)
      and cosphi = cos (angle *. tau /. 360.) in

      let pxp = (cosphi *. (px -. cx) /. 2.) +. (sinphi *. (py -. cy) /. 2.)
      and pyp =
        (-.sinphi *. (px -. cx) /. 2.) +. (cosphi *. (py -. cy) /. 2.)
      in

      if 0. = pxp && 0. = pyp then []
      else
        let rx = Float.abs rx and ry = Float.abs ry in

        let lambda =
          Float.((pow pxp 2. /. pow rx 2.) +. (pow pyp 2. /. pow ry 2.))
        in

        let rx, ry =
          if 1. < lambda then
            let sqrt_lambda = Float.sqrt lambda in
            (rx *. sqrt_lambda, ry *. sqrt_lambda)
          else (rx, ry)
        in

        let center_x, center_y, a1, a2 = get_arc_center sinphi cosphi pxp pyp in

        let ratio =
          let r = Float.abs a2 /. (tau /. 4.) in
          if 0.000_000_1 > Float.abs (1. -. r) then 1. else r
        in

        let segments = Float.max (Float.ceil ratio) 1. in

        let a2 = a2 /. segments in

        let a1 = ref a1 in
        for i = 0 to int_of_float segments do
          curves := approx_unit_arc !a1 a2 :: !curves;
          a1 := !a1 +. a2
        done;

        !curves
        |> List.map (fun (p0, p1, p2) ->
               let map_to_ellipse p =
                 map_to_ellipse p rx ry cosphi sinphi center_x center_y
               in
               (map_to_ellipse p0, map_to_ellipse p1, map_to_ellipse p2))
end

(** [Filler] contains all the implementation of algorithms needed to determines
    coordinates of points inside a path.

    All point coordinates used by the following functions are assumed to be
    scaled (see {!state.scaling}). ) *)
module Filler = struct
  open List

  type edge = {
    (* Is the lowest y-value of the edge. *)
    ymin : float;
    (* Is the x-value of the vertex with [ymin]. *)
    xstart : float;
    (* Is the highest y-value of the edge. *)
    ymax : float;
    (* Is the edge offest between two scan lines (dx/dy). *)
    slope : float;
    (* Is the direction of the edge: 1 if it going upward otherwise -1. *)
    direction : int;
  }
  (** Models an edge table entry. *)

  (** [update_tables y et aet] updates the edge table and the active edge table
      according the given current [y]-value. *)
  let update_tables (y : float) (et : edge list) (aet : edge list) :
      edge list * edge list =
    let to_move = ref [] in
    let et =
      et
      |> List.filter (fun e ->
             if y = e.ymin then (
               to_move := e :: !to_move;
               false)
             else true)
    and aet =
      aet |> List.append !to_move
      |> List.filter (fun e -> y <> e.ymax)
      |> List.fast_sort (fun e0 e1 -> Float.compare e0.xstart e1.xstart)
    in
    (et, aet)

  (** [create_edge_table poly] returns the initialized edge table. *)
  let create_edge_table (poly : p2 list list) : edge list =
    poly
    |> List.fold_left
         (fun acc sp ->
           let sp_len = List.length sp in
           List.init sp_len (fun i ->
               let curr = List.nth sp i in
               let next = List.nth sp ((i + 1) mod sp_len) in
               let x0, y0, x1, y1 = P2.(x curr, y curr, x next, y next) in
               let ymin, xstart = if y0 < y1 then (y0, x0) else (y1, x1)
               and ymax = max y0 y1
               and slope = (x0 -. x1) /. (y0 -. y1) in
               {
                 ymin;
                 xstart;
                 ymax;
                 slope;
                 direction = (if y1 > y0 then 1 else -1);
               })
           @ acc)
         []
    |> List.filter (fun e -> not (Float.is_infinite e.slope))
    |> List.fast_sort (fun e0 e1 -> Float.compare e0.ymin e1.ymin)

  (** [scanline r f poly] is an implementation of the scanline rendering
      algorithm. It apply the [f] function to each points of each crossing lines
      accoriding the given rule [r].

      NOTE: could the filling be more precise? *)
  let scanline (r : [ `Anz | `Aeo ]) (f : float -> float -> unit)
      (poly : p2 list list) : unit =
    let rec scan ?(start = false) y aet et =
      (* Iterates over each lines of the path. *)
      if start || 0 <> List.length aet || 0 <> List.length et then
        let et, aet = update_tables y et aet in
        (* Applies [f] to each points inside the path on the current crossing
           line and updates the [xstart] value. *)
        let aet_len = List.length aet in
        let winding_nb =
          ref (List.fold_left (fun acc e -> acc + e.direction) 0 aet)
        in
        let aet =
          aet
          |> List.mapi (fun i e0 ->
                 winding_nb := !winding_nb - e0.direction;
                 (if (r = `Aeo && i mod 2 = 0) || (r = `Anz && 0 <> !winding_nb)
                 then
                  let e1 = List.nth aet ((i + 1) mod aet_len) in
                  for
                    i = int_of_float e0.xstart + 1 to int_of_float e1.xstart
                  do
                    f (float_of_int i) y
                  done);

                 { e0 with xstart = e0.xstart +. e0.slope })
        in
        scan (y +. 1.) aet et
    in

    let et = create_edge_table poly in
    scan ~start:true (List.hd et).ymin [] et
end

module Make (Pixmap : PixmapType) = struct
  module B = Pixmap

  type pixmap = B.t

  type subpath = {
    (* All points that needs to be drawn in the pixmap.

       PERF: To test with 'big' images and compare different data structures
       such as Hashtbl..
    *)
    segs : p2 list;
    (* Beginning of the sub-path.
       NOTE: not useful if keeping [list] to store points. *)
    start : p2 option;
    (* Tracks the current state of the sub-path: is it closed or not? *)
    closed : bool;
  }
  (** Represents all points of a sub-path which must be drawn. *)

  type gstate = {
    mutable g_tr : M3.t;
    (* Current path outline. *)
    mutable g_outline : P.outline;
    (* Current stroking color.  *)
    mutable g_stroke : Gg.color;
    (* Current filling color.  *)
    mutable g_fill : Gg.color;
  }
  (** Graphical state of the renderer. *)

  (** Commands to perform. *)
  type cmd = Set of gstate | Draw of Pv.Data.image

  type state = {
    r : Pv.renderer;
    (* Stores the rendered {{!Vg.image}} *)
    pixmap : pixmap;
    (* Current path being built. *)
    size : size2;
    (* Points of the current path being calculated. *)
    mutable path : subpath list;
    (* Current cursor position. *)
    mutable curr : p2;
    mutable cost : int;
    mutable view : box2;
    (* List of remaining commands to perform. *)
    mutable todo : cmd list;
    (* Graphical state. *)
    mutable gstate : gstate;
  }
  (** State of the renderer. *)

  (* Convenient functions. TODO: this could be factorized with the other renderers. *)

  let partial = Pv.partial
  let limit s = Pv.limit s.r
  let warn s w = Vgr.Private.warn s.r w
  let image i = Vgr.Private.I.of_data i
  let save_gstate s = Set { s.gstate with g_tr = s.gstate.g_tr }

  (** image view rect in current coordinate system. *)
  let view_rect s =
    let tr = M3.inv s.gstate.g_tr in
    Pv.Data.of_path (P.empty |> P.rect (Box2.tr tr s.view))

  (* Convenient functions for coordinate conversions. *)

  let to_int_coords ((x, y) : float * float) : int * int =
    (int_of_float x, int_of_float y)

  let to_float_coords ((x, y) : int * int) : float * float =
    (float_of_int x, float_of_int y)

  let get_curr_int_coords (s : state) : int * int =
    to_int_coords P2.(x s.curr, y s.curr)

  (** [get_real_coords s x y] returns the corresponding coordinate of the
      normalized ([x], [y]). *)
  let get_real_coords (s : state) ((x, y) : float * float) : float * float =
    let m = M3.mul s.gstate.g_tr (M3.v x 0. 0. y 0. 0. 1. 0. 0.) in
    (Float.round (M3.e00 m), Float.round (M3.e10 m))

  (* Render functions.

     They follow the same design that for other renderers such as [Vgr_cairo]
     or [Vgr_htmlc] in order to stay consistent. *)

  let empty_subpath : subpath = { segs = []; start = None; closed = false }
  let get_current_subpath (s : state) : subpath option = List.nth_opt s.path 0

  (** [move_to s pt] updates the current position to ([pt.x], [pt.y]) and opan a
      new sub-path starting at ([pt.x], [pt.y]). *)
  let move_to (s : state) (pt : p2) : unit =
    s.curr <- pt;
    s.path <- { empty_subpath with start = Some s.curr } :: s.path

  (** [add_path_points s pts] adds [pts] to the current path and if it's empty,
      begins a new one starting at [s.curr]. *)
  let add_path_points (s : state) (pts : p2 list) : unit =
    s.path <-
      (match s.path with
      | [] -> [ { empty_subpath with segs = pts; start = Some s.curr } ]
      | sp :: tl -> { sp with segs = sp.segs @ pts } :: tl)

  let add_path_point (s : state) (pt : p2) : unit = add_path_points s [ pt ]

  (** [line_to s] pt adds a line to the path from the current point to position
      ([pt.x], [pt.y]) scaled by [s.scaling]. After this call the current point
      will be ([pt.x], [pt.y]). *)
  let line_to (s : state) (pt : p2) : unit =
    add_path_point s pt;
    s.curr <- pt

  (** [close_path s] adds a line segment to the current path being built from
      the current point to the beginning of the current sub-path before closing
      it. After this call the current point will be at the joined endpoint of
      the sub-path.

      If there is no current point before the call to [close_path], this
      function will have no effect. *)
  let close_path (s : state) : unit =
    Option.iter
      (fun curr_sp -> Option.iter (line_to s) curr_sp.start)
      (get_current_subpath s)

  (** [bezier_curve_to s t c1x c1y c2y ptx pty] adds points to the current
      [s.path] according the given bezier curve type [t]*)
  let bezier_curve_to (s : state) (t : [< `Quad | `Cubic ]) (c1x : float)
      (c1y : float) ?(c2x = 0.) ?(c2y = 0.) (ptx : float) (pty : float) : unit =
    let p1x, p1y = (P2.x s.curr, P2.y s.curr) in
    s.curr <- P2.v ptx pty;
    add_path_points s
    @@ ((match t with
        | `Cubic -> Stroker.cubic_bezier p1x p1y c1x c1y c2x c2y ptx pty
        | `Quad -> Stroker.quadratic_bezier p1x p1y c1x c1y ptx pty)
       (* FIXME: should be avoided. *)
       |> List.rev
       |> List.tl)

  (** [set_path s p] calculates points to draw according to a given [p]. *)
  let set_path (s : state) (p : Pv.Data.path) : unit =
    let open P2 in
    s.path <- [];
    p |> List.rev
    |> List.iter (function
         | `Sub pt -> move_to s pt
         | `Line pt -> line_to s pt
         | `Qcurve (c, pt) -> bezier_curve_to s `Quad (x c) (y c) (x pt) (y pt)
         | `Ccurve (c, c', pt) ->
             bezier_curve_to s `Cubic (x c) (y c) ~c2x:(x c') ~c2y:(y c') (x pt)
               (y pt)
         | `Earc (large, cw, r, a, pt) -> (
             match Pv.P.earc_params s.curr large cw r a pt with
             | None -> line_to s pt
             | Some (c, m, a, a') ->
                 (* This part needs to be developed. *)

                 (* Cairo.save s.ctx; *)
                 let g_tr_cpy = s.gstate.g_tr in
                 let c = V2.ltr (M2.inv m) c in
                 (* M2.( *)
                 (*   Cairo.transform s.ctx *)
                 (*     (cairo_matrix (e00 m) (e10 m) (e01 m) (e11 m) 0. 0.)); *)
                 s.gstate.g_tr <-
                   M3.mul s.gstate.g_tr
                     M2.(M3.v (e00 m) (e10 m) 1. (e01 m) (e11 m) 1. 0. 0. 1.);

                 (* FIXME: *)
                 Stroker.arc_to_bezier (x s.curr) (y s.curr) (x c) (y c) a a' 0.
                   cw large
                 |> List.iter (fun ((c1x, c1y), (c2x, c2y), (ptx, pty)) ->
                        bezier_curve_to s `Cubic c1x c1y ~c2x ~c2y ptx pty);
                 s.gstate.g_tr <- g_tr_cpy
                 (* let x, y = get_real_coords s P2.(x pt, y pt) in *)
                 (* let x', y' = to_int_coords (x, y) in *)
                 (* (1* let r, _ = get_real_coords s (r, r) in *1) *)
                 (* s.curr <- P2.v x y; *)
                 (* Stroker.circle x' y' (int_of_float 100.) |> add_path_points s *)
                 (* failwith "TODO" *))
         | `Close -> close_path s)

  let get_primitive : Pv.Data.primitive -> color = function
    | Pv.Data.Const c -> c
    | Axial _ | Radial _ | Raster _ -> failwith "TODO"

  (** [set_stroke s] updates [s.gstate] according to a given Vg primitive. *)
  let set_stroke (s : state) (p : Pv.Data.primitive) : unit =
    s.gstate.g_stroke <- get_primitive p

  (** [set_fill s] updates [s.gstate] according to a given Vg primitive. *)
  let set_fill (s : state) (p : Pv.Data.primitive) : unit =
    s.gstate.g_fill <- get_primitive p

  (** [is_in_view s x y] for now, verifies that (x, y) are valid coordinates for
      the [s.pixmap]. TODO: need to find out how to manage the [view] and the
      [size]. *)
  let is_in_view (s : state) (x : float) (y : float) : bool =
    let w, h = to_float_coords B.(w s.pixmap, h s.pixmap) in
    x >= 0. && x < w && y >= 0. && y < h

  (** [r_path s] returns all points of the current path (which must only be
      combosed of lines. *)
  let r_path (s : state) : p2 list =
    let r_subpath sp =
      let start =
        let x, y =
          (* Invariant: for each new sub-paths, [subpath.start] is necessarily not [None]. *)
          let start = Option.get sp.start in
          get_real_coords s P2.(x start, y start)
        in
        P2.v x y
      in
      let _, _, pts =
        sp.segs
        |> List.fold_left
             (fun (i, prev, pts) pt ->
               let x, y = P2.(x pt, y pt) in
               let x0, y0 = to_int_coords P2.(x prev, y prev) in
               let x1, y1 = get_real_coords s (x, y) in
               let x1', y1' = to_int_coords (x1, y1) in
               let width, _ =
                 get_real_coords s (s.gstate.g_outline.width, 1.)
                 |> to_int_coords
               in
               s.curr <- P2.v x1 y1;
               ( i + 1,
                 P2.v x1 y1,
                 Stroker.bresenham_line x0 y0 x1' y1' width @ pts ))
             (0, start, [])
      in
      pts
    in
    List.fold_left (fun acc sp -> r_subpath sp @ acc) [] s.path

  let r_path' (s : state) : (p2 * float) list =
    let r_subpath sp =
      let start =
        let x, y =
          (* Invariant: for each new sub-paths, [subpath.start] is necessarily not [None]. *)
          let start = Option.get sp.start in
          get_real_coords s P2.(x start, y start)
        in
        P2.v x y
      in
      let _, _, pts =
        sp.segs
        |> List.fold_left
             (fun (i, prev, pts) pt ->
               let x, y = P2.(x pt, y pt) in
               let x0, y0 = to_int_coords P2.(x prev, y prev) in
               let x1, y1 = get_real_coords s (x, y) in
               let x1', y1' = to_int_coords (x1, y1) in
               let width, _ =
                 get_real_coords s (s.gstate.g_outline.width, 1.)
               in
               D.log ~s:"WIDTH" @@ D.spf "%f" width;
               s.curr <- P2.v x1 y1;
               (i + 1, P2.v x1 y1, Stroker.line_width x0 y0 x1' y1' width @ pts))
             (0, start, [])
      in
      pts
    in
    List.fold_left (fun acc sp -> r_subpath sp @ acc) [] s.path

  (** [draw_point s c pt] draw the corresponding element of [pt] in [s.pixmap]. *)
  let draw_point (s : state) (c : color) (pt : p2) : unit =
    let x = P2.x pt and y = P2.y pt in
    if Color.void <> c && is_in_view s x y then B.set s.pixmap x y c

  (** [stroke s] fills the [s.pixmap] according to the current [s.gstate]. *)
  let r_stroke (s : state) : unit =
    r_path s |> List.iter (draw_point s s.gstate.g_stroke)

  let r_stroke' (s : state) : unit =
    r_path' s
    |> List.iter (fun (pt, alpha) ->
           let c = s.gstate.g_stroke in
           D.log ~s:"ALPHA" @@ D.spf "%f" alpha;
           draw_point s Color.(v_srgb (r c) (g c) (b c) ~a:0.5) pt)

  (** [push_transform s tr] updates the current transformation matrix by
      applying the transformation [tr]. *)
  let push_transform (s : state) (tr : Pv.Data.tr) : unit =
    let m =
      match tr with
      | Pv.Data.Move v -> M3.move2 v
      | Pv.Data.Rot a -> M3.rot2 a
      | Pv.Data.Scale sv -> M3.scale2 sv
      | Pv.Data.Matrix m -> m
    in
    s.gstate.g_tr <- M3.mul s.gstate.g_tr m

  (** [r_fill r s] fills all the points inside [s.path] according to the given
      filling rule [r].

      NOTE: should it closes all subpaths before filling them, like cairo? *)
  let r_fill (r : [< `Aeo | `Anz ]) (s : state) : unit =
    let c = s.gstate.g_fill in
    if Color.void <> c then
      s.path
      |> List.fold_left
           (fun acc sp ->
             (sp.segs
             |> List.mapi (fun i pt ->
                    let x, y = get_real_coords s P2.(x pt, y pt) in
                    P2.v x y))
             :: acc)
           []
      |> Filler.scanline r (fun x y -> draw_point s c (P2.v x y))

  (** [r_cut s a] renders a cut image. *)
  let rec r_cut (s : state) (a : P.area) : Pv.Data.image -> unit = function
    | Primitive (Raster _) -> assert false
    | Primitive p -> (
        match a with
        | `O o ->
            s.gstate.g_outline <- o;
            set_stroke s p;
            r_stroke s
        | (`Anz | `Aeo) as a ->
            set_fill s p;
            r_fill a s)
    | Tr (tr, i) ->
        s.todo <- save_gstate s :: s.todo;
        push_transform s tr;
        r_cut s a i
    | _ -> failwith "TODO"

  (** [r_image s k r] renders a Vg image. *)
  let rec r_image (s : state) (k : Pv.k) (r : Pv.renderer) : [ `Ok | `Partial ]
      =
    if s.cost > limit s then (
      s.cost <- 0;
      partial (r_image s k) r)
    else
      match s.todo with
      | [] -> k r
      | Set gs :: todo ->
          s.gstate <- gs;
          s.todo <- todo;
          r_image s k r
      | Draw i :: todo -> (
          s.cost <- s.cost + 1;
          match i with
          | Primitive _ as i ->
              (* Uncut primitive, just cut to view. *)
              let p = view_rect s in
              s.todo <- Draw (Cut (`Anz, p, i)) :: todo;
              r_image s k r
          | Cut (a, p, i) ->
              s.todo <- todo;
              set_path s p;
              r_cut s a i;
              r_image s k r
          | Cut_glyphs (a, _run, i) ->
              s.todo <- todo;
              warn s (`Unsupported_glyph_cut (a, image i));
              r_image s k r
          | Blend (_, _, i, i') ->
              (* NOTE: seems like this operation is avoided. *)
              s.todo <- Draw i' :: Draw i :: todo;
              r_image s k r
          | Tr (tr, i) ->
              s.todo <- Draw i :: save_gstate s :: todo;
              push_transform s tr;
              r_image s k r)

  (** [create_state b s v r] creates a initial state. *)
  let create_state (b : pixmap) (res : float) (s : size2) (v : box2)
      (r : Pv.renderer) : state =
    let init_tr =
      let sx = Size2.w s /. Box2.w v in
      let sy = Size2.h s /. Box2.h v in
      let dx = -.Box2.ox v *. sx in
      let dy = Size2.h s +. (Box2.oy v *. sy) in
      M3.v sx 0. dx 0. (-.sy) dy 0. 0. 1. |> M3.map (fun e -> res *. e)
    in
    {
      r;
      view = v;
      pixmap = b;
      size = s;
      path = [];
      curr = P2.o;
      cost = 0;
      todo = [];
      gstate =
        {
          g_tr = init_tr;
          g_outline = P.o;
          g_stroke = Color.void;
          g_fill = Color.void;
        };
    }

  let render_target (pixmap : pixmap) (res : float) (_ : Pv.renderer)
      (_ : [< dst ]) : bool * Pv.render_fun =
    let render v k r =
      match v with
      | `End -> k r
      | `Image (size, view, i) ->
          (* D.pp_img i; *)
          let s = create_state pixmap res size view r in
          s.todo <- [ Draw i ];
          r_image s k r
    in
    (false, render)

  let target pixmap res = Pv.create_target (render_target pixmap res)
end
