(** Vg pixmap renderer dependency free. *)

(** {1 Generic pixmap interface} *)

(** [PixmapType] is a generic interface allowing to use custom [pixmap]
    implementations. (For provided one, see {!providedtype}). *)
module type PixmapType = sig
  (** [t] is the type of the pixmap implementation used to store a rasterized
      {!Vg.image}*)
  type t

  val create : int -> int -> t
  (** [create w h] must return an initialized pixmap corresponding to an image
      with a width of [w] and a height of [h]. *)

  val get : t -> float -> float -> Gg.color
  (** [get pixmap x y] must return the color of the stored pixel with
      coordinates ([x], [y]). *)

  val set : t -> float -> float -> Gg.color -> unit
  (** [set pixmap x y c] must update the [pixmap] by associated the [c] color to
      the pixel with coordinates ([x], [y]). *)

  val w : t -> int
  (** [w pixmap] must return the width of the represented image by the [pixmap]. *)

  val h : t -> int
  (** [width pixmap] must return the height of the represented image by the
      [pixmap]. *)
end

(** {2:providedtype Provided pixmap implementations} *)

(** Provided {!PixmapType} implementation using a {!Gg.Ba.Float32} (linear
    {!Bigarray}). *)
module F32_ba : PixmapType

(** RGBa color channels of a pixel are stored in a row:

    {v
 0        1        2        3        4           (x*h+y)*c
 +--------+--------+--------+--------+--------+-------+--------+---
 | (0,0)  | (0,0)  | (0,0)  | (0,0)  | (0,1)  |  ...  | (x,y)  |
 |      r |      g |      b |      a |      r |       |      r |
 +--------+--------+--------+--------+--------+-------+--------+---
    v}

    where:

    - [w] is the image width
    - [h] is the image height
    - [c] is the number of color channel *)

(** {1:target Pixmap render targets} *)

module type S = sig
  type pixmap

  val target : pixmap -> float -> [ `Other ] Vg.Vgr.target
  (** [target state]. *)
end

(** Functor building an implementation of the pixmap given a {!PixmapType}. *)
module Make (Pixmap : PixmapType) : S with type pixmap = Pixmap.t
