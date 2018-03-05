type +'a t
val start: 'a Raw.t -> 'a t

type ('a,'b) prim = Geometry.t -> 'a -> 'b t -> 'b t
val string: (string,_) prim
val open_box: (Format.box,_) prim
val close_box: (unit,_) prim
val break: (Format.break_data,_) prim
val full_break: (int,_) prim
val flush: 'a t -> 'a
