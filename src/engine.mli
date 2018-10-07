type +'a t
val start: Geometry.t -> 'a Raw.t -> 'a t

type ('a,'b) prim = 'a -> 'b t -> 'b t
val string: (string,_) prim
val open_box: (Defs.box,_) prim
val close_box: (unit,_) prim
val break: (Defs.break_data,_) prim
val full_break: (int,_) prim
val column_switch: (unit,_) prim
val flush: 'a t -> 'a
