type t
val start: Raw.t -> t

type 'a prim = Geometry.t -> 'a -> t -> t
val string: string prim
val open_box: Format.box prim
val close_box: unit prim
val break: Format.break_data prim
val full_break: int prim
