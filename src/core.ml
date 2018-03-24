(** Definition of formatter and tag semantic *)

type 'any tag = 'any Defs.tag
module E = Engine
type open_tag = Open_tag : {tag: 'any tag; with_box: bool } -> open_tag

type 'a endo = 'a -> 'a

type z = private Z
type 'a s = private Succ

module Nlist = struct
  type ('a,'b) t =
    | [] : (z,'b) t
    | (::): 'a * ('n,'a) t -> ('n s, 'a) t
end

type sem = <
  mine:'any. 'any tag -> bool;
  box: 'any. 'any tag -> 'any -> Defs.box option;
  break: 'any. 'any tag -> 'any -> Defs.break option;
  open_printer: 'any 'a 'b. 'any tag -> 'any ->
    'self *
    (('a,'b) t -> ('a,'b) t);
  close_printer: 'a 'b. 'self * (('a,'b) t -> ('a,'b) t);
> as 'self

and ('a,'n) t = {
  geometry: Geometry.t;
  tag_semantic: sem list;
  open_tags: ('n, open_tag) Nlist.t;
  layout_engine: 'a E.t
}

class type semclass = object('self)
 method mine:'any. 'any tag -> bool
 method box: 'any. 'any tag -> 'any -> Defs.box option
 method break: 'any. 'any tag -> 'any -> Defs.break option
 method open_printer: 'any 'a 'b. 'any tag -> 'any ->
   'self * ('a,'b) t endo
 method close_printer: 'a 'b. 'self *  ('a,'b) t endo
end

type (_,_) index =
  | Z: ('elt, 'elt -> _ ) index
  | S: ('elt, 'list) index -> ('elt, 'any -> 'list) index
