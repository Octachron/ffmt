(** Definition of formatter and tag semantic *)

type 'any tag = 'any Format.tag
module E = Engine
type open_tag = Open_tag : {tag: 'any Format.tag; with_box: bool } -> open_tag


type 'a t = {
  geometry: Geometry.t;
  tag_semantic: sem list;
  open_tags: open_tag list;
  metadata: 'a E.t
}
and sem = <
  mine:'any. 'any tag -> bool;
  box: 'any. 'any tag -> 'any -> Format.box option;
  break: 'any. 'any tag -> 'any -> Format.break option;
  open_printer: 'any 'a. 'any tag -> 'any -> 'self * 'a t Format.captured;
  close_printer: 'a. 'self * 'a t Format.captured;
> as 'self

class type semclass = object('self)
 method mine:'any. 'any tag -> bool
 method box: 'any. 'any tag -> 'any -> Format.box option
 method break: 'any. 'any tag -> 'any -> Format.break option
 method open_printer: 'any 'a. 'any tag -> 'any -> 'self * 'a t Format.captured
 method close_printer: 'a. 'self * 'a t Format.captured
end
