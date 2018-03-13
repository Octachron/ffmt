(** Definition of formatter and tag semantic *)

type 'any tag = 'any Defs.tag
module E = Engine
type open_tag = Open_tag : {tag: 'any tag; with_box: bool } -> open_tag

type 'a endo = 'a -> 'a

type z = private Z
type 'a s = private Succ

module Inner: sig

  type (+'a,+'b) t

  type sem = <
    mine:'any. 'any tag -> bool;
    box: 'any. 'any tag -> 'any -> Defs.box option;
    break: 'any. 'any tag -> 'any -> Defs.break option;
    open_printer: 'any 'a 'b. 'any tag -> 'any ->
      'self *
      (('a,'b) t -> ('a,'b) t);
    close_printer: 'a 'b. 'self * (('a,'b) t -> ('a,'b) t);
  > as 'self

  type 'a basic = {
    geometry: Geometry.t;
    tag_semantic: sem list;
    open_tags: open_tag list;
    metadata: 'a E.t
  }

  val open_tags: ('a,'b) t -> open_tag list
  val semantics: ('a,'b) t -> sem list


  val from_basic: 'a basic -> ('a,z) t
  val to_basic: ('a,'b) t -> 'a basic
  val string: string -> ('a,'b) t endo
  val break: Defs.break_data -> ('a,'b) t endo
  val full_break: int -> ('a,'b) t endo
  val open_box: Defs.box -> ('a,'b) t -> ('a,'b s) t
  val close_box: ('a,'b s) t -> ('a,'b) t
  val open_tag: ('a,'b) t -> ('a,'b s) t
  val close_tag: ('a,'b s) t -> ('a,'b) t
  val update: ?open_tags:open_tag list -> tag_semantic:sem list
    -> ('a,'b) t -> ('a,'b) t
  val flush: ('a,z) t -> 'a
end = struct
  type (+'a,+'b) t = 'a basic
  and sem = <
    mine:'any. 'any tag -> bool;
    box: 'any. 'any tag -> 'any -> Defs.box option;
    break: 'any. 'any tag -> 'any -> Defs.break option;
    open_printer: 'any 'a 'b. 'any tag -> 'any ->
      'self *
      (('a,'b) t -> ('a,'b) t);
    close_printer: 'a 'b. 'self * (('a,'b) t -> ('a,'b) t);
  > as 'self
  and 'a basic = {
  geometry: Geometry.t;
  tag_semantic: sem list;
  open_tags: open_tag list;
  metadata: 'a E.t
}

  let from_basic x = x
  let to_basic x = x

  let semantics x = x.tag_semantic
  let open_tags x = x.open_tags

  let lift f s ppf =
    { ppf with metadata = f ppf.geometry s ppf.metadata }

  let string x = lift E.string x
  let open_box x = lift E.open_box x
  let open_tag x = x
  let close_tag x = x
  let break x = lift E.break x
  let full_break x  = lift E.full_break x
  let close_box ppf = lift E.close_box () ppf
  let update ?open_tags ~tag_semantic ppf =
    match open_tags with
    | Some open_tags ->
      { ppf with open_tags; tag_semantic }
    | None -> { ppf with tag_semantic }
  let flush ppf = E.flush ppf.metadata
end
include Inner


class type semclass = object('self)
 method mine:'any. 'any tag -> bool
 method box: 'any. 'any tag -> 'any -> Defs.box option
 method break: 'any. 'any tag -> 'any -> Defs.break option
 method open_printer: 'any 'a 'b. 'any tag -> 'any ->
   'self * ('a,'b) t endo
 method close_printer: 'a 'b. 'self *  ('a,'b) t endo
end
