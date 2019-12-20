(** Formatter *)


module E = Engine
module D = Defs
module Ft =Core
module F = Interpolation
module Sem = Tagsem

module NL = Ft.Nlist

type open_tag =
    Open_tag : {tag: 'any D.tag; with_box: bool } -> open_tag

type ('a,'b) t = ('a,'b) Ft.t

let with_sem f ?(geometry=Geometry.default) ?(tags=[Sem.box]) x: ('a,Ft.z) t =
  { Ft.tag_semantic=tags; geometry; open_tags = NL.[];
    layout_engine = E.start geometry ( f x ) }

let chan ?geometry = with_sem (new Raw.chan) ?geometry
let buffer ?geometry = with_sem (new Raw.buffer) ?geometry

let stdout = chan stdout
let stderr = chan stderr


type tag_name= Name: 'any D.tag -> tag_name


exception No_open_tag
type exn += Mismatched_close: {expected:'any Defs.tag; got:'other Defs.tag}
  -> exn

let lift f s (ppf: _ t) =
  { ppf with layout_engine = f s ppf.layout_engine }

let string x = lift E.string x
let open_box x = lift E.open_box x
let break x = lift E.break x
let full_break x  = lift E.full_break x
let close_box ppf = lift E.close_box () ppf
let flush (ppf: _ t) = E.flush ppf.layout_engine

let open_tag x (ppf: _ t) =
  { ppf with open_tags = NL.( x :: ppf.open_tags ) }
