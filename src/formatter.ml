(** Formatter *)


module E = Engine
module D = Defs
module Ft = Formatter_def
module F = Interpolation
module Sem = Tagsem

module NL = Ft.Nlist

type open_tag = Open_tag : {tag: 'any D.tag; with_box: bool } -> open_tag

type ('a,'b) t = ('a,'b) Ft.t

let with_sem f ?(geometry=Geometry.default) ?(tags=[Sem.box]) x: ('a,Ft.z) t =
  { Ft.tag_semantic=tags; geometry; open_tags = NL.[];
    metadata = E.start ( f x ) }

let chan ?geometry = with_sem (new Raw.chan) ?geometry
let buffer ?geometry = with_sem (new Raw.buffer) ?geometry

let stdout = chan Pervasives.stdout
let stderr = chan Pervasives.stderr


type tag_name= Name: 'any D.tag -> tag_name


exception No_open_tag
type exn += Mismatched_close: {expected:'any Defs.tag; got:'other Defs.tag}
  -> exn

let lift f s (ppf: _ t) =
  { ppf with metadata = f ppf.geometry s ppf.metadata }

let string x = lift E.string x
let open_box x = lift E.open_box x
let break x = lift E.break x
let full_break x  = lift E.full_break x
let close_box ppf = lift E.close_box () ppf
let flush (ppf: _ t) = E.flush ppf.metadata

let open_tag x (ppf: _ t) =
  { ppf with open_tags = NL.( x :: ppf.open_tags ) }
