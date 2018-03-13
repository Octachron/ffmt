(** Formatter *)


module E = Engine
module D = Defs
module Ft = Formatter_def
module F = Format
module Sem = Tagsem

type open_tag = Open_tag : {tag: 'any D.tag; with_box: bool } -> open_tag

type ('a,'b) t = ('a,'b) Ft.t

let with_sem f ?(geometry=Geometry.default) ?(tags=[Sem.box]) x: ('a,F.z) t =
  Ft.from_basic
  { Ft.tag_semantic=tags; geometry; open_tags = []; metadata = E.start ( f x ) }

let chan ?geometry = with_sem (new Raw.chan) ?geometry
let buffer ?geometry = with_sem (new Raw.buffer) ?geometry

let stdout = chan Pervasives.stdout
let stderr = chan Pervasives.stderr


type tag_name= Name: 'any D.tag -> tag_name


exception No_open_tag
type exn += Mismatched_close: {expected:'any Defs.tag; got:'other Defs.tag}
  -> exn


let rec eval:
  type free b right final before after.
  <all:free; right:right; tail:b; fmt:final; tag_count:before -> after >
    Format.format
  -> (free,right) Format.iargs
  -> (final,before) t -> (final,after) t  =
  let open Format in
  fun fmt iargs ppf -> match fmt with
    | [] -> ppf
    | Literal s :: q  -> eval q iargs (Ft.string s ppf)
    | Captured (k, f):: q ->
      let ppf = f iargs ppf in
      let iargs = Format.take k iargs in
      eval q iargs ppf
    | Open_tag (tag,data) :: q ->
      begin match Sem.find_sem tag (Ft.semantics ppf) with
        | None -> eval q iargs (Ft.open_tag ppf)
        | Some (sem, rest) ->
          let with_box, open_box =
            match sem#box tag data with
            | None -> false, Ft.open_tag
            | Some b -> true, Ft.open_box b in
          let open_tags: _ list =
            Sem.Open_tag { with_box; tag } :: (Ft.open_tags ppf) in
          let sem, p = sem#open_printer tag data in
          let tag_semantic: _ list = sem :: rest in
          let ppf = ppf |> open_box |> p in
          eval q iargs (Ft.update  ppf ~open_tags ~tag_semantic )
      end
    | Close_any_tag :: q ->
      begin match Ft.open_tags ppf with
        | [] -> raise No_open_tag
        | Sem.Open_tag r :: tags -> close_tag r.with_box r.tag tags q iargs ppf
      end
    | Close_tag ctag :: q ->
      begin match Ft.open_tags ppf with
        | [] -> raise No_open_tag
        | Sem.Open_tag {tag; _ } :: _ when Name tag <> Name ctag ->
          raise (Mismatched_close {expected=tag;got=ctag})
        | Sem.Open_tag {tag; with_box} :: tags ->
          close_tag with_box tag tags q iargs ppf
      end
    | Point_tag (tag, tdata) :: q ->
      let ppf =
      begin match Sem.find_sem tag (Ft.semantics ppf) with
        | None -> ppf
        | Some (sem,rest) ->
          let sem, p = sem#open_printer tag tdata in
          let ppf = p ppf in
          let ppf = begin match sem#break tag tdata with
            | None -> ppf
            | Some Break {space; indent} ->
              Ft.break {space;indent} ppf
            | Some Full_break b ->
              Ft.full_break b ppf
          end in
          let sem, p = sem#close_printer  in
          let ppf = p ppf in
          Ft.update ~tag_semantic:(sem ::rest) ppf
      end in
      eval q iargs ppf

and close_tag: type any n after free b right final.
  bool -> any Defs.tag -> Sem.open_tag list
  -> <all:free; right:right; tail:b; fmt:final;
      tag_count: n -> after > Format.format
  -> (free,right) Format.iargs
  -> (final,n F.s) t -> (final, after) t =
  fun with_box tag open_tags q iargs ppf ->
  match Sem.find_sem tag (Ft.semantics ppf) with
  | None -> eval q iargs (Ft.close_box ppf)
  | Some (sem, rest) ->
    let sem, p =  sem#close_printer in
    (*  Printf.eprintf "Eval start %a\n%!" E.pp_pos ppf;*)
    let ppf = p ppf in
    let ppf = if with_box then Ft.close_box ppf else Ft.close_tag ppf in
    (* Printf.eprintf "Eval %a\n%!" E.pp_pos ppf;*)
    let tag_semantic: _ list = sem :: rest in
    eval q iargs (Ft.update ppf ~open_tags ~tag_semantic)

let fprintf fmt args = eval fmt (Format.make args)

let flush fmt = Ft.flush fmt
let open_box, close_box, break, full_break, string = let open Ft in
  open_box, close_box, break, full_break, string
