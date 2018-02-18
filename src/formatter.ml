(** Formatter *)


module E = Engine
module Sem = Tagsem

type t = {
  geometry: Geometry.t;
  phy: Raw.t;
  tag_semantic: t Sem.t list;
  open_tags: Sem.open_tag list;
  metadata: E.t
}

let with_sem f ?(geometry=Geometry.default) ?(tags=[Sem.box]) x =
  { phy = f x; tag_semantic=tags; geometry; open_tags = []; metadata = E.start }

let chan ?geometry = with_sem Raw.chan ?geometry
let buffer ?geometry = with_sem Raw.buffer ?geometry

let stdout = chan Pervasives.stdout
let stderr = chan Pervasives.stderr


type tag_name= Name: 'any Format.tag -> tag_name


let lift f s ppf =
  { ppf with metadata = f ppf.geometry ppf.phy s ppf.metadata }

let string = lift E.string
let open_box = lift E.open_box
let break = lift E.break
let full_break = lift E.full_break
let close_box ppf = lift E.close_box () ppf

exception No_open_tag
type exn += Mismatched_close: {expected:'any Format.tag; got:'other Format.tag}
  -> exn


let rec eval:
  type free b right.
  (free,b * right,t) Format.format
  -> (free,right) Format.iargs
  -> t -> t  =
  let open Format in
  fun fmt iargs ppf -> match fmt with
    | [] -> ppf
    | Literal s :: q  -> eval q iargs (string s ppf)
    | Captured (k, f):: q ->
      let ppf = f iargs ppf in
      let iargs = Format.take k iargs in
      eval q iargs ppf
    | Open_tag (tag,data) :: q ->
      begin match Sem.find_sem tag ppf.tag_semantic with
        | None -> ppf
        | Some (Sem.T sem, rest) ->
          let with_box, open_box =
            match sem.box sem.data tag data with
            | None -> false, (fun x -> x)
            | Some b -> true, open_box b in
          let open_tags: _ list =
            Sem.Open_tag { with_box; tag } :: ppf.open_tags in
          let sdata, p = sem.open_printer sem.data tag data in
          let tag_semantic: _ list = Sem.T { sem with data = sdata } :: rest in
          let ppf = ppf |> open_box |> p in
          eval q iargs { ppf with open_tags; tag_semantic }
      end
    | Close_any_tag :: q ->
      begin match ppf.open_tags with
        | [] -> raise No_open_tag
        | Sem.Open_tag r :: tags -> close_tag r.with_box r.tag tags q iargs ppf
      end
    | Close_tag ctag :: q ->
      begin match ppf.open_tags with
        | [] -> raise No_open_tag
        | Sem.Open_tag {tag; _ } :: _ when Name tag <> Name ctag ->
          raise (Mismatched_close {expected=tag;got=ctag})
        | Sem.Open_tag {tag; with_box} :: tags ->
          close_tag with_box tag tags q iargs ppf
      end
    | Point_tag (tag, tdata) :: q ->
      let ppf =
      begin match Sem.find_sem tag ppf.tag_semantic with
        | None -> ppf
        | Some (Sem.T sem,rest) ->
          let data, p = sem.open_printer sem.data tag tdata in
          let ppf = p ppf in
          let ppf = begin match sem.break data tag tdata with
            | None -> ppf
            | Some Break {space; indent} ->
              break {space;indent} ppf
            | Some Full_break b ->
              full_break b ppf
          end in
          let data, p = sem.close_printer data  in
          let ppf = p ppf in
          { ppf with tag_semantic = T { sem with data } :: rest }
      end in
      eval q iargs ppf

and close_tag: type any free b right.
  bool -> any Format.tag -> Sem.open_tag list
  -> (free,b * right,t) Format.format
  -> (free,right) Format.iargs
  -> t -> t  = fun with_box tag open_tags q iargs ppf ->
  let open Format in
  match Sem.find_sem tag ppf.tag_semantic with
  | None -> ppf
  | Some (Sem.T sem, rest) ->
    let data, p =  sem.close_printer sem.data in
    (*  Printf.eprintf "Eval start %a\n%!" E.pp_pos ppf;*)
    let ppf = p ppf in
    let ppf = if with_box then close_box ppf else ppf in
    (* Printf.eprintf "Eval %a\n%!" E.pp_pos ppf;*)
    let tag_semantic: _ list = Sem.T { sem with data } :: rest in
    eval q iargs { ppf with open_tags; tag_semantic}

let eval ppf fmt args = eval fmt (Format.make args) ppf
