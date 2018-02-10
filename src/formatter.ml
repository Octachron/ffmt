
module E = Reengine

let with_sem f = fun ?(geometry=Geometry.default) ?tag_semantic x ->
  let tag_semantic =
  match tag_semantic with
  | None -> Spec.init Semantics.box
  | Some x -> Spec.init x in
  let logical = { Spec.phy = f x; tag_semantic; geometry; open_tags = [] } in
  { E.logical; status = Direct {position=0;indent=0; kind=H }; context = [] }

let chan ?geometry = with_sem Spec.chan ?geometry
let buffer ?geometry = with_sem Spec.buffer ?geometry

let stdout = chan Pervasives.stdout
let stderr = chan Pervasives.stderr


type tag_name= Name: 'any Format.tag -> tag_name



let string = E.string
let int d = string (string_of_int d)
let float f = string (string_of_float f)

exception No_open_tag
type exn += Mismatched_close: {expected:'any Format.tag; got:'other Format.tag}
  -> exn


let rec eval:
  type free right.
  (free,E.t,right,E.t) Format.format
  -> (free,right,E.t) Format.iargs
  -> E.t -> E.t  =
  let open Format in
  let open E in
  fun fmt iargs ppf -> match fmt with
    | [] -> ppf
    | Literal s :: q  -> eval q iargs (string s ppf)
    | Captured f:: q -> let g, iargs = f iargs in eval q iargs (g ppf)
    | Open_tag {tag;data} :: q ->
      let Spec.S s = ppf.logical.tag_semantic in
      let module Sem = (val s.semantic) in
      let with_box, open_box =
        match Sem.box s.data tag data with
        | None -> false, (fun x -> x)
        | Some b -> true, E.open_box b in
      let open_tags: _ list =
        Spec.Open_tag { with_box; tag } :: ppf.logical.open_tags in
      let sdata, p = Sem.open_printer s.data tag data in
      let ppf = ppf |> open_box |> p in
      let logical = { ppf.logical with open_tags;
                      tag_semantic = S { s with data=sdata } } in
      eval  q iargs { ppf with logical }
    | Close_any_tag :: q ->
      begin match ppf.logical.open_tags with
        | [] -> raise No_open_tag
        | Spec.Open_tag r :: tags -> close_tag r.with_box tags q iargs ppf
      end
    | Close_tag ctag :: q ->
      begin match ppf.logical.open_tags with
        | [] -> raise No_open_tag
        | Spec.Open_tag {tag; _ } :: _ when Name tag <> Name ctag ->
          raise (Mismatched_close {expected=tag;got=ctag})
        | Spec.Open_tag {tag; with_box} :: tags ->
          close_tag with_box tags q iargs ppf
      end
    | Break {space; indent} :: q ->
      ppf |> E.break {space;indent} |> eval q iargs



and close_tag: type any free right.
  bool -> Spec.open_tag list
  -> (free,E.t,right,E.t) Format.format
  -> (free,right,E.t) Format.iargs
  -> E.t -> E.t  = fun with_box open_tags q iargs ppf ->
  let open Format in
  let open E in
  let Spec.S s = ppf.logical.tag_semantic in
  let module Sem = (val s.semantic) in
  let acc, p =  Sem.close_printer s.data in
  (*  Printf.eprintf "Eval start %a\n%!" E.pp_pos ppf;*)
  let ppf = p ppf in
  let ppf = if with_box then E.close_box ppf else ppf in
 (* Printf.eprintf "Eval %a\n%!" E.pp_pos ppf;*)
  eval q iargs
    { ppf with logical =
                 { ppf.logical with open_tags; tag_semantic =  S { s with data= acc}
                 }
    }

let eval ppf fmt args = eval fmt (Format.make args) ppf
