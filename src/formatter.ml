
module E = Engine

let with_sem f = fun ?(geometry=Geometry.default) ?tag_semantic x ->
  let tag_semantic =
  match tag_semantic with
  | None -> Spec.T Semantics.box
  | Some x -> T x in
  let logical = { Spec.phy = f x; tag_semantic; geometry; open_tags = [] } in
  { E.logical; status = Direct {position=0;indent=0; kind=H; last_indent=0 };
    context = [] }

let chan ?geometry = with_sem Spec.chan ?geometry
let buffer ?geometry = with_sem Spec.buffer ?geometry

let stdout = chan Pervasives.stdout
let stderr = chan Pervasives.stderr


type tag_name= Name: 'any Format.tag -> tag_name



let string = E.string

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
    | Open_tag (tag,data) :: q ->
      let Spec.T sem = ppf.logical.tag_semantic in
      let with_box, open_box =
        match sem.box sem.data tag data with
        | None -> false, (fun x -> x)
        | Some b -> true, E.open_box b in
      let open_tags: _ list =
        Spec.Open_tag { with_box; tag } :: ppf.logical.open_tags in
      let sdata, p = sem.open_printer sem.data tag data in
      let tag_semantic = Spec.T { sem with data = sdata } in
      let ppf = ppf |> open_box |> p in
      let logical = { ppf.logical with open_tags;
                      tag_semantic } in
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
    | Full_break b :: q ->
      ppf |> E.full_break b |> eval q iargs



and close_tag: type any free right.
  bool -> Spec.open_tag list
  -> (free,E.t,right,E.t) Format.format
  -> (free,right,E.t) Format.iargs
  -> E.t -> E.t  = fun with_box open_tags q iargs ppf ->
  let open Format in
  let open E in
  let Spec.T sem = ppf.logical.tag_semantic in
  let acc, p =  sem.close_printer sem.data in
  (*  Printf.eprintf "Eval start %a\n%!" E.pp_pos ppf;*)
  let ppf = p ppf in
  let ppf = if with_box then E.close_box ppf else ppf in
 (* Printf.eprintf "Eval %a\n%!" E.pp_pos ppf;*)
  eval q iargs
    { ppf with logical =
                 { ppf.logical with open_tags;
                                    tag_semantic =  T { sem with data= acc}
                 }
    }

let eval ppf fmt args = eval fmt (Format.make args) ppf
