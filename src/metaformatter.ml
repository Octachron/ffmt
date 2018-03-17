(** Formatter *)


module E = Engine
module D = Defs
module Ft = Formatter_def
module F = Fmt
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

let rec primitive: type x any. (x,any) F.typ -> x -> _ =
  fun typ x ->
    let open F in
    match typ with
    | String -> string x
    | Int -> string (string_of_int x)
    | Int32 -> string (Int32.to_string x)
    | Int64 -> string (Int64.to_string x)
    | Nativeint -> string (Nativeint.to_string x)
    | Float -> string (string_of_float x)
    | Char -> string (String.make 1 x)
    | Uchar -> string( string_of_int (Uchar.to_int x))
    | List typ -> fun ppf ->
      List.fold_left (fun ppf x -> ppf |> primitive typ x |> string ";" )
        (string "[ " ppf) x |> string " ]"
    | Array typ -> fun ppf ->
      Array.fold_left (fun ppf x -> ppf |> primitive typ x |> string ";" )
        (string "[| " ppf) x |> string " |]"
    | Option typ ->
      match x with
      | None -> string "None"
      | Some x -> fun ppf ->
        ppf |> string "Some(" |> primitive typ x |> string ")"

let rec eval:
  type free b right final before after.
  <all:free; right:right; tail:b; fmt:final; tag_count:before -> after > F.t
  -> (free,right) F.iargs
  -> (final,before) t -> (final,after) t  =
  let open F in
  fun fmt iargs ppf -> match fmt with
    | [] -> ppf
    | Literal s :: q  -> eval q iargs (string s ppf)
    | Open_tag (tag,data) :: q ->
      begin match Sem.find_sem tag ppf.tag_semantic with
        | None -> eval q iargs (open_tag (Open_tag {with_box=false; tag}) ppf)
        | Some (sem, rest) ->
          let with_box, open_box =
            match sem#box tag data with
            | None -> false, (fun ppf -> ppf)
            | Some b -> true, open_box b in
          let open_tags: _ NL.t =
            Sem.Open_tag { with_box; tag } :: ppf.open_tags in
          let sem, p = sem#open_printer tag data in
          let tag_semantic: _ list = sem :: rest in
          let ppf = ppf |> open_box |> p in
          eval q iargs { ppf  with open_tags; tag_semantic }
      end
    | Close_any_tag :: q ->
      let NL.(Sem.Open_tag r :: tags) = ppf.open_tags in
      close_tag r.with_box r.tag tags q iargs ppf
    | Close_tag ctag :: q ->
      let NL.( Sem.Open_tag {tag; with_box} :: tags ) = ppf.open_tags in
      if  Name tag <> Name ctag then
        raise (Mismatched_close {expected=tag;got=ctag})
      else
        close_tag with_box tag tags q iargs ppf
    | Point_tag (tag, tdata) :: q ->
      let ppf =
      begin match Sem.find_sem tag ppf.tag_semantic with
        | None -> ppf
        | Some (sem,rest) ->
          let sem, p = sem#open_printer tag tdata in
          let ppf = p ppf in
          let ppf = begin match sem#break tag tdata with
            | None -> ppf
            | Some Break {space; indent} -> break {space;indent} ppf
            | Some Full_break b -> full_break b ppf
          end in
          let sem, p = sem#close_printer  in
          let ppf = p ppf in
          { ppf with tag_semantic= sem ::rest }
      end in
      eval q iargs ppf
    | S (n,typ) :: q  ->
      let x, rest = F.take iargs n in
      ppf |> primitive typ x |> eval q rest
    | Theta n :: q  ->
      let f, rest = F.take iargs n in
      ppf |> f |> eval q rest
    | Alpha (fn,an) :: q ->
      let f, rest = F.take iargs fn in
      let x, rest = F.take rest an in
      ppf |> f x |> eval q rest
    | Star (One l, typ, k) :: q ->
      let _x, rest = take iargs l in
      let y, rest = take rest k in
      ppf |> primitive typ y |> eval q rest
  | Star (Two (k,l), typ, n) :: q ->
    let _x, rest = take iargs k in
    let _y, rest = take rest l in
    let y, rest = take rest n in
    ppf |> primitive typ y |> eval q rest

and close_tag: type any n after free b right final.
  bool -> any Defs.tag -> (n, Sem.open_tag) NL.t
  -> <all:free; right:right; tail:b; fmt:final;
      tag_count: n -> after > F.t
  -> (free,right) F.iargs
  -> (final,n F.s) t -> (final, after) t =
  fun with_box tag open_tags q iargs ppf ->
  match Sem.find_sem tag ppf.tag_semantic with
  | None -> eval q iargs { ppf with open_tags }
  | Some (sem, rest) ->
    let sem, p =  sem#close_printer in
    (*  Printf.eprintf "Eval start %a\n%!" E.pp_pos ppf;*)
    let ppf = p ppf in
    let ppf = if with_box then close_box ppf else ppf in
    (* Printf.eprintf "Eval %a\n%!" E.pp_pos ppf;*)
    let tag_semantic: _ list = sem :: rest in
    eval q iargs { ppf with open_tags; tag_semantic }

let fprintf fmt args = eval fmt (F.make args)
