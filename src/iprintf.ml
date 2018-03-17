open Formatter

let rec eval:
  type free b right final before after.
  <all:free; right:right; tail:b; fmt:final;
   tag_count:before -> after > F.format
  -> (free,right) F.iargs
  -> (final,before) t -> (final,after) t  =
  let open F in
  fun fmt iargs ppf -> match fmt with
    | [] -> ppf
    | Literal s :: q  -> eval q iargs (string s ppf)
    | Captured (k, f):: q ->
      let ppf = f iargs ppf in
      let iargs = F.take k iargs in
      eval q iargs ppf
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

and close_tag: type any n after free b right final.
  bool -> any Defs.tag -> (n, Sem.open_tag) NL.t
  -> <all:free; right:right; tail:b; fmt:final;
      tag_count: n -> after > F.format
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
