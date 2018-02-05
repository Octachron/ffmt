
open Format

type range = {start:int; stop:int}
type substring = { content:string; range:range }

type geometry = { margin: int; max_indent:int }

type indentation = { line: int; column:int }
let zindent = { line=0; column=0 }

type status = { position: int; indent: indentation; box_indent: indentation }

let box_indent: Format.box -> _ = function
  | H -> 0
  | V n | B n | HV n | HoV n -> n

type core =
  {
    string: substring -> unit;
    space: int -> unit;
    indent: int -> unit;
    break: unit -> unit;
  }

type tag_printer = core captured

module type tag_semantic = sig
  type data
  type printer
  val init: unit -> data
  val box: data -> 'any tag -> 'any -> box option
  val open_printer: data -> 'any tag -> 'any -> data * printer
  val close_printer: data -> data * printer
end

type ('acc,'printer) tagsem =
  (module tag_semantic with type data = 'acc and type printer = 'printer captured )

type 'acc tag_semantic = {
  init: unit -> 'acc;
  box_from_tag: 'any. 'any tag -> 'any -> box;
  open_printer_from_tag: 'any. 'acc -> 'any tag -> 'any -> 'acc * core captured;
  close_printer_from_tag: 'acc -> 'acc * core captured;
}

type 'p semantic_with_data =
    S: { semantic: ('acc,'p) tagsem ; data: 'acc }
    -> 'p semantic_with_data

let init (type a b) ((module Sem) as semantic : (a,b) tagsem) =
  S { semantic ; data = Sem.init () }

type open_tag = Open_tag: {tag:'a tag; with_box:bool} -> open_tag
type t = {
  core: core;
  geometry: geometry;
  open_tags: open_tag list;
  position: status;
  open_boxes: box list;
  tag_semantic: t semantic_with_data;
}

module Null_semantic = struct
  type data = unit
  type printer = t captured
  let init () = ()
  let box _data _tag _tag_data= None
  let open_printer _ _ _ = (), (fun _ -> ())
  let close_printer _ =  (), (fun _ -> ())
end


type formatter = t
type printer = t captured

let core_buffer b =
  let string {content;range} =
    Buffer.add_substring b content range.start (range.stop - range.start) in
  let space n = for _ = 1 to n do
      Buffer.add_char b ' '
    done in
  { string; space; indent = space; break = (fun () -> Buffer.add_char b ' ') }

let core_chan ch =
  let string {content;range} =
    output_substring ch content range.start (range.stop-range.start) in
  let space n = for _ = 1 to n do
      output_string ch " "
    done in
  { string; space; indent = space; break = (fun () -> output_string ch "\n") }

let null_semantic =
  (module Null_semantic: tag_semantic with type data = unit
                                       and type printer = printer  )

let default_geometry = { margin = 78; max_indent=68 }

let start = { position=0; indent=zindent; box_indent=zindent }

let with_sem ?(geometry=default_geometry) f ?tag_semantic x =
  let tag_semantic =
  match tag_semantic with
  | None -> init null_semantic
  | Some x -> init x in
  { core = f x; open_tags = []; open_boxes = []; tag_semantic; geometry;
    position = start;
  }

let chan x = with_sem core_chan x
let buffer x = with_sem core_buffer x

let stdout = chan Pervasives.stdout
let stderr = chan Pervasives.stderr


type tag_name= Name: 'any tag -> tag_name
let full s = { start = 0; stop=String.length s }
let all s = { content=s; range=full s }

let string s: printer = fun ppf -> ppf.core.string (all s)
let int d: printer = string (string_of_int d)
let float f: printer = string (string_of_float f)

exception No_open_tag
type exn += Mismatched_close: {expected:'any tag; got:'other tag} -> exn

let rec eval:
  type free right.
  formatter -> (free,unit,right,formatter) format
  -> (free,right,unit) iargs
  -> unit  =
  fun ppf fmt iargs -> match fmt with
    | [] -> ()
    | Literal s :: q  -> string s ppf; eval ppf q iargs
    | Captured f:: q -> let g, iargs = f iargs in g ppf; eval ppf q iargs
    | Open_tag {tag;data} :: q ->
      let S s = ppf.tag_semantic in
      let module Sem = (val s.semantic) in
      let with_box, open_boxes =
        let before = ppf.open_boxes in
        match Sem.box s.data tag data with
        | None -> false, before
        | Some b -> true, b :: before in
      let open_tags: _ list = Open_tag { tag; with_box } :: ppf.open_tags in
       let sdata, p = Sem.open_printer s.data tag data in
      p ppf;
      eval { ppf with open_tags; open_boxes;
                      tag_semantic = S { s with data=sdata} } q iargs
    | Close_any_tag :: q ->
      begin match ppf.open_tags with
        | [] -> raise No_open_tag
        | Open_tag r :: tags -> close_tag r.with_box tags ppf q iargs
      end
    | Close_tag ctag :: q ->
      begin match ppf.open_tags with
        | [] -> raise No_open_tag
        | Open_tag {tag; _ } :: _ when Name tag <> Name ctag ->
          raise (Mismatched_close {expected=tag;got=ctag})
        | Open_tag {tag; with_box} :: tags ->
          close_tag with_box tags ppf q iargs
      end
    | Break { space; indent} :: q ->
      eval ppf q iargs

and close_tag: type any free right.
  bool -> open_tag list
  -> formatter -> (free,unit,right,formatter) format -> (free,right,unit) iargs
  ->  unit = fun with_box open_tags ppf q iargs ->
  let S s = ppf.tag_semantic in
  let module Sem = (val s.semantic) in
  let acc, p =  Sem.close_printer s.data in
  p ppf;
  let open_boxes = if with_box then List.tl ppf.open_boxes else
      ppf.open_boxes in
  let ppf = { ppf with open_tags; open_boxes;
                       tag_semantic = S { s with data= acc}
            } in
  eval ppf q iargs


let eval ppf fmt args = eval ppf fmt (Format.make args)
