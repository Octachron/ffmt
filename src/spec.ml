open Format

type range = {start:int; stop:int}
type substring = { content:string; range:range }

type phy =
  {
    string: substring -> unit;
    space: int -> unit;
    indent: Geometry.Indentation.t -> unit;
    break: unit -> unit;
  }

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

type 'p semantic_with_data =
    S: { semantic: ('acc,'p) tagsem ; data: 'acc }
    -> 'p semantic_with_data

let init (type a b) ((module Sem) as semantic : (a,b) tagsem) =
  S { semantic ; data = Sem.init () }


type open_tag = Open_tag: {tag:'a tag; with_box:bool} -> open_tag
type 'a t = {
  phy: phy;
  geometry: Geometry.t;
  tag_semantic: 'a semantic_with_data;
  open_tags: open_tag list;
}

type 'a spec = 'a t


let full s = { start = 0; stop=String.length s }
let all s = { content=s; range=full s }

let buffer b =
  let string {content;range} =
    Buffer.add_substring b content range.start (range.stop - range.start) in
  let space n = for _ = 1 to n do
      Buffer.add_char b ' '
    done in
  let indent x = space x.Geometry.Indentation.column in
  { string; space; indent; break = (fun () -> Buffer.add_char b '\n'); }

let chan ch =
  let string {content;range} =
    output_substring ch content range.start (range.stop-range.start) in
  let space n = for _ = 1 to n do
      output_string ch " "
    done in
  let indent x = space x.Geometry.Indentation.column in
  { string; space; indent; break = (fun () -> output_string ch "\n"); }
