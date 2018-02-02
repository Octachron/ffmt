
open Format

type range = {start:int; stop:int}
type substring = { content:string; range:range }

type core =
  {
    string: substring -> unit;
    space: int -> unit;
    indent: int -> unit;
  }

type 'acc tag_semantic = {
  init: unit -> 'acc;
  box_from_tag: 'any. 'any tag -> 'any -> box;
  open_printer_from_tag: 'any. 'acc -> 'any tag -> 'any -> 'acc * core captured;
  close_printer_from_tag: 'any. 'acc -> 'any tag -> 'any -> 'acc * core captured;
}

type semantic_with_data =
    S: { semantic: 'acc tag_semantic; data: 'acc } -> semantic_with_data

let init semantic = S { semantic; data = semantic.init () }

type open_tag = Open_tag: { tag: 'a tag; data:'a; box:box} -> open_tag
type t = {
  core: core;
  open_tags: open_tag list;
  tag_semantic: semantic_with_data;
}

let null_semantic =
  {
    init = (fun () -> ());
    box_from_tag = (fun _ _ -> No_box);
    open_printer_from_tag = (fun _ _ _ -> (), fun _ -> ());
    close_printer_from_tag = (fun _ _ _ -> (), fun _ -> () );
  }


type formatter = t
type printer = t captured

let core_buffer b =
  let string {content;range} =
    Buffer.add_substring b content range.start (range.stop - range.start) in
  let space n = for _ = 1 to n do
      Buffer.add_char b ' '
    done in
  { string; space; indent = space }

let core_chan ch =
  let string {content;range} =
    output_substring ch content range.start (range.stop-range.start) in
  let space n = for _ = 1 to n do
      output_string ch " "
    done in
  { string; space; indent = space }


let with_sem f ?(tag_semantic=null_semantic) x =
  { core = f x; open_tags = []; tag_semantic = init tag_semantic }

let chan = with_sem core_chan
let buffer = with_sem core_buffer

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
      let o = Open_tag { box = s.semantic.box_from_tag tag data; tag; data } in
      let open_tags: _ list = o :: ppf.open_tags in
      let sdata, p = s.semantic.open_printer_from_tag s.data tag data in
      p ppf.core;
      eval { ppf with open_tags; tag_semantic = S { s with data=sdata} } q iargs
    | Close_tag ctag :: q ->
      begin match ppf.open_tags with
        | [] -> raise No_open_tag
        | Open_tag { tag; _ } :: _ when Name tag <> Name ctag ->
          raise (Mismatched_close {expected=tag;got=ctag})
        | Open_tag{ data; tag} :: tags  ->
          let S s = ppf.tag_semantic in
          let acc, p =  s.semantic.close_printer_from_tag s.data tag data in
          p ppf.core;
          let ppf = { ppf with open_tags = tags;
                               tag_semantic = S { s with data= acc}
                    } in
          eval ppf q iargs
      end

let eval ppf fmt args = eval ppf fmt (Format.make args)
