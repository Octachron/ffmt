type range = {start:int; stop:int}
type substring = { content:string; range:range }

type t =
  {
    string: substring -> unit;
    space: int -> unit;
    indent: Geometry.Indentation.t -> unit;
    break: unit -> unit;
  }


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
