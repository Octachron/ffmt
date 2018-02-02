
open Format

type range = {start:int; stop:int}
type substring = { content:string; range:range }

type formatter =
  {
    string: substring -> unit;
    space: int -> unit;
    indent: int -> unit;
  }

let buffer b =
  let string {content;range} =
    Buffer.add_substring b content range.start (range.stop - range.start) in
  let space n = for _ = 1 to n do
      Buffer.add_char b ' '
    done in
  { string; space; indent = space }

let chan ch =
  let string {content;range} =
    output_substring ch content range.start (range.stop-range.start) in
  let space n = for _ = 1 to n do
      output_string ch " "
    done in
  { string; space; indent = space }


let stdout = chan Pervasives.stdout
let stderr = chan Pervasives.stderr


let full s = { start = 0; stop=String.length s }
let all s = { content=s; range=full s }

let string s ppf = ppf.string (all s)
let int d = string (string_of_int d)
let float f = string (string_of_float f)


let rec eval:
  type free right.
  formatter -> (free,unit,right,formatter) format
  -> (free,right,unit) iargs
  -> unit  =
  fun ppf fmt iargs -> match fmt with
    | [] -> ()
    | Literal s :: q  -> string s ppf; eval ppf q iargs
    | Captured f:: q -> let g, iargs = f iargs in g ppf; eval ppf q iargs

let eval ppf fmt args = eval ppf fmt (Format.make args)
