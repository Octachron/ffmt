
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
  -> (free,unit) args -> (right,unit) args
  -> unit  =
  fun ppf fmt xs ys -> match fmt,ys with
    | [], ys -> ()
    | Literal s :: q , ys -> string s ppf; eval ppf q xs ys
    | Captured x:: q, ys -> x ppf; eval ppf q xs ys
    | Var { typ; pos }:: q , ys -> typ (nth pos xs) ppf; eval ppf q xs ys
    | Implicit_pos typ :: q , y :: ys -> typ y ppf; eval ppf q xs ys
    | Implicit :: q, printer :: y :: ys -> printer y ppf; eval ppf q xs ys
    | Ext_var { data_index; printer_index } :: q , ys ->
      (nth printer_index xs) (nth data_index xs) ppf; eval ppf q xs ys


let eval ppf fmt args = eval ppf fmt args args

(*
let rec (^^): type right free.
  (free,unit,right) format -> (free,unit,right) format -> (free,unit,right) format =
  fun l r -> match l with
    | [] -> r
    | a :: q -> a :: (q ^^ r)
*)
