

type range = {start:int; stop:int}
type substring = { content:string; range:range }

type formatter =
  {
    string: substring -> unit;
    space: int -> unit;
    indent: int -> unit;
  }

type show = formatter -> unit

type token =
  | Literal of string
  | Var of show
and format = token list

let full s = { start = 0; stop=String.length s }

let all s = { content=s; range=full s }

let string s ppf = ppf.string (all s)
let int d = string (string_of_int d)

let eval_token f = function
  | Literal s -> string s f
  | Var x -> x f

let eval ppf = List.iter (eval_token ppf)

let (!$) x = Var x
let l x = Literal x
let test ppf s = eval ppf [ l"Hello "; !$(string s)]
