

type range = {start:int; stop:int}
type substring = { content:string; range:range }

type formatter =
  {
    string: substring -> unit;
    space: int -> unit;
    indent: int -> unit;
  }

let buffer =
  let b = Buffer.create 17 in
  let string {content;range} =
    Buffer.add_substring b content range.start (range.stop - range.start) in
  let space n = for _ = 1 to n do
      Buffer.add_char b ' '
    done in
  { string; space; indent = space }

type captured = formatter -> unit
type 'a printer = 'a -> captured

type (_,_) index =
  | Z: ('elt, 'elt -> _ ) index
  | S: ('elt, 'list) index -> ('elt, 'any -> 'list) index

type (_,_) token =
  | Literal: string -> ('list,'close) token
  | Captured: captured -> ('list,'close) token
  | Var: { typ:'typ printer;
           pos: ('typ, 'list) index
         } -> ('list,'close) token

type (_,_) format =
  | []: ('any,'result) format
  | (::): ('list,'result) token * ('list,'result) format ->
    ('list,'result) format


type (_,_) args =
  | []: ('result,'result) args
  | (::): 'a * ('list,'result) args -> ('a -> 'list, 'result) args

let rec nth: type elt a r. (elt,a) index -> (a,r) args -> elt = fun n  args ->
  match n, args with
  | Z , a :: _ -> a
  | S n, _ :: q -> nth n q
  | _, [] -> raise Not_found

let full s = { start = 0; stop=String.length s }

let all s = { content=s; range=full s }

let string s ppf = ppf.string (all s)
let int d = string (string_of_int d)
let float f = string (string_of_float f)

let eval_token:
  type free. formatter -> (free,unit) token -> (free,unit) args -> unit =
  fun ppf fmt xs -> match fmt with
    | Literal s -> string s ppf
    | Captured x -> x ppf
    | Var { typ; pos } -> typ (nth pos xs) ppf

let rec eval:
  type free. formatter -> (free,unit) format -> (free,unit) args -> unit  =
  fun ppf fmt xs -> match fmt with
    | [] -> ()
    | a :: q -> eval_token ppf a xs; eval ppf q xs

let rec (^^): type free.
  (free,unit) format -> (free,unit) format -> (free,unit) format =
  fun l r -> match l with
    | [] -> r
    | a :: q -> a :: (q ^^ r)


let (!$) x = Captured x
let (%:) nth typ = Var{ pos=nth; typ }
let l x = Literal x
let test ppf s =
  eval ppf [ l"Hello "; !$(string s); l"N°"; (Z %: int); (S Z %: string ) ]
    [1;"How is your day?"]
