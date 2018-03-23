{

  type param = No | Rel of int | Const of int | Abs of int
  type hole = Abs of int | Rel of int
  type align = Left | Right
  type string_info = { variant:char; left_align:bool; padding:param; pos:hole}

  type integer_info = {padding:param;
    left_align:bool;
    variant:char;
    zero_padding: bool;
    sign: bool;
    alt:bool;
    typ: char option;
    pos:hole }
  type float_info = {
    zero_padding:bool;
    left_align:bool;
    sign:bool;
    precision:param;
    padding:param;
    variant:char;
    pos: hole }

  type token =
  | OPEN_TAG of string
  | OPEN_DEFAULT_TAG
  | TEXT of string

  | FLOAT of float_info
  | INTEGER of integer_info
  | SIMPLE of {variant:char; pos: hole }
  | STRING of string_info
  | ALPHA of hole * hole

  | CLOSE_TAG
  | EOF
  | BREAK of {space:int; indent: int}
  | FULL_BREAK of int

  let bfrag = Buffer.create 17
  let char= String.make 1
  let star: hole = Rel 0
  let star': param = Rel 0
  let pos n: hole = Abs (int_of_string n)
  let mpos = function None -> star | Some s -> pos s
  let par = function
  | None -> No
  | Some "*" -> Rel 0
  | Some s -> if s.[0] = '$' then
    let s' = String.sub s 1 (String.length s - 1) in
    Abs (int_of_string s')
    else Const (int_of_string s)

}

let any = [^ '%' '@']
let num = ['0'-'9']+
let implicit = ['a' 'b' 'B' 'd' 'f' 's' 't' ]
let float = ['f' 'F' 'g' 'h' 'e']
let integer = [ 'i' 'I' 'u' 'd' 'x' 'X' 'o']
let int_typ = [ 'l' 'L' 'n']
let pos = "$" num
let space = " "+
let float = ['f' 'F' 'g' 'G']
let string = ['s' 'S']
let simple = ['b' 'B' 'c' 'C' 't']


rule main = parse
 | any+ as t { TEXT t }
 | "%%" { TEXT "%" }
 | "%a" ("$" (num as n))? ("$" (num as l))? { ALPHA(mpos n,mpos l) }
 | "%" ("-" as align)? ("+" as sign)?
       ("0" as zp)? ("*"| num | "$" num as p)? ("." ("*"| "$" num | num as pr))?
       (float as variant) ("$" (num as n))?
 { FLOAT {
     zero_padding = zp <> None; sign = sign <> None; left_align = align <> None;
     pos = mpos n; padding=par p; variant;
     precision=par pr }
 }
 | "%" ("-" as align)? ("+" as sign)? ("#" as alt)?
        ("0" as zp)? ("*"| num | "$" num as p)? (int_typ as typ)?
       (integer as variant) ("$" (num as n))?
 {
   INTEGER { typ; variant; left_align = align <> None;
             sign = sign <> None; alt = alt <> None; zero_padding = zp <> None;
             padding = par p; pos = mpos n }
 }

 | "%" (simple as s) ("$" (num as n))? { SIMPLE { variant = s; pos = mpos n } }
 | "%" ("-" as align)? ("*" | num | "$" num as padding)? (string as variant)
  ("$" (num as n))?
 { STRING { variant;
            left_align = align <> None;
	    padding = par padding;
	    pos = mpos n}
 }
 | "@\n" { FULL_BREAK 0 }
 | "@\n<"(num as n)">" { FULL_BREAK (int_of_string n) }
 | "@," { BREAK{space=0; indent=0} }
 | "@ " { BREAK{space=1; indent=0} }
 | "@;<"(num as space) space (num as indent) ">"
 { BREAK{space=int_of_string space; indent = int_of_string indent } }
 | "@@" { TEXT "@" }
 | "@[" | "@{" { open_box lexbuf }
 | "@]" | "@}" { CLOSE_TAG }
 | eof { EOF }

and open_box = parse
  | "<" ([^ '>']* as tag) ">"  { OPEN_TAG tag }
  | "" { OPEN_DEFAULT_TAG }
