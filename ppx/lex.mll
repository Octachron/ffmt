{

  type token =
  | FRAG of string
  | OPEN_TAG of string
  | TEXT of string
  | OPEN_IMPLICIT_TAG
  | CLOSE_TAG
  | EOF
  | BREAK of {space:int; indent: int}
  | FULL_BREAK of int
  | IMPLICIT_FRAG of string
  | POS_IMPLICIT_FRAG of string * int

  | IMPLICIT_POS_ARG of int
  | IMPLICIT_ARG of int
  let bfrag = Buffer.create 17
  let char= String.make 1

}

let any = [^ '%' '@']
let num = ['0'-'9']+
let implicit = ['a' 'b' 'B' 'd' 'f' 's' 't' ]
let pos = "$" num
let space = " "+

rule main = parse
 | "%{" { Buffer.clear bfrag; frag 0 lexbuf }
 | any+ as t { TEXT t }
 | "%%" { TEXT "%" }
 | "%" (implicit as i) { IMPLICIT_FRAG (char i) }
 | "%" (implicit as i) "$" (num as n)
 { POS_IMPLICIT_FRAG (char i, int_of_string n) }
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

and frag count = parse
  | "}" { if count = 0 then FRAG(Buffer.contents bfrag)
          else (Buffer.add_char bfrag '}'; frag (count - 1) lexbuf) }
  | "{" { Buffer.add_char bfrag '{'; frag (count + 1) lexbuf }
  | [^ '{' '}' ]+ as s { Buffer.add_string bfrag s; frag count lexbuf }

and open_box = parse
  | "<" ([^ '>']* as tag) ">"  { OPEN_TAG tag }
  | "" { OPEN_IMPLICIT_TAG }

and subfrags = parse
  | "$"(num as n)  { IMPLICIT_POS_ARG (int_of_string n) }
  | ("_"* as skip) "*" { IMPLICIT_ARG (String.length skip) }
  | "_"  { TEXT "_" }
  | "\\*"  { TEXT "*" }
  | "$" | "\\$"  { TEXT "$" }
  | [^ '$' '*' '_']+ as t { TEXT t }
  | eof  { EOF }
