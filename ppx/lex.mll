{

  type token =
  | FRAG of string
  | OPEN_TAG of string
  | TEXT of string
  | OPEN_IMPLICIT_TAG
  | CLOSE_TAG
  | EOF
  let bfrag = Buffer.create 17

}

let any = [^ '%' '@']


rule main = parse
 | "%{" { Buffer.clear bfrag; frag 0 lexbuf }
 | any+ as t { TEXT t }
 | "%%" { TEXT "%" }
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
