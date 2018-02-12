open Ppx_core.Light

let name = "stringi-ppx"


let (@?) lex loc =
  lex.Lexing.lex_start_p <- loc.loc_start;
  lex.lex_curr_p <- loc.loc_end;
  lex

let stream loc s =
  let lexbuf = Lexing.from_string s @? loc in
  let loc loc_start loc_end= Location.{ loc_start; loc_end; loc_ghost = true } in
  fun () -> Lex.main lexbuf,
            loc lexbuf.lex_start_p lexbuf.lex_curr_p

let stop = [%expr [] ]
let (@::) x y = [%expr [%e x] :: [%e y] ]

let str loc x = Ast_builder.Default.estring loc x




let subparser loc x =
  let lexbuf = Lexing.from_string x in
  Parser.parse_expression Lexer.token (lexbuf @? loc)

let implicit = function
  | "d" -> [%expr int]
  | "s" -> [%expr string]
  | "f" -> [%expr float]
  | "B" | "b" -> [%expr bool]
  | _ -> [%expr int]

let rec nat n =
  if n <= 0 then [%expr Z]
  else [%expr S [%e nat (n-1)]]

module B = Ast_builder.Default
let escaped x =("%"^x^"%")

let ge x = B.evar (escaped x)
let gp x = B.pvar (escaped x)

let rec ast stream =
  let tok, loc = stream () in
  match tok with
  | Lex.EOF ->  stop
  | TEXT t ->
    [%expr Literal [%e str loc t] ] @:: ast stream
  | FRAG s ->
    let captured = subparser loc s in
    [%expr Captured [%e captured]] @:: ast stream
  | OPEN_IMPLICIT_TAG ->
    [%expr Open_tag(B,1)] @:: ast stream
  | OPEN_TAG s ->
    let box = subparser loc s in
    [%expr Open_tag [%e box] ] @:: ast stream
  | CLOSE_TAG ->
    [%expr Close_any_tag] @:: ast stream
  | BREAK {space;indent} ->
    let space = Ast_builder.Default.eint loc space
    and indent = Ast_builder.Default.eint loc indent in
    [%expr Break {space=[%e space]; indent=[%e indent] } ] @:: ast stream
  | IMPLICIT_FRAG "a" ->
    [%expr Captured (fun ({ right = a :: b :: right; _ } as iargs) ->
        a b, { iargs with right }
      )
    ] @:: ast stream
  | IMPLICIT_FRAG "t" ->
    [%expr Captured (fun ({ right = a :: right; _ } as iargs) ->
        a, { iargs with right }) @:: ast stream
    ] @:: ast stream
  | IMPLICIT_FRAG n ->
    [%expr Captured (fun ({ right = a :: right; _ } as iargs) ->
        [%e implicit n] a, { iargs with right }
      )
    ] @:: ast stream

  | POS_IMPLICIT_FRAG (n,pos) ->
    let pargs = gp "iargs" loc in
    let eargs = ge "iargs" loc in
    let arg = [%expr nth [%e eargs] [%e nat pos]] in
    [%expr Captured (fun [%p pargs] ->
        [%e implicit n] [%e arg], [%e eargs]
      )
    ] @:: ast stream



let build ~loc ~path:_ s =
  [%expr Stringi.Format.( [%e ast @@ stream loc s] ) ]





let extension =
  let open Extension in
  declare "fmt" Expression
    Ast_pattern.(single_expr_payload @@ estring __ )
    build

let () = Ppx_driver.register_transformation name ~extensions:[extension]
