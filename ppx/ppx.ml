open Ppx_core.Light

let name = "metafmt-ppx"

let (@?) lex loc =
  lex.Lexing.lex_start_p <- loc.loc_start;
  lex.lex_curr_p <- loc.loc_end;
  lex

let stream lex loc s =
  let lexbuf = Lexing.from_string s @? loc in
  let loc loc_start loc_end=
    Location.{ loc_start; loc_end; loc_ghost = true } in
  fun () -> lex lexbuf,
            loc lexbuf.lex_start_p lexbuf.lex_curr_p

let stop = [%expr [] ]
let (@::) x y = [%expr [%e x] :: [%e y] ]

let str loc x = Ast_builder.Default.estring ~loc x

let subparser loc x =
  let lexbuf = Lexing.from_string x in
  Parser.parse_expression Lexer.token (lexbuf @? loc)


let typ  = function
  | "d" -> [%expr Int]
  | "s" -> [%expr String]
  | "f" -> [%expr Float]
  | _ -> [%expr Int]

exception Not_implemented_yet

let s t = function
  | "d" ->
    [%expr Integer { core = Int Variant.int;
                     padding = implicit;
                     pos=[%e t] } ]
  | "f" -> [%expr Float { variant = Variant.float;
                          padding = implicit;
                          precision = implicit;
                          pos=[%e t] } ]
  | "s" -> [%expr String { variant = Variant.string;
                           padding=implicit; pos=[%e t] } ]
  | _ -> raise Not_implemented_yet

let rec nat n =
  if n <= 0 then [%expr Z]
  else [%expr S [%e nat (n-1)]]

let relative n = [%expr Relative [%e nat n]]
let absolute n = [%expr Absolute [%e nat n]]

module B = Ast_builder.Default

exception Not_supported
let rec ast stream =
  let tok, loc = stream () in
  match tok with
  | Lex.EOF ->  stop
  | TEXT t ->
    [%expr Literal [%e str loc t] ] @:: ast stream
  | FRAG _ -> raise Not_supported
  | OPEN_IMPLICIT_TAG ->
    [%expr Open_tag(B,1)] @:: ast stream
  | OPEN_TAG s ->
    let box = subparser loc s in
    [%expr Open_tag [%e box] ] @:: ast stream
  | CLOSE_TAG ->
    [%expr Close_any_tag] @:: ast stream
  | BREAK {space;indent} ->
    let space = Ast_builder.Default.eint ~loc space
    and indent = Ast_builder.Default.eint ~loc indent in
    [%expr Point_tag(Defs.Break, ([%e space], [%e indent])) ] @:: ast stream
  | IMPLICIT_FRAG "a" -> [%expr Alpha([%e relative 0],[%e relative 0]) ]
    @:: ast stream
  | IMPLICIT_FRAG "t" -> [%expr Theta [%e relative 0] ]  @:: ast stream
  | IMPLICIT_FRAG n -> s (relative 0) n @:: ast stream

  | POS_IMPLICIT_FRAG ("a",pos) ->
    [%expr Alpha([%e absolute pos],[%e relative 0]) ]
    @:: ast stream
  | POS_IMPLICIT_FRAG ("t",pos) ->
    [%expr Theta [%e absolute pos] ]
    @:: ast stream

  | POS_IMPLICIT_FRAG (n,pos) ->
    s (absolute pos) n @:: ast stream
  | FULL_BREAK n ->
    [%expr Point_tag(Defs.Full_break, [%e B.eint ~loc n])] @:: ast stream
  | _ -> assert false


let build ~loc ~path:_ s =
  let frag = ast @@ stream Lex.main loc s in
  [%expr Metafmt.Fmt.( [%e frag ] ) ]





let extension =
  let open Extension in
  declare "fmt" Expression
    Ast_pattern.(single_expr_payload @@ estring __ )
    build

let () = Ppx_driver.register_transformation name ~extensions:[extension]
