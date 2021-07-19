open Ppxlib

let name = "ffmt-ppx"

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

let stop loc = [%expr [] ]
let mkcons loc x y = [%expr [%e x] :: [%e y] ]

let str loc x = Ast_builder.Default.estring ~loc x

let subparser loc x =
  let lexbuf = Lexing.from_string x in
  Parser.parse_expression Lexer.token (lexbuf @? loc)


let typ loc  = function
  | "d" -> [%expr Int]
  | "s" -> [%expr String]
  | "f" -> [%expr Float]
  | _ -> [%expr Int]

exception Not_implemented_yet

let s loc t = function
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

let rec nat loc n =
  if n <= 0 then [%expr Z]
  else [%expr S [%e nat loc (n-1)]]

let relative loc n = [%expr Relative [%e nat loc n]]
let absolute loc n = [%expr Absolute [%e nat loc n]]

module B = Ast_builder.Default

let hole loc = function
  | Mlex.Abs n -> absolute loc n
  | Mlex.Rel n -> relative loc n

let param loc (x:Mlex.param) = match x with
  | Abs n -> absolute loc n
  | Rel n -> relative loc n
  | No -> [%expr Int_constant None]
  | Const i -> [%expr Int_const (Some [%e B.eint ~loc i]) ]

let simple loc pos = function
  | 't' -> [%expr Theta [%e hole loc pos]]
  | 'b' | 'B' -> [%expr S(Bool,[%e hole loc pos])]
  | 'c' | 'C' -> [%expr S(Char,[%e hole loc pos])]
  | _ -> assert false

let integer loc (r:Mlex.integer_info) =
  let typ = match r.typ with
    | Some 'l' -> [%expr Int32]
    | Some 'L' -> [%expr Int64]
    | Some 'n' -> [%expr Native_int]
    | _  -> [%expr Int] in
  let alt = if r.alt then [%expr Hash] else [%expr Std] in
  let a = if r.left_align then [%expr Left] else [%expr Right] in
  let zp = if r.zero_padding then [%expr Zero] else [%expr Space] in
  let s = if r.sign then [%expr Plus] else [%expr No] in
  let v = match r.variant with
    | 'X' | 'x' ->
      let c = B.ebool ~loc (r.variant = 'X') in
      [%expr Hex { capitalized=[%e c];
               hex = [%e alt];
               align=[%e a];
               padding = [%e zp]}]
    | 'o' -> [%expr Octa { hex=[%e alt]; align=[%e a]; padding=[%e zp] } ]
    | 'u' -> [%expr Unsigned {align=[%e a]; padding=[%e zp] } ]
    | _ -> [%expr Signed {align=[%e a]; padding=[%e zp]; sign=[%e s] } ]
  in
  [%expr Integer { variant=[%e v]; core = [%e typ];
                   padding=[%e param loc r.padding];
                   pos=[%e hole loc r.pos]}
  ]

let float loc (r:Mlex.float_info) =
  let a = if r.left_align then [%expr Left] else [%expr Right] in
  let zp = if r.zero_padding then [%expr Zero] else [%expr Space] in
  (*  let s = if r.sign then [%expr Plus] else [%expr No] in*)
  let v = match r.variant with
    | 'H' | 'h' ->
      let c = B.ebool ~loc (r.variant = 'H') in
      [%expr Float_hex { capitalized=[%e c]; align=[%e a]; padding = [%e zp]}]
    | 'F' -> [%expr Float { reflect=true; align=[%e a]; padding=[%e zp] } ]
    | 'g' -> [%expr Float_g { align=[%e a]; padding=[%e zp] } ]
    | 'e' -> [%expr Float_g { align=[%e a]; padding=[%e zp] } ]
    | 'f' | _ -> [%expr Float { reflect=false; align=[%e a]; padding=[%e zp] } ]
  in
  [%expr Float { variant=[%e v];
                 padding=[%e param loc r.padding];
                 precision=[%e param loc r.precision];
                 pos=[%e hole loc r.pos]}
  ]


let string loc (r: Mlex.string_info) =
  let re = B.ebool ~loc (r.variant = 'S') in
  let a = if r.left_align then [%expr Left] else [%expr Right] in
  [%expr String { variant = { reflect = [%e re]; align=[%e a] };
                  padding = [%e param loc r.padding];
                  pos = [%e hole loc r.pos] }
  ]



exception Not_supported
let rec ast stream =
  let tok, loc = stream () in
  Ast_helper.default_loc := loc;
  let (@::) = mkcons loc in
  let k x = x @:: ast stream in
  match tok with
  | Mlex.EOF ->  stop loc
  | TEXT t -> k [%expr Literal [%e str loc t] ]
  | OPEN_DEFAULT_TAG -> k [%expr Open_tag(B,1)]
  | OPEN_TAG s -> k [%expr Open_tag [%e subparser loc s] ]
  | CLOSE_TAG -> k [%expr Close_any_tag]
  | BREAK {space;indent} ->
    let space = Ast_builder.Default.eint ~loc space
    and indent = Ast_builder.Default.eint ~loc indent in
    k [%expr Point_tag(Defs.Break, ([%e space], [%e indent])) ]
  | FULL_BREAK n -> k [%expr Point_tag(Defs.Full_break, [%e B.eint ~loc n])]
  | ALPHA(a,b) -> k [%expr Alpha([%e hole loc a],[%e hole loc b]) ]
  | SIMPLE {variant; pos} -> k @@ simple loc pos variant
  | INTEGER r -> k @@ integer loc r
  | FLOAT r -> k @@ float loc r
  | STRING r -> k @@ string loc r
  | SKIP n -> k @@ [%expr Skip [%e nat loc (n-1) ] ]

let build ~loc ~path:_ s =
  let frag = ast @@ stream Mlex.main loc s in
  [%expr Freefmt.Fmt.( [%e frag ] ) ]





let extension =
  let open Extension in
  declare "fmt" Expression
    Ast_pattern.(single_expr_payload @@ estring __ )
    build

let () = Driver.register_transformation name ~extensions:[extension]
