open Ppx_core.Light

let name = "freefmt-ppx"


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

let implicit  = function
  | "d" -> [%expr int]
  | "s" -> [%expr string]
  | "f" -> [%expr float]
  | "B" | "b" -> [%expr bool]
  | _ -> [%expr int]

let positional n arg  = match n with
  | "d" -> [%expr int [%e arg] ]
  | "s" -> [%expr string [%e arg]]
  | "f" -> [%expr float [%e arg] ]
  | "B" | "b" -> [%expr bool [%e arg]]
  | "t" -> arg
  | _ -> [%expr int]


let rec nat n =
  if n <= 0 then [%expr Z]
  else [%expr S [%e nat (n-1)]]

let nat' n=
  let rec close n = if n = 0 then [] else ")" :: close (n-1) in
  let rec nat n start =
    if n = 0 then "Z" :: start else "S (" :: nat (n-1) start in
  String.concat "" @@ "Size.(" :: nat n (close@@n+1)

module B = Ast_builder.Default
let escaped x =("%"^x^"%")

let ge x = B.evar (escaped x)
let gp x = B.pvar (escaped x)


let contextualize iarg explicit n s =
  let rec args last k =
    if k = last then
      if iarg then ["_; _ } as iargs)"] else ["_;_}"]
    else
      (if List.mem k explicit then [] else ["_"]) @
      ["x"; string_of_int k; "::"] @ args last (k+1) in
  String.concat "" @@
  "Captured(" :: nat' n :: ", "
  ::"fun " :: (if iarg then "(" else "")
  :: "{ right = " :: args n 0 @ "->" :: s @ [")"]

let rec snat = function
  | 0 -> "Z"
  | k when k < 0 -> "Z"
  | k -> "S (" ^ snat (k-1) ^ ")"

let frag loc s=
  let s = stream Lex.subfrags loc s in
  let rec rewrite with_iarg explicit n ls = let tok, _loc = s () in
    match tok with
    | Lex.EOF -> contextualize with_iarg explicit n @@ List.rev @@ ls
    | IMPLICIT_POS_ARG pos ->
      rewrite true explicit n
      @@ "))" :: (snat pos) :: "(nth iargs (" :: ls
    | IMPLICIT_ARG skip ->
      rewrite with_iarg ((n+skip)::explicit ) (n+skip+1)
      @@ ("x" ^ string_of_int(n + skip)) :: ls
    | TEXT t -> rewrite with_iarg explicit n @@ t :: ls
    | _ -> rewrite with_iarg explicit n ls in
  subparser loc @@ rewrite false [] 0 []

let rec ast stream =
  let tok, loc = stream () in
  match tok with
  | Lex.EOF ->  stop
  | TEXT t ->
    [%expr Literal [%e str loc t] ] @:: ast stream
  | FRAG s ->
    let captured = frag loc s in
    captured @:: ast stream
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
  | IMPLICIT_FRAG "a" ->
    [%expr Captured ( Size.(S (S Z)),
                      fun { right = f :: x :: _; _ } -> f x ) ]
    @:: ast stream
  | IMPLICIT_FRAG "t" ->
    [%expr Captured (Size.(S Z), fun { right = a :: _; _ } -> a)]
    @:: ast stream
  | IMPLICIT_FRAG n ->
    [%expr Captured (Size.(S Z),
                     fun { right = a :: _; _ } -> [%e implicit n] a)]
    @:: ast stream

  | POS_IMPLICIT_FRAG ("a",pos) ->
    let eone = ge "one" ~loc in
    let pone = gp "one" ~loc in
    let arg = [%expr nth __iargs__ [%e nat pos]] in
    [%expr
      Captured (Size.(S Z), fun ({ right =[%p pone] :: _; _ } as __iargs__)
                  -> [%e arg] [%e eone] )]
    @:: ast stream

  | POS_IMPLICIT_FRAG (n,pos) ->
    let pargs = gp "iargs" ~loc in
    let eargs = ge "iargs" ~loc in
    let arg = [%expr nth [%e eargs] [%e nat pos]] in
    [%expr Captured (Size.Z, fun [%p pargs] -> [%e positional n arg] )]
    @:: ast stream
  | FULL_BREAK n ->
    [%expr Point_tag(Defs.Full_break, [%e B.eint ~loc n])] @:: ast stream
  | _ -> assert false


let build ~loc ~path:_ s =
  let frag = ast @@ stream Lex.main loc s in
  [%expr Metafmt.Interpolation.( [%e frag ] ) ]





let extension =
  let open Extension in
  declare "fmt" Expression
    Ast_pattern.(single_expr_payload @@ estring __ )
    build

let () = Ppx_driver.register_transformation name ~extensions:[extension]
