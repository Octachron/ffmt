open Ppx_core.Light

let name = "stringi-ppx"

let build ~loc:_ ~path:_ s = [%expr "Hello"]

let extension =
  let open Extension in
  declare "fmt" Expression
    Ast_pattern.(single_expr_payload @@ estring __ )
    build

let () = Ppx_driver.register_transformation name ~extensions:[extension]
