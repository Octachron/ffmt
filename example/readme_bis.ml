open Ffmt
let stdout = Formatter.chan ~tags:[Tagsem.box; Ansi.sem] stdout
let fpi x = Iprintf.fprintf x []

let stdout =
  let open Combinators in
  let earth = string "Earth" in
  let one = int 1 in
  stdout
  |> fpi {%fmt|Hello %{earth}! |}
  |> fpi {%fmt|%{one} + %{float 2.} = %{string "3"}|}
