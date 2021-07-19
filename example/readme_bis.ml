open Ffmt
let stdout = Formatter.chan ~tags:[Tagsem.box; Ansi.sem] stdout
let fpi = Iprintf.fprintf

let stdout =
  let open Combinators in
  let earth = string "Earth" in
  let one = int 1 in
  stdout
  |> fpi {%fmt|Hello %{earth}! |} ["Earth"]
  |> fpi {%fmt|%{one} + %{float 2.} = %{string "3"}|} [1;2.;"3"]
