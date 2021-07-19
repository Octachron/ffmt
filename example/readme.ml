open Ffmt
let stdout = Formatter.chan ~tags:[Tagsem.box; Ansi.sem] stdout
let fp = Ffmt.Printf.fprintf

let stdout =
    stdout
    |> fp {%fmt|Hello %s! |} ["Earth"]
    |> fp {%fmt|%d + %g = %s|} [1;2.;"3"]

