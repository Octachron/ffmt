open Metafmt
let fprintf = Printf.fprintf

let stdout = Formatter.chan ~tags:[Tagsem.box; Ansi.sem] stdout

;; fprintf [%fmt "Int %d"] [1] stdout
