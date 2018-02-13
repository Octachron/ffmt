type t = { margin: int; max_indent:int }
type geometry = t
let default = { margin = 78; max_indent=68 }


module Indentation = struct
  type t = { line: int; column:int }
  type indentation = t
  let zero = { line=0; column=0 }
end

type position = {
  line : int;
  column : int;
  indent : Indentation.t;
}

let start = { column=0;
              line = 0;
              indent=Indentation.zero}