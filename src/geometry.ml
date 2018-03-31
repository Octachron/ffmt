type t = {
  box_margin:int;
  max_indent:int;
  margin: int;
}
type geometry = t

let default = { margin = 78; box_margin=50; max_indent=68 }

let ( *. ) scalar g =
  let iof = int_of_float in
  { margin = iof (scalar *. float g.margin);
    box_margin = iof (scalar *. float g.box_margin);
    max_indent = iof (scalar *. float g.max_indent)
  }

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
