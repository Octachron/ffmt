
module F = Format
module E = Engine
type printer = E.t F.captured
let id x = x
module Null = struct
  type data = unit
  type nonrec printer = printer
  let init () = ()
  let box _data _tag _tag_data= None
  let open_printer _ _ _ = (), id
  let close_printer _ =  (), id
end


module Box = struct
  type data = unit
  type nonrec printer = printer
  let init () = ()
  let box (type a) () (tag: a F.tag)  (i:a): F.box option = Some(match tag with
    | F.B -> B i
    | F.H -> H
    | F.V -> V i
    | F.HV -> HV i
    | F.HoV -> HoV i
    | _ -> raise F.Unknown_tag)

  let open_printer _ _ _ = (), id
  let close_printer _ =  (), id
end


let null =
  (module Null: Spec.tag_semantic with type data = unit
                                       and type printer = printer  )

let box =
  (module Box: Spec.tag_semantic with type data = unit
                                       and type printer = printer  )
