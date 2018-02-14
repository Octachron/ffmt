
module F = Format
module E = Engine
type printer = E.t F.captured
let id x = x

let open_printer _ _ _ = (), id
let close_printer _ = (), id
let null = {
  Spec.data = ();
  box = (fun _data _tag _tag_data -> None);
  open_printer;
  close_printer
}


let box = {
  Spec.data = ();
  box= (fun (type a) () (tag: a F.tag)  (i:a): F.box option -> Some(match tag with
    | F.B -> B i
    | F.H -> H
    | F.V -> V i
    | F.HV -> HV i
    | F.HoV -> HoV i
    | _ -> raise F.Unknown_tag));
  open_printer;
  close_printer;
}
