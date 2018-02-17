
module F = Format
module E = Engine
type printer = E.t F.captured
let id x = x

let open_printer _ _ _ = (), id
let close_printer _ = (), id
let null = {
  Spec.data = ();
  mine = (fun _ -> false);
  box = (fun _data _tag _tag_data -> None);
  break = (fun _data _tag _tag_data -> None);
  open_printer;
  close_printer
}


let box =
  let mine: type any. any F.tag -> bool = function
    | F.B -> true | F.V -> true | F.HV -> true | F.HoV -> true | F.H -> true
    | F.Break -> true | F.Full_break -> true
    | _ -> false in
  Spec.T {
  Spec.data = ();
  mine;
  box= (fun (type a) () (tag: a F.tag)  (i:a): F.box option -> match tag with
    | F.B -> Some (B i)
    | F.H -> Some H
    | F.V -> Some (V i)
    | F.HV -> Some (HV i)
    | F.HoV -> Some (HoV i)
    | _ -> None);
  break = (fun (type a) () (tag: a F.tag) (i:a): F.break option ->
      match tag with
      | F.Break -> let space, indent = i in Some (F.Break {space; indent})
      | F.Full_break -> Some (F.Full_break i)
      | _ -> None
    );
  open_printer;
  close_printer;
}
