module F = Format

type ('data,'printer) typed =  {
  mine: 'any. 'any F.tag -> bool;
  box: 'any. 'data -> 'any F.tag -> 'any -> F.box option;
  break: 'any. 'data -> 'any F.tag -> 'any -> F.break option;
  open_printer: 'any. 'data -> 'any F.tag -> 'any
    -> 'data * 'printer F.captured;
  close_printer: 'data -> 'data * 'printer F.captured;
  data :'data
}


type 'printer t = T: ('data,'printer) typed ->
  'printer t [@@unboxed]

type open_tag = Open_tag : {tag: 'any F.tag; with_box: bool } -> open_tag

let rec find_sem:type any p.
  p t list -> any F.tag -> p t list
  -> (p t * p t list) option =
  fun rest tag -> function
    | [] -> None
    | T x :: q ->
      if x.mine tag then Some (T x, List.rev_append rest q)
      else find_sem (T x :: rest) tag q

let find_sem x = find_sem [] x

let id x = x

let open_printer _ _ _ = (), id
let close_printer _ = (), id
let null = {
  data = ();
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
  T {
    data = ();
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
