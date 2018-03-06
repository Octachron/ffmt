module F = Format

type open_tag = Formatter_def.open_tag =
    Open_tag : {tag: 'any Format.tag; with_box: bool } -> open_tag

type sem = Formatter_def.sem
class type semclass = Formatter_def.semclass
type 'a printer = 'a Formatter_def.t Format.captured


let rec find_sem:type any.
  sem list -> any F.tag -> sem list
  -> (sem * sem list) option =
  fun rest tag -> function
    | [] -> None
    | x :: q ->
      if x#mine tag then Some (x, List.rev_append rest q)
      else find_sem (x :: rest) tag q

let find_sem x = find_sem [] x

let id x = x

let open_printer _ _ _ = (), id
let close_printer _ = (), id
class null = object(_:'self)
  constraint 'self = #semclass
  method mine _ = false
  method box _tag _tag_data = None
  method break _tag _tag_data = None
  method open_printer _ _ = {< >}, id
  method close_printer = {< >}, id
end


let box =
  object(_:'self)
    constraint 'self = #semclass
    method mine: type any. any F.tag -> bool =
      function
      | F.B -> true | F.V -> true | F.HV -> true | F.HoV -> true | F.H -> true
      | F.Break -> true | F.Full_break -> true
      | _ -> false
    method box: type a. a F.tag -> a -> F.box option = fun tag i ->
      match tag with
      | F.B -> Some (B i)
      | F.H -> Some H
      | F.V -> Some (V i)
      | F.HV -> Some (HV i)
      | F.HoV -> Some (HoV i)
      | _ -> None

    method break: type a. a F.tag -> a -> F.break option = fun tag i ->
      match tag with
      | F.Break -> let space, indent = i in Some (F.Break {space; indent})
      | F.Full_break -> Some (F.Full_break i)
      | _ -> None
    method open_printer: 'any 'final.  'any F.tag -> 'any
      -> 'self * 'final printer = fun _ _ -> {< >}, id
    method close_printer: 'final. 'self * 'final printer = {< >}, id
  end
