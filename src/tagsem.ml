module D = Defs
module F = Core

type open_tag = Core.open_tag =
    Open_tag : {tag: 'any D.tag; with_box: bool } -> open_tag

type sem = Core.sem
class type semclass = Core.semclass
type ('a,'b) printer = ('a,'b) F.t D.endo


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
    method mine: type any. any D.tag -> bool =
      function
      | D.B -> true | D.V -> true | D.HV -> true | D.HoV -> true | D.H -> true
      | D.Break -> true | D.Full_break -> true
      | _ -> false
    method box: type a. a D.tag -> a -> D.box option = fun tag i ->
      match tag with
      | D.B -> Some (B i)
      | D.H -> Some H
      | D.V -> Some (V i)
      | D.HV -> Some (HV i)
      | D.HoV -> Some (HoV i)
      | _ -> None

    method break: type a. a D.tag -> a -> D.break option = fun tag i ->
      match tag with
      | D.Break -> let space, indent = i in Some (D.Break {space; indent})
      | D.Full_break -> Some (D.Full_break i)
      | _ -> None
    method open_printer: 'any 'b 'final.  'any D.tag -> 'any
      -> 'self * ('final,'b) printer = fun _ _ -> {< >}, id
    method close_printer: 'final 'b. 'self * ('final,'b) printer = {< >}, id
  end
