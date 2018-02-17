(** Combinators for building printers *)

open Format
module E = Engine

module Handwritten = struct
  let ($) n x = Captured (n,x)
  let keep f = Size.Z $ f
  let (!$) x = (keep @@ fun iargs -> x)

  let (%:) n typ =  (keep @@ fun iargs -> typ iargs.%(n) )
  let (%:%) n p = ( keep @@ fun iargs -> iargs.%(p) iargs.%(n))

  let take f = Size.(S Z), fun x -> let elt, x = current x in f elt
  let (!) (k,f) = k $ f
  let skip (k,f) =  Size.(S k), fun x -> let _, x =current x in f x

  let (!<) (tag,data): _ token = Open_tag (tag,data)

  let (!>) (tag,_) = Close_tag tag

  let (!%) x = !(take x)
  let (!%%) x = !(skip @@ take x )
  let (!%%%) x = !(skip @@ skip @@ take x)
end


let box n = B, n

let b n = B, n
let v n = V, n
let hv n = HV, n
let hov n = HoV, n

let break ~space ~indent = Formatter.break {space;indent}
let space = break ~space:1 ~indent:0

let l x = Literal x

let string = Formatter.string
let int d = string (string_of_int d)
let float f = string (string_of_float f)
let bool b = if b then string "true" else string "false"

let listlike left sep right elt l ppf =
  let rec elts elt (l: _ list) ppf =
  match l with
    | [] -> ppf
    | [a] -> elt a ppf
    | a :: q -> ppf |> elt a |> sep |> elts elt q in
  ppf |> left |> elts elt l |> right

let iterable left sep right iter elt collection ppf =
  let stack = ref None in
  let ppf = ref (left ppf) in
  let rec elts elt x =
    match !stack with
    | None -> stack:= Some x
    | Some prev ->
      ppf := !ppf |> elt prev |> sep;
      stack := Some x in

  let () = iter (elts elt) collection in
  let ppf = match !stack with
    | None -> !ppf
    | Some x -> elt x !ppf in
  right ppf

let (%>) f g x = g(f(x))

let seq printers ppf = List.fold_left (|>) ppf printers

let semicolon ppf = ppf |> string ";" |> space
let comma ppf = ppf |> string "," |> space


let pair ?(parentheses=string "(", string ")") ?(sep=comma) left right (x,y) =
  let lpar, rpar = parentheses in
  seq [ lpar; left x; sep; right y; rpar ]
let list x = listlike (string "[") semicolon (string "]") x
let array elt = iterable (string "[|") semicolon (string "|]")
    Array.iter elt
let set iter elt = iterable (string "{") semicolon (string "}") iter elt
