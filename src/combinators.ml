(** Combinators for building printers *)

open Defs
module E = Engine


let box n = B, n

let b n = B, n
let v n = V, n
let hv n = HV, n
let hov n = HoV, n

let break ~space ~indent = Formatter.break {space;indent}
let space x = break ~space:1 ~indent:0 x


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
  let elts elt x =
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
