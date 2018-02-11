
open Format

let keep f x = f x, x
let (!) x = Captured x
let (!$) x = (keep @@ fun iargs -> x)


let (%:) n typ =  (keep @@ fun iargs -> typ iargs.%(n) )
let (%:%) n p = ( keep @@ fun iargs -> iargs.%(p) iargs.%(n))

let take f x = let elt, x = current x in f elt, x

let skip f x = let _, x =current x in f x

let (!<) (tag,data): _ token = Open_tag (tag,data)

let (!>) (tag,_) = Close_tag tag

let (!%) x = (take x)
let (!%%) x = (skip @@ take x )
let (!%%%) x = (skip @@ skip @@ take x)



let box n = B, n
let b0 = box 0

let v = V,2
let hv = HV, 0


let b = Break{space=1; indent=0}

let l x = Literal x

let string = Formatter.string
let int d = string (string_of_int d)
let float f = string (string_of_float f)
let bool b = if b then string "true" else string "false"

let list elt l ppf =
  let rec elts elt (l: _ list) ppf =
  match l with
    | [] -> ppf
    | [a] -> elt a ppf
    | a :: q -> ppf |> elt a |> string ";" |> elts elt q in
  ppf |> string "[" |> elts elt l |> string "]"
