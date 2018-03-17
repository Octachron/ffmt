(** Combinators for building printers with interpolation format*)

open Interpolation
module E = Engine


module Handwritten = struct
  let ($) n x = Captured (n,x)
  let keep f = Size.Z $ f
  let (!$) x = (keep @@ fun _iargs -> x)

  let (%:) n typ =  (keep @@ fun iargs -> typ iargs.%(n) )
  let (%:%) n p = ( keep @@ fun iargs -> iargs.%(p) iargs.%(n))

  let take f = Size.(S Z), fun x -> let elt, _ = current x in f elt
  let (!) (k,f) = k $ f
  let skip (k,f) =  Size.(S k), fun x -> let _, x =current x in f x

  let (!<) (tag,data): _ token = Open_tag (tag,data)

  let (!>) (tag,_) = Close_tag tag

  let (!%) x = !(take x)
  let (!%%) x = !(skip @@ take x )
  let (!%%%) x = !(skip @@ skip @@ take x)
  let l x = Literal x
end
