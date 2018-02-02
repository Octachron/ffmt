open Stringi
open Format
open Formatter

let keep f x = f x, x
let (!$) x = Captured (keep @@ fun iargs -> x)


let (%:) n typ = Captured (keep @@ fun iargs -> typ iargs.%(n) )
let (%:%) n p =
  Captured ( keep @@ fun iargs -> iargs.%(p) iargs.%(n))

let take f x = let elt, x = current x in f elt, x

let skip f x = let _, x =current x in f x

type _ tag += Box: int tag  | Vbox: int tag | Hbox: unit tag

let (!<) (tag,data): _ token = Open_tag {tag;data}

let (!>) tag = Close_tag tag

let (!%) x = Captured(take x)
let (!%%) x = Captured(skip @@ take x )
let (!%%%) x = Captured(skip @@ skip @@ take x)


let l x = Literal x
let test ppf s =
  eval ppf [ !<(Box,1);
             l[%fmt ""]; l" "; !$(string s); l" N°"; (Z %: int); (S Z %: string );
             !%int; l"? π="; (S (S (S Z))) %:% S (S Z); l" or "; !%%%float ; l"!";
             !> Box;
           ]
    [1;"! How is your day N°"; float; 3.1415926535 ]


let test' ppf world = "Hello %{s world} N° %d$0! %s$1%d? π=%{$3 $2}"

let () = test stdout "world"; print_newline ()
