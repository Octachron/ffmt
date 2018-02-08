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

let (!<) (tag,data): _ token = Open_tag {tag;data}

let (!>) tag = Close_tag tag

let (!%) x = Captured(take x)
let (!%%) x = Captured(skip @@ take x )
let (!%%%) x = Captured(skip @@ skip @@ take x)

let box n = B, 1

let v = V,2

let b = Break{space=1; indent=0}

let l x = Literal x
let test ppf s =
  eval ppf
    [ !<(box 1); l"123:";
      !<(box 1); l[%fmt ""]; l" "; !$(string s); l" N°"; (Z %: int); l"?";
      b;
      (S Z %: string ); !%int; l"?";
      b;
      l"π="; (S (S (S Z))) %:% S (S Z); l" or "; !%%%float ; l"!";
      !> B;
      b; l "a list:";b; (*l"123";*)
      (*!<v; l "123"; !>V; l " out";*)
      !<v; l" 1";b; l "2";b; l "3"; b; l"4567890123456789";b; l"A"; !> V; b; l" out";
      !> B
    ]
    [1;"How is your day N°"; float; 3.1415926535 ]


let test' ppf world = "Hello %{s world} N° %d$0! %s$1%d? π=%{3s 2s}"

let stdout =
  Formatter.chan
    ~geometry:Geometry.{margin=20;max_indent=10}
    Pervasives.stdout

let () = test stdout "world"; print_newline ()
