open Stringi
open Format
open Formatter

let (!$) x = Captured x
let (%:) nth typ = Var{ pos=nth; typ }
let (%:%) nth pth = Ext_var{ data_index = nth; printer_index = pth }
let (!%) x = Implicit_pos x
let l x = Literal x
let test ppf s =
  eval ppf [ l[%fmt ""]; !$(string s); l" N°"; (Z %: int); (S Z %: string ); !%int; l"? π="; (S (S (S Z))) %:% S (S Z) ]
    [1;"! How is your day N°"; float; 3.1415926535 ]


let test' ppf world = "Hello %{s world} N° %d$0! %s$1 %d? π=%($3:$2)"

let () = test stdout "world"
