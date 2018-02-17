open Stringi
open Combinators

let test s ppf =
  let open Handwritten in
  let b = Format.Point_tag (Format.Break, (1,0)) in
  let fb n = Format.Point_tag(Format.Full_break, n) in
  let b0: _ Format.tag * _ = Format.B, 0 in
  let v = v 2 and hv = hv 0 in
  Formatter.eval ppf
    [ !<(box 1); l"123:";
      !<(box 1); l"Hello"; l" "; !$(string s); l" N°"; Z %: int; l"?";
      b;
      S Z %: string; !%int; l"?";
      fb 0;
      l"π="; (S (S (S Z))) %:% S (S Z); l" or "; !%%%float ; l"!";
      !> b0;
      b; l "a list:";b; (*l"123";*)
      (*!<v; l "123"; !>V; l " out";*)
      !<v; l"1";b; l "23"; b; l"4567"; b; l"89ABCDE"; b;
      l"123456"; !> v; b; l "abcdef";
      !> b0;
      !<hv; l"NCKLQ"; b; l"LAKCM"; b; l"ABCDEF"; b; l"CNKSS"; b; l"XLAXMA"; !>hv;
    ]
    [1;"How is your day N°"; float; 3.1415926535 ]

let test' world ppf =
  Formatter.eval ppf
    [%fmt "@{<box 1>123: @{<box 1>今日は %{string world} N°%d!@ %s$1%d$0?@ \
           π=%{$2 $3} or %{float __*}!@}@ \
           a list:@ @{<v 2>1@ 23@ 4567@ 89ABCDE@ 123456@}@ abcdef@}\
           @{<hv 0>NCKLQ@ LAKCM@ ABCDEF@ CNKSS@ XLAXMA@}\
          "
    ]
    [1; "How is your day N°"; float; 3.1415926535 ]


let structural ppf =
  Formatter.eval ppf [%fmt "@[<box 1>12@[<v 1> -5@ -6@ -7@]@ 8@]"] []

let buffer b =
  Formatter.buffer
    ~geometry:Geometry.{margin=20; box_margin=10; max_indent=15}
    b

let stdout = Formatter.chan ~tags:[Semantics.box; More_semantics.ansi] stdout

let explain got expected ppf =
  if got <> expected then
    let len = min (String.length got) (String.length expected) in
    let i = ref 0 in
    while  got.[!i] = expected.[!i] do incr i done;
    let sub s = String.sub s (max 0 (!i-10)) (min (!i + 10) (len- !i)) in
    Formatter.eval ppf
      [%fmt "@{<v 0>all:@,%s@,got %d$3     :@,%s@,expected %d$3:@,%s@}"]
          [got; sub got; sub expected;!i]
  else ppf

let exec (name,x,expected) =
  let b = Buffer.create 17 in
  x (buffer b) |> ignore;
  let res = Buffer.contents b in
  let green = More_semantics.( Fg, Green ) in
  let red = More_semantics.( Fg, Red ) in
  let status ppf = if res = expected then
      Formatter.eval ppf [%fmt "@{<green>[OK]@}"] []
    else
      Formatter.eval ppf [%fmt "@{<red>[FAILURE]@}"] [] in
  Formatter.eval stdout
    [%fmt "@{<v 0> Test [%{string name}]:%t@,%t@}"]
    [status; explain res expected] |> ignore

let full_break ppf = Formatter.eval ppf
    [%fmt "@{<hov 2>x@ @{<hov 2>y@\nz@}@ w@\nt@}"] []

let box_margin ppf = Formatter.eval ppf
    [%fmt "@{<hov 1>break here:@ @{<hov 0>new box@}@}"] []
(* the second hov box triggers the upstream break to avoid opening
   beyond the box margin *)

let box_margin_2 ppf = Formatter.eval ppf
    [%fmt "@{<hov 1>reset break here:@{<hov 0>new box@}@}"] []
(* the second hov box is rejected to the left *)

let rec nested indent n ppf =
  if n = 0 then Formatter.eval ppf
      [%fmt "@[<v indent>level 0@ [@ item@ item@ ]@]"] []
  else
    Formatter.eval ppf
      [%fmt "@[<v indent>level %d@,[@,%{nested indent (n-1)}\
             @,%{nested indent(n-1)}@,]@]"]
      [n]

let fmt = [%fmt "%s$4 %d %d"]
let fmt2 = fmt

let fmt3 ppf = Formatter.eval ppf
    Format.(fmt ^^ fmt2) [1;2;3;4;"⇒"]

let break_all ppf =
  Formatter.eval ppf
    [%fmt "@[<hv 0>x@ y@ z@ w@;<1000 0>t@]"] []

let () =
  List.iter exec [
    "Handwritten", test "world",
{|123:Hello world N°1?
     How is your day N°1?
     π=3.1415926535 or 3.1415926535!
 a list: 1
           23
           4567
           89ABCDE
           123456
 abcdefNCKLQ
       LAKCM
       ABCDEF
       CNKSS
       XLAXMA|}
;
"Ppx", test' "κοσμοσ",
{|123: 今日は κοσμοσ N°1!
      How is your day N°1?
      π=3.1415926535 or 3.1415926535!
 a list: 1
           23
           4567
           89ABCDE
           123456
 abcdefNCKLQ
       LAKCM
       ABCDEF
       CNKSS
       XLAXMA|};
"Structural box", structural,
{|12 -5
   -6
   -7
 8|};
"Full break", full_break,
{|x
  y
    z w
  t|};
"Box margin", box_margin,
{|break here:
 new box|};
"Box margin 2", box_margin_2,
{|reset break here:
new box|};
"Nested", nested 7 2,
{|level 2
       [
       level 1
              [
              
level 0
       [
       item
       item
       ]
level 0
       [
       item
       item
       ]
]
       level 1
              [
              
level 0
       [
       item
       item
       ]
level 0
       [
       item
       item
       ]
]
       ]|};
"Long break", break_all,
{|x
y
z
w
t|};
"Format concatenation", fmt3,
{|⇒ 1 2⇒ 3 4|}
]
