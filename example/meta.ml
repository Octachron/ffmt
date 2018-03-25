open Metafmt
let fprintf = Printf.fprintf
let flush = Formatter.flush

open Metafmt
open Combinators

let test_out =
  Formatter.chan
    ~geometry:Geometry.{margin=20; box_margin=10; max_indent=15}
    stdout

let test' world =
  fprintf
    [%fmt "@{<box 1>123: @{<box 1>今日は %s N°%d!@ %s%d$1?@ \
           π=%a or %f$4!@}@ \
           a list:@ @{<v 2>1@ 23@ 4567@ 89ABCDE@ 123456@}@ abcdef@}\
           @{<hv 0>NCKLQ@ LAKCM@ ABCDEF@ CNKSS@ XLAXMA@}\
          "
    ]
    [world; 1; "How is your day N°"; float; 3.1415926535 ]


let structural =
  fprintf [%fmt "@[<box 1>12@[<v 1> -5@ -6@ -7@]@ 8@]"] []

type vec = { x:int; y:int}
let ( +: ) x y = {x;y}

let pp_vec {x;y} = fprintf [%fmt "{x:%d;y:%d}"] [x;y]
(*
let vec =
  fprintf
    [%fmt "@{<v 2> %_{ int _*} %a$0@ %a$0@ %a$0@ %a$0@}"]
    [pp_vec; 1; 1 +: 2; 3 +: 4; 5 +: 6; 7 +: 8]
*)
let buffer () =
  Formatter.buffer
    ~geometry:Geometry.{margin=20; box_margin=10; max_indent=15}
    (Buffer.create 17)

let stdout = Formatter.chan ~tags:[Tagsem.box; Ansi.sem] stdout

let explain got expected ppf =
  if got <> expected then
    let len = min (String.length got) (String.length expected) in
    let i = ref 0 in
    if len <> 0 then while  got.[!i] = expected.[!i] && !i < len -1
      do incr i done;
    let sub s = String.sub s (max 0 (!i-10)) (min (!i + 10) (len- !i)) in
    fprintf
      [%fmt "@{<v 0>all:@,%s@,got %d$3     :@,%s@,expected %d$3:@,%s@}"]
          [got; sub got; sub expected;!i] ppf
  else ppf

let semicolumn =
  fprintf
    [%fmt "@[<hov 0>@[<v 0>le@,la@,les@]@ \
           @[<v 0>der@,die@,die@]@ @[<v 0>the@,the@,the@]@]"]
    []

let exec (name,x,expected) =
  let fmt = x @@ buffer () in
  let res = Formatter.flush fmt in
  let green = Ansi.( Fg, {base=Green; bright=true} ) in
  let red = Ansi.( Fg, {base=Red; bright=true} ) in
  let bold = Ansi.bold in
  let u, fk, i, crossed = Ansi.(u,fk,i, crossed) in
  let status = if res = expected then
      fprintf [%fmt "@{<crossed>@{<green>[OK]@}@}"] []
    else
      fprintf [%fmt "@{<red>[FAILURE]@}"] [] in
  fprintf
    [%fmt "@{<v 0> @{<fk>@{<u>Test@}@} @{<i>@{<bold>[%s]@}@}:%t@,%t@}"]
    [name;status; explain res expected] stdout |> ignore

let full_break = fprintf
    [%fmt "@{<hov 2>x@ @{<hov 2>y@\nz@}@ w@\nt@}"] []

let box_margin = fprintf
    [%fmt "@{<hov 1>break here:@ @{<hov 0>new box@}@}"] []
(* the second hov box triggers the upstream break to avoid opening
   beyond the box margin *)

let box_margin_2 = fprintf
    [%fmt "@{<hov 1>reset break here:@{<hov 0>new box@}@}"] []
(* the second hov box is rejected to the left *)

let rec nested: 'n. int -> int -> ('a,'n) Formatter.t -> ('a,'n) Formatter.t =
  fun indent n ppf ->
  if n = 0 then
    fprintf [%fmt "@[<v indent>level 0@ [@ item@ item@ ]@]"] [] ppf
  else
    fprintf
      [%fmt "@[<v indent>level %d@,[@,%t$1@,%t$1@,]@]"]
      [n; nested indent (n-1)]
      ppf

let fmt = [%fmt "%s$4 %d %d"]
let fmt2 = fmt

let fmt3 = fprintf
    Fmt.(fmt ^^ fmt2) [1;2;3;4;"⇒"]

let break_all =
  fprintf
    [%fmt "@[<hv 0>x@ y@ z@ w@\nt@]"] []

let boxes_in_hv =
  fprintf
    [%fmt "@[<hv 0>か@ ら@ ま@ や@ @[<hov 0>ひ@ き@ し@ み@]@ こ@ の@ も@]"]
    []


let hv_boxes_in_hv =
  fprintf
    [%fmt "@[<hv 0>ꙮ@ 05@ @[<hv 0>08@ 11@ 14@ 17@ 20+@]@ в@ у@ щ@]"]
    []

let v_boxes_in_hv =
  fprintf
    [%fmt "@[<hv 2>@ 𒆍 @ ---@[<v 0>𒀭 @,𒊏 @]@ 𒆠 @ end@]"]
    []


let overfit_v_boxes_in_hv =
  fprintf
    [%fmt "@[<hv 2>@ Arma@ @[<v 0>virumque@,cano@,Troiæ qui primus ab@]\
           @ oris@]"]
    []


let v_in_hv_in_hv =
  fprintf
    [%fmt "@[<hv 2>@ Arma@ @[<hv 0>virumque@ @[<v 0>cano@,\
           Troiæ@]@ qui@ primus@ ab@]\
           @ oris@]"]
    []

let try0 = fprintf
    [%fmt "@[<try'>abcdefghijklmnopqrstuvwxyz@]@[<else'>α@]"] []

let hh = fprintf
  [%fmt "@[<v 2>@,\
         @[<try'>123456@]@[<then'>@,@]\
         @[<try'>12345678@]@[<then'>@,@]\
         @[<try'>abcdefghijklmnopqrstuvwxyz@]@[<else'>β@]@,\
         stop@,@]"
  ] []

let () =
  List.iter exec [
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
{|⇒ 1 2⇒ 3 4|};
(*"Common printer", vec,
{| 1 {x:1;y:2}
  {x:3;y:4}
  {x:5;y:6}
  {x:7;y:8}|};*)
"Nested boxes in hv boxes", boxes_in_hv,
{|か
ら
ま
や
ひ き し み
こ
の
も|};
"Hv boxes in hv boxes", hv_boxes_in_hv,
{|ꙮ
05
08 11 14 17 20+
в
у
щ|};
"Semi-column", semicolumn,
{|le
la
les der
    die
    die the
        the
        the|};
"V boxes in hv box", v_boxes_in_hv,
{|
  𒆍 
  ---𒀭 
     𒊏 
  𒆠 
  end|};
"Overfit V box in hv box", overfit_v_boxes_in_hv,
{|
  Arma
  virumque
  cano
  Troiæ qui primus ab
  oris|};
"V box in HV in HV", v_in_hv_in_hv,
{|
  Arma
  virumque
  cano
  Troiæ
  qui
  primus
  ab
  oris|};
"Fit or hide", hh,
{|
  123456
  12345678
  β
  stop
  |};
"try 0", try0,{|α|}
]
