open Stringi
open Combinators

let test ppf s =
  Formatter.eval ppf
    [ !<(box 1); l"123:";
      !<(box 1); l"Hello"; l" "; ! !$(string s); l" N°"; !(Z %: int); l"?";
      b;
      !(S Z %: string ); ! !%int; l"?";
      b;
      l"π="; ! ((S (S (S Z))) %:% S (S Z)); l" or "; ! !%%%float ; l"!";
      !> b0;
      b; l "a list:";b; (*l"123";*)
      (*!<v; l "123"; !>V; l " out";*)
      !<v; l"1";b; l "23"; b; l"4567"; b; l"89ABCDE"; b;
      l"123456"; !> v; b; l "abcdef";
      !> b0;
      !<hv; l"NCKLQ"; b; l"LAKCM"; b; l"ABCDEF"; b; l"CNKSS"; b; l"XLAXMA"; !>hv;
    ]
    [1;"How is your day N°"; float; 3.1415926535 ]


let test' ppf world =
  Formatter.eval ppf
    [%fmt {|123: @{<box 0>今日は κοσμοσ N°%d!@ %s$1%d$0?@ Time flows@}|}]
    [1; "How is your day N°"]


let stdout =
  Formatter.chan
    ~geometry:Geometry.{margin=20;max_indent=10}
    Pervasives.stdout

let () =
  test stdout "world" |> ignore; print_newline ();
  test' stdout "world" |> ignore; print_newline ();
