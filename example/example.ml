open Stringi
open Combinators

let test s ppf =
  let open Handwritten in
  let b = Format.Break { space=1; indent = 0 } in
  let b0: _ Format.tag * _ = Format.B, 0 in
  let v = v 2 and hv = hv 0 in
  Formatter.eval ppf
    [ !<(box 1); l"123:";
      !<(box 1); l"Hello"; l" "; ! !$(string s); l" N°"; !(Z %: int); l"?";
      b;
      !(S Z %: string ); ! !%int; l"?";
      Full_break 0;
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

let stdout =
  Formatter.chan
    ~geometry:Geometry.{margin=20;max_indent=10}
    Pervasives.stdout

let exec x = x stdout |> ignore; print_newline ()

let full_break ppf = Formatter.eval ppf
    [%fmt "@{<hov 2>x@ @{<hov 2>y@\nz@}@ w@\nt@}"] []

let () =
  List.iter exec [
    test "world";
    test' "κοσμοσ";
    structural;
    full_break ]
