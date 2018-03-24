type modal =
  { alternative: char;
    precision: int;
    paddle: int;
  }

type yes = private Yes
type no = private No

module Variant = struct

  type hex = Std | Hash
  type justification = Left | Right
  type sign = No | Plus
  type padding = Space | Zero
  type int =
    | Hex of {capitalized:bool; hex:hex; align:justification; padding:padding }
    | Octa of {hex:hex; align:justification; padding:padding }
    | Unsigned of {align:justification; padding: padding }
    | Signed of { align:justification; padding: padding; sign: sign }


  let int = Signed { align=Right; padding=Space; sign=No }

  type float =
    | Float of { reflect: bool; align:justification; padding:padding }
    | Float_hex of {capitalized:bool; align:justification; padding:padding }
    | Float_e of { align:justification; padding:padding }
    | Float_g of { align:justification; padding:padding }

  let float = Float { reflect=false; align=Left; padding= Space }

  type string = { align:justification; reflect: bool }

  let string = { align=Left; reflect=false }

end


type _ integer_core =
  | Int: int integer_core
  | Int32: int32 integer_core
  | Int64:  int64 integer_core
  | Native_int: nativeint integer_core

(*
let string_of_num (type x) ~padding ~precision (x: num_core) (x:x)
*)

type (_,_) typ =
  | String: (string, no) typ

  | Int: (int,yes) typ
  | Bool: (bool,no) typ
  | Int32: (int32,yes) typ
  | Int64: (int64,yes) typ
  | Nativeint: (nativeint,yes) typ

  | Float: (float,no) typ

  | Char: (char,no) typ
  | Uchar: (Uchar.t,no) typ

  | List: ('a,'b) typ -> ('a list,'b) typ
  | Array: ('a,'b) typ -> ('a array,'b) typ
  | Option: ('a,'b) typ -> ('a option,'b) typ

module Ft = Core

type 'p mapper =
  ('a,'n) Ft.t -> ('a,'n2) Ft.t
  constraint 'p = 'a * ('n -> 'n2)

type ('x,'p) printer = 'x -> 'p mapper


type (_,_,_) index =
  | Z: ('elt,'elt -> 't, 't ) index
  | S: ('elt, 'list, 't) index -> ('elt, 'any -> 'list, 't ) index


type (_,_,_,_) pos =
  | Relative: ('x,'right, 'right2 ) index -> ('x, 'list, 'right, 'right2) pos
  | Absolute: ('x,'list, _ ) index -> ('x,'list, 'right,'right) pos
  | Int_constant: int option  -> (int, 'list, 'right, 'right) pos

type ('a,'b) eq = Refl: ('a,'a) eq
type empty = (int,float) eq

type _ args =
  | []: empty args
  | (::): 'a * 'list args -> ('a -> 'list) args

type ('all,'right) iargs =
  { all: 'all args; right: 'right args }


let rec nth: type x l a. (x,l,a) index -> l args -> x =
  fun pos l -> match pos, l with
    | Z, a :: _ -> a
    | S n, _ :: q -> nth n q

let rec take_nth: type x l r. (x,l,r) index -> l args -> x * r args =
  fun pos l -> match pos, l with
    | Z, a :: q -> a, q
    | S n, _ :: q -> take_nth n q

let take: type x l r r2. (l,r) iargs -> (x,l,r,r2) pos -> x * (l,r2) iargs =
  fun iargs pos ->
    match pos with
    | Absolute k -> nth k iargs.all, iargs
    | Relative k -> let x, right = take_nth k iargs.right in
      x, { iargs with right }
    | Int_constant (Some x) -> x, iargs
    | Int_constant None -> 0, iargs

type 'all default = 'all
  constraint 'all = < list: 'a; pos:'b -> 'c; fmt:'d; tag_count:'e -> 'f >

type 'all unread = 'all
  constraint 'all = < pos:'b -> 'b; .. > default

type 'all notag = 'all
  constraint 'all = < tag_count: 'a -> 'a; ..> default


type 'all nofmt = 'all
  constraint 'all = < fmt:'any; ..> default

type 'a s = 'a Ft.s
type z = Ft.z

let implicit = Int_constant None

type _ token =
  | Literal: string -> _ notag unread token
  | Open_tag: ('data Defs.tag * 'data) ->
    < tag_count: 'n -> 'n s; .. > unread token
  | Close_tag: _ Defs.tag -> < tag_count: 'e s -> 'e; .. > unread token
  | Close_any_tag: <tag_count: 'e s -> 'e; .. > unread token
  | Point_tag: ('data Defs.tag * 'data) -> _ notag unread token
(*  | Hole: ('l, 'r -> 'r2, 'n -> 'n2, 'x ) elt ->
    <list:'l; pos:'r -> 'r2; fmt:'x; tag_count:'n -> 'n2 > token*)
  | S: ('a,'l,'r,'r2) pos * ('a,_) typ
    -> <list:'l; pos:'r -> 'r2; .. > notag token
  | Alpha:
      (('a, 'x * 'fn) printer ,'l,'r,'r2) pos *
      ( 'a ,'l,'r2,'r3) pos
    -> <list:'l; pos:'r -> 'r3; tag_count: 'fn; fmt: 'x > default token
  | Theta:
      (('x * 'fn) mapper ,'l,'r,'r2) pos
    -> <list:'l; pos:'r -> 'r2; tag_count: 'fn; fmt: 'x > default token
  | Integer: {core: 'x integer_core; variant:Variant.int;
              padding:(int, 'l,'r,'r2) pos;
              pos:('x,'l,'r2,'r3) pos
             } ->
    <list:'l; pos:'r -> 'r3; ..> notag token
  | Float: {variant: Variant.float;
            padding:(int, 'l,'r,'r2) pos;
            precision:(int,'l,'r2,'r3) pos;
            pos:(float,'l,'r3,'r4) pos } ->
    <list:'l; pos:'r -> 'r4; ..> notag token

  | String:
      { variant: Variant.string
      ; padding:(int,'l,'r,'r2) pos
      ; pos:(string,'l,'r2,'r3) pos
      }
    -> <list:'l;pos:'r -> 'r3; .. > notag token

type _ t =
  | []:
    <all:'any; right:'right; tail:'right; fmt:'fmt; tag_count:'a -> 'a > t
  | (::):
      <list:'list; pos:'right -> 'right2; fmt:'fmt; tag_count: 'a -> 'b > token
      * <all:'list; right:'right2; tail: 'tail; fmt: 'fmt;
         tag_count:'b -> 'c > t
    ->
    <all:'list; right:'right; tail:'tail; fmt:'fmt;
     tag_count:'a -> 'c > t

let rec (^^): type left right b a k l m fmt.
  <all:a; right:left;  tail:right; fmt:fmt; tag_count: k -> l > t ->
  <all:a; right:right; tail:b;     fmt:fmt; tag_count: l -> m > t ->
  <all:a; right:left;  tail:b;     fmt:fmt; tag_count: k -> m > t =
  fun l r -> match l with
    | [] -> r
    | Open_tag t :: q -> Open_tag t :: (q ^^ r)
    | Close_tag c :: q -> Close_tag c :: (q ^^ r)
    | Close_any_tag as t :: q -> t :: (q ^^ r)
    | Point_tag t :: q -> Point_tag t :: (q ^^ r)
    | Literal t :: q -> Literal t :: (q ^^ r)
    | S (x,y) :: q -> S(x,y) :: (q ^^ r)
    | Alpha (k,l) :: q -> Alpha(k,l) :: ( q ^^ r)
    | Theta k :: q -> Theta k :: ( q ^^ r)
    | Integer x :: q -> Integer x :: (q ^^ r )
    | Float x :: q -> Float x :: (q ^^ r)
    | String x :: q -> String x :: (q ^^ r)
let make x = { all=x; right = x}
