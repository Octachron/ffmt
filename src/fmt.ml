type modal =
  { alternative: char;
    precision: int;
    paddle: int;
  }

type yes = private Yes
type no = private No

type (_,_) typ =
  | String: (string, no) typ

  | Int: (int,yes) typ
  | Int32: (int32,yes) typ
  | Int64: (int64,yes) typ
  | Nativeint: (nativeint,yes) typ

  | Float: (float,no) typ

  | Char: (char,no) typ
  | Uchar: (Uchar.t,no) typ

  | List: ('a,'b) typ -> ('a list,'b) typ
  | Array: ('a,'b) typ -> ('a array,'b) typ
  | Option: ('a,'b) typ -> ('a option,'b) typ

module Ft = Formatter_def

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

type (_,_,_) modifier =
  | One: (int, 'list,'right,'right2) pos ->
    ('list, 'right, 'right2) modifier
  | Two:
      (int, 'list,'right,'right2) pos * (int,'list,'right2, 'right3) pos ->
    ('list, 'right, 'right3) modifier

type (_,_,_,_) elt =
  | S: ('a,'l,'r,'r2) pos * ('a,_) typ -> ('l, 'r -> 'r2, 'x -> 'x, 'any) elt
  | Alpha:
      (('a, 'x * 'fn) printer ,'l,'r,'r2) pos *
      ( 'a ,'l,'r2,'r3) pos
    -> ('l,'r -> 'r3, 'fn, 'x) elt
  | Theta:
      (('x * 'fn) mapper ,'l,'r,'r2) pos  -> ('l,'r->'r2 , 'fn,'x) elt
  | Star:
      ('l, 'r, 'r2) modifier
      * ('x,yes) typ * ('x,'l,'r2,'r3) pos
    -> ('l,'r -> 'r3 ,'x -> 'x, 'any) elt



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
  | Star:
      ('l, 'r, 'r2) modifier
      * ('x,yes) typ * ('x,'l,'r2,'r3) pos
    -> <list:'l; pos: 'r -> 'r3; .. > notag token


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
    | Star(m,t,k) :: q -> Star (m,t,k) :: ( q ^^ r)

let make x = { all=x; right = x}