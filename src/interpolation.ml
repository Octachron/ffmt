(** Core definition of format string *)
module Def = Formatter_def
type ('a,'b) fmt = ('a,'b) Formatter_def.t



type 'a s = 'a Formatter_def.s
type z = Formatter_def.z

module Size = struct
  type (_,_) t =
    | Z : ('a, 'a) t
    | S: ('list,'last) t -> ( 'one_more -> 'list, 'last) t
end

type ('a,'b) eq = Refl: ('a,'a) eq
type empty = (int,float) eq


type _ args =
  | []: empty args
  | (::): 'a * 'list args -> ('a -> 'list) args

type ('all,'right) iargs =
  { all: 'all args; right: 'right args }

let make x = { all = x; right = x }

let current (type all elt right) (x: (all,elt -> right) iargs) =
  match x.right with
  | a :: q -> a, { x with right = q}


type 'all default = 'all
  constraint 'all = < list: 'a; pos:'b -> 'c; fmt:'d; tag_count:'e -> 'f >

type 'all unread = 'all
  constraint 'all = < pos:'b -> 'b; .. > default

type 'all notag = 'all
  constraint 'all = < tag_count: 'a -> 'a; ..> default

type _ token =
  | Literal: string -> _ notag unread token
  | Captured:
      ('right,'right2) Size.t *
      ( ('list,'right) iargs -> ('a,'b) fmt -> ('a,'c) fmt )
      -> <pos: 'right -> 'right2; list:'list; fmt:'a; tag_count:'b -> 'c > token
  | Open_tag: ('data Def.tag * 'data) ->
    < tag_count: 'n -> 'n s; .. > unread token
  | Close_tag: _ Def.tag -> < tag_count: 'e s -> 'e; .. > unread token
  | Close_any_tag: <tag_count: 'e s -> 'e; .. > unread token
  | Point_tag: ('data Def.tag * 'data) -> _ notag unread token


type _ format =
  | []:
    <all:'any; right:'right; tail:'right; fmt:'fmt; tag_count:'a -> 'a > format
  | (::):
      <list:'list; pos:'right -> 'right2; fmt:'fmt; tag_count: 'a -> 'b > token
      * <all:'list; right:'right2; tail: 'tail; fmt: 'fmt;
         tag_count:'b -> 'c > format
    ->
    <all:'list; right:'right; tail:'tail; fmt:'fmt;
     tag_count:'a -> 'c > format

let rec nth: type elt a. (elt,a) Def.index -> a args -> elt =
  fun n  args -> let open Def in
  match n, args with
  | Z , a :: _ -> a
  | S n, _ :: q -> nth n q

let (.%()) iargs n = nth n iargs.all
let nth x y = x.%(y)

let rec take: type free s e.  (s, e) Size.t -> (free,s) iargs ->
  (free,e) iargs =
  fun n iargs -> match n, iargs.right with
    | Size.Z, _ -> iargs
    | Size.S k, _ :: right -> take k { iargs with right }

let rec (^^): type left right b a k l m fmt.
  <all:a; right:left;  tail:right; fmt:fmt; tag_count: k -> l > format ->
  <all:a; right:right; tail:b;     fmt:fmt; tag_count: l -> m > format ->
  <all:a; right:left;  tail:b;     fmt:fmt; tag_count: k -> m > format =
  fun l r -> match l with
    | [] -> r
    | Captured(k,f) :: q -> Captured(k,f) :: (q ^^ r)
    | Open_tag t :: q -> Open_tag t :: (q ^^ r)
    | Close_tag c :: q -> Close_tag c :: (q ^^ r)
    | Close_any_tag as t :: q -> t :: (q ^^ r)
    | Point_tag t :: q -> Point_tag t :: (q ^^ r)
    | Literal t :: q -> Literal t :: (q ^^ r)
