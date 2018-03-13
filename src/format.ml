(** Core definition of format string *)

type 'fmt captured = 'fmt -> 'fmt
type ('a,'fmt) printer = 'a -> 'fmt captured

type (_,_) index =
  | Z: ('elt, 'elt -> _ ) index
  | S: ('elt, 'list) index -> ('elt, 'any -> 'list) index



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

type _ tag = ..

exception Unknown_tag
type box = H  | V of int | HoV of int | HV of int | B of int
type break_data = { space: int; indent: int }
type break = Break of break_data | Full_break of int

type _ tag +=
  | B: int tag
  | H: unit tag
  | V: int tag
  | HoV: int tag
  | HV: int tag
  | Break: (int * int) tag
  | Full_break: int tag


type 'all default = 'all
  constraint 'all = < list: 'a; pos:'b -> 'c; fmt:'d >

type 'all simple = 'all
  constraint 'all = < pos:'b -> 'b; .. > default

type _ token =
  | Literal: string -> _ simple token
  | Captured:
      ('right,'right2) Size.t *
      ( ('list,'right) iargs -> 'fmt -> 'fmt )
      -> <pos: 'right -> 'right2; list:'list; fmt:'fmt > default token
  | Open_tag: ('data tag * 'data) -> _ simple token
  | Close_tag: _ tag -> _ simple token
  | Close_any_tag: _ simple token
  | Point_tag: ('data tag * 'data) -> _ simple token


type _ format =
  | []: <all:'any; right:'right; tail:'right; fmt:'fmt> format
  | (::):
      <list:'list; pos:'right -> 'right2; fmt:'fmt> token
      * <all:'list; right:'right2; tail: 'tail; fmt: 'fmt> format ->
      <all:'list; right:'right; tail:'tail; fmt:'fmt> format




let rec nth: type elt a. (elt,a) index -> a args -> elt =
  fun n  args ->
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

let rec (^^): type left right b a fmt.
  <all:a; right:left;  tail:right; fmt:fmt> format ->
  <all:a; right:right; tail:b;     fmt:fmt> format ->
  <all:a; right:left;  tail:b;     fmt:fmt> format =
  fun l r -> match l with
    | [] -> r
    | Captured(k,f) :: q -> Captured(k,f) :: (q ^^ r)
    | Close_tag _ as t :: q -> t :: (q ^^ r)
    | Close_any_tag as t :: q -> t :: (q ^^ r)
    | Point_tag _ as t :: q -> t :: (q ^^ r)
    | Open_tag _ as t :: q -> t :: (q ^^ r)
    | Literal _ as t :: q -> t :: (q ^^ r)
