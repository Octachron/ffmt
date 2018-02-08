
type 'fmt captured = 'fmt -> unit
type ('a,'fmt) printer = 'a -> 'fmt captured

type (_,_) index =
  | Z: ('elt, 'elt -> _ ) index
  | S: ('elt, 'list) index -> ('elt, 'any -> 'list) index

type (_,_) args =
  | []: ('result,'result) args
  | (::): 'a * ('list,'result) args -> ('a -> 'list, 'result) args

type ('all,'right,'result) iargs =
  { all: ('all,'result) args; right: ('right,'result) args }

let make x = { all = x; right = x }
let current (type all elt right stop) (x: (all,elt -> right,stop) iargs) =
  match x.right with
  | a :: q -> a, { x with right = q}
  | [] -> raise Not_found

type _ tag = ..

exception Unknown_tag
type box = H  | V of int | HoV of int | HV of int | B of int

type _ tag +=
  | B: int tag
  | H: unit tag
  | V: int tag
  | HoV: int tag
  | HV: int tag


type (_,_,_,_) token =
  | Literal: string -> ('list, 'close,'pos * 'pos,'fmt) token
  | Captured:
      ( ('list,'right,'close) iargs ->
        'fmt captured * ('list,'right2,'close) iargs )
      -> ('list,'close, 'right * 'right2 ,'fmt) token
  | Open_tag: {tag: 'data tag; data:'data} ->
    ('list, 'close,'pos * 'pos,'fmt) token
  | Close_tag: _ tag -> ('list, 'close,'pos * 'pos,'fmt) token
  | Close_any_tag: ('list, 'close,'pos * 'pos,'fmt) token
  | Break: { indent: int; space:int } -> ('list, 'close,'pos * 'pos,'fmt) token

type (_,_,_,_) format =
  | []: ('any,'result, 'right,'fmt) format
  | (::):
      ('list,'result, 'right * 'right2,'fmt) token
      * ('list,'result,'right2,'fmt) format ->
      ('list,'result,'right,'fmt) format




let rec nth: type elt a right r. (elt,a) index -> (a,r) args -> elt = fun n  args ->
  match n, args with
  | Z , a :: _ -> a
  | S n, _ :: q -> nth n q
  | _, [] -> raise Not_found

let (.%()) iargs n = nth n iargs.all

(*
let rec (^^): type r right free fmt.
  (free,r,right,fmt) format -> (free,r,right,fmt) format ->
  (free,r,right,fmt) format =
  fun l r -> match l with
    | [] -> r
    | a :: q -> a :: (q ^^ r)
 *)
