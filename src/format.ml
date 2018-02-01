
type 'fmt captured = 'fmt -> unit
type ('a,'fmt) printer = 'a -> 'fmt captured

type (_,_) index =
  | Z: ('elt, 'elt -> _ ) index
  | S: ('elt, 'list) index -> ('elt, 'any -> 'list) index

type (_,_,_,_) token =
  | Literal: string -> ('list, 'close,'pos * 'pos,'fmt) token
  | Captured: 'fmt captured -> ('list,'close,'pos * 'pos,'fmt) token
  | Var: { typ:('typ,'fmt) printer;
           pos: ('typ, 'list) index
         } -> ('list,'close,'pos * 'pos,'fmt ) token
  | Implicit_pos: ('typ,'fmt) printer ->
    ('list,'close, ('typ -> 'more) * 'more,'fmt) token
  | Ext_var: {
      data_index: ('typ,'list) index;
      printer_index:( ('typ,'fmt) printer,'list) index } ->
    ('list,'close,'pos * 'pos, 'fmt) token
  | Implicit:
    ('list,'close, ( ('typ,'fmt) printer -> 'typ -> 'more) * 'more,'fmt) token



type (_,_,_,_) format =
  | []: ('any,'result, 'right,'fmt) format
  | (::):
      ('list,'result, 'right * 'right2,'fmt) token
      * ('list,'result,'right2,'fmt) format ->
      ('list,'result,'right,'fmt) format


type (_,_) args =
  | []: ('result,'result) args
  | (::): 'a * ('list,'result) args -> ('a -> 'list, 'result) args

let rec nth: type elt a r. (elt,a) index -> (a,r) args -> elt = fun n  args ->
  match n, args with
  | Z , a :: _ -> a
  | S n, _ :: q -> nth n q
  | _, [] -> raise Not_found
