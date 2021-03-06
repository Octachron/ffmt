exception Unknown_tag
type box =
  | H
  | V of int
  | HoV of int
  | HV of int
  | B of int
  | Hide
  | If
  | Then
  | Else
  | Translucid of int
  | Columns of int list

type break_data = { space: int; indent: int }
type break = Break of break_data | Full_break of int

type 'a endo = 'a -> 'a

type _ tag = ..

type _ tag +=
  | B: int tag
  | H: unit tag
  | V: int tag
  | HoV: int tag
  | HV: int tag
  | If: unit tag
  | Then: unit tag
  | Else: unit tag
  | Break: (int * int) tag
  | Full_break: int tag
  | Columns: int array tag
  | Column_switch: unit tag
