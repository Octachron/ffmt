
module F= Format
type 'a t =  'a Format.tag = ..


type color = Red | Blue | Green | Cyan | Magenta | White | Black | Yellow
type _ t +=
  | Fg: color t

type style = { fg: color }

let string = Formatter.string

let color_escape = function
  | Black -> "\x1b[30m"
  | Red -> "\x1b[31m"
  | Green -> "\x1b[32m"
  | Yellow -> "\x1b[33m"
  | Blue -> "\x1b[34m"
  | Magenta -> "\x1b[35m"
  | Cyan -> "\x1b[36m"
  | White -> "\x1b[97m"


let none x = x
let open_printer (type a) data (tag: a F.tag) (data_tag:a) =
  match tag, data_tag with
  | Fg, color ->
    {fg = color} :: data,
    string (color_escape color)
  | _ -> data, none

let close_printer data = match data with
  | _ :: ({fg = color} :: _ as data) -> data, string (color_escape color)
  | _ -> [], Formatter.string (color_escape White)

let ansi =
  let mine: type any. any F.tag -> bool = function
    | Fg -> true
    | _ -> false in
  Tagsem.T {
    box = (fun _ _ _ -> None);
    break = (fun _ _ _ -> None);
    data = [{fg = White}];
    mine;
    open_printer;
    close_printer;
}
