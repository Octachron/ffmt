module F= Format

type 'a t =  'a Defs.tag = ..

type ('a,'b) printer = ('a,'b) Tagsem.printer

type base_color = Red | Blue | Green | Cyan | Magenta | White | Black | Yellow

type color = { base: base_color; bright: bool }

type frame = Framed | Encircled | None

type font = Fraktur | Italic | Normal
type boldness = Faint | Normal | Bold

type _ t +=
  | Fg: color t
  | Bg: color t
  | Font: font t
  | Bold: boldness t
  | Underlined: unit t
  | Crossed_out: unit t
  | Framed: frame t
  | Overlined: unit t


type style = { fg: color;
               bg: color;
               frame:frame;
               overlined:bool;
               font:font;
               bold:boldness;
               underlined:bool;
               crossed_out: bool;
             }

let string = Formatter.string

let color_code = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7


let null ppf = ppf

let color_escape {base;bright} =
  color_code base + if bright then 90 else 30

let int = Combinators.int

let diff (field,f) (prev,st) =
  if field prev <> field st then
    let r = f (field st) in
    int r
  else null

let (/) = diff
let seq l ppf =
  ppf |> string "\x1b[" |> Combinators.seq l |> string "m"


let fg = (fun x -> x.fg), (fun x -> color_escape x)

let bg = (fun x -> x.bg), (fun x -> 10 + color_escape x)

let frame =
  (fun x -> x.frame),
  function
  | Encircled -> 52
  | Framed -> 51
  | None -> 54

let overlined =
  (fun  x -> x.overlined),
  function
  | true -> 53
  | false -> 55

let font =
  (fun x -> x.font),
  function
  | Italic -> 3
  | Fraktur -> 20
  | Normal -> 23

let bold =
  (fun x -> x.bold),
  function
  | Faint -> 2
  | Bold -> 1
  | Normal -> 22

let underlined =
  (fun x -> x.underlined),
  function
  | true -> 4
  | false -> 24

let crossed_out =
  (fun x -> x.crossed_out),
  function true -> 9 | false -> 29


let style pair =
  seq [ fg / pair;
        bg / pair;
        frame / pair;
        font / pair;
        bold / pair;
        underlined / pair ;
        overlined / pair;
        crossed_out /pair;
      ]


let none x = x

let default = {
  fg = {base=White;bright=true};
  bg = {base=Black; bright=true};
  frame = None;
  bold = Normal;
  font = Normal;
  underlined = false;
  overlined = false;
  crossed_out = false
}


let close_printer data = match data with
  | prev :: (st :: _ as data) -> data, style (prev,st)
  | [prev] -> [], style (prev,default)
  | _ -> [], null

let sem =
  object(_:'self)
    constraint 'self = #Tagsem.semclass
    val data = [default]
    method mine: type any. any Defs.tag -> bool = function
      | Fg -> true | Bg -> true | Underlined -> true
      | Overlined -> true | Bold -> true | Font -> true
      | Framed -> true | Crossed_out -> true
      | _ -> false
    method box: type any. any Defs.tag -> any -> _ option = fun _ _ -> None
    method break:  type any. any Defs.tag -> any -> _ option = fun _ _ -> None

    method open_printer: type a f n. a Defs.tag -> a -> 'self * (f,n) printer =
      fun tag data_tag ->
        let prev = match data with
          | [] -> default
          | a :: _ -> a in
        let st =
          match tag, data_tag with
          | Fg, fg -> { prev with fg }
          | Bg, bg -> { prev with bg }
          | Font, font -> { prev with font }
          | Bold, bold -> {prev with bold }
          | Underlined, () -> {prev with underlined = true }
          | Overlined, () -> { prev with overlined = true }
          | Framed, frame -> { prev with frame }
          | Crossed_out, () -> { prev with crossed_out = true }
          | _, _ -> prev
        in
        {< data = st :: data >}, style (prev,st)

    method close_printer: type f n. 'self * (f,n) printer =
      match data with
      | prev :: (st :: _ as data) -> {< data >}, style (prev,st)
      | [prev] -> {< data = [] >}, style (prev,default)
      | _ -> {< data = [] >}, null
  end

let bold = (Bold: _ Defs.tag ), (Bold:boldness)
let u = Underlined, ()
let i = Font, Italic
let fk = Font, Fraktur
let crossed = Crossed_out, ()
