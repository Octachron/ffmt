type alt = Plus | Space | Hash
type ext = Lit of int | Star

type t = {
    modifier:alt option;
    padding:ext option;
    precision:ext option;
    variant:string option;
  }
