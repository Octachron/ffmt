
open Format

module Deque = CCDeque

type range = {start:int; stop:int}
type substring = { content:string; range:range }


let box_indent: Format.box -> _ = function
  | H -> 0
  | V n | B n | HV n | HoV n -> n

type core =
  {
    string: substring -> unit;
    space: int -> unit;
    indent: Geometry.Indentation.t -> unit;
    break: unit -> unit;
  }

type tag_printer = core captured

module type tag_semantic = sig
  type data
  type printer
  val init: unit -> data
  val box: data -> 'any tag -> 'any -> box option
  val open_printer: data -> 'any tag -> 'any -> data * printer
  val close_printer: data -> data * printer
end

type ('acc,'printer) tagsem =
  (module tag_semantic with type data = 'acc and type printer = 'printer captured )

type 'acc tag_semantic = {
  init: unit -> 'acc;
  box_from_tag: 'any. 'any tag -> 'any -> box;
  open_printer_from_tag: 'any. 'acc -> 'any tag -> 'any -> 'acc * core captured;
  close_printer_from_tag: 'acc -> 'acc * core captured;
}

type 'p semantic_with_data =
    S: { semantic: ('acc,'p) tagsem ; data: 'acc }
    -> 'p semantic_with_data

let init (type a b) ((module Sem) as semantic : (a,b) tagsem) =
  S { semantic ; data = Sem.init () }


type lit_token = { fmt: 'a 'b 'c 'd. ('a,'b,'c * 'c,'d) token } [@@unboxed]
type break = { indent: int; space: int }


type eagerness = Strict | Lazy_up_to_break | Lazy
type open_box =
  { kind:box;
    mutable indent:int;
    mutable right:int;
    mutable eager:eagerness;
    deferred: lit_token Queue.t;
  }

type open_tag = Open_tag: {tag:'a tag; with_box:bool} -> open_tag
type t = {
  core: core;
  geometry: Geometry.t;
  tag_semantic: t semantic_with_data;

  mutable open_tags: open_tag list;
  mutable backtrack: Geometry.position;
  mutable position: Geometry.position;
  open_boxes: open_box CCDeque.t;
  after_boxes: open_box list ref;
}

let full s = { start = 0; stop=String.length s }
let all s = { content=s; range=full s }

let core_buffer b =
  let string {content;range} =
    Buffer.add_substring b content range.start (range.stop - range.start) in
  let space n = for _ = 1 to n do
      Buffer.add_char b ' '
    done in
  let indent x = space x.Geometry.Indentation.column in
  { string; space; indent; break = (fun () -> Buffer.add_char b ' '); }

  let pp_box ch (x:box) = Printf.fprintf ch (match x with
    | H -> "H"
    | V _ ->  "V"
    | HoV _ -> "HoV"
    | B _ -> "B"
    | HV _ -> "HV"
    )


module Implementation = struct
  let len = String.length
  let pp_pos ch ppf =
    Printf.fprintf ch "position: %d" ppf.position.column


  let newline base indent_t ppf =
    Printf.eprintf "indent:%d⇒%d\n%!" base indent_t;
    let {Geometry.line; column; _ } = ppf.position in
    let indent =
      { Geometry.Indentation.line = 0; column = indent_t } in
    let pos = {Geometry.line = line + 1; column = indent_t; indent } in
    ppf.position <- pos;
    ppf.backtrack <- { pos with column = base;  indent };
    ppf.core.break ();
    ppf.core.indent indent

  let space n = fun ppf ->
    ppf.position <- { ppf.position with column = ppf.position.column + n };
    ppf.core.space n

  let eager_string s ppf =
    ppf.position <- { ppf.position with column = ppf.position.column + len s };
    ppf.core.string (all s)

  let deferred ppf = match (Deque.peek_back ppf.open_boxes).eager with
    | exception Deque.Empty -> false
    | Strict -> false
    | Lazy | Lazy_up_to_break -> true


  let len_lit {fmt} = match fmt with
    | Literal s -> len s
    | Break br -> br.space
    | _ -> assert false

  let go_right ppf =
    let ppf = { ppf with position = ppf.backtrack } in
    Printf.eprintf "backtracking to %d\n%!" ppf.position.column;
    match Deque.take_front ppf.open_boxes with
    | exception Deque.Empty -> ()
    | main ->
      Deque.iter (fun box ->
          let pos = ppf.position in
          Printf.eprintf "Repositioning %a box, indent: %d \n" pp_box box.kind
            box.indent;
          Printf.eprintf "Setting indentation to %d\n%!" pos.column;
          box.indent <- pos.column;
(*          let right = Queue.fold (fun r x -> r + len_lit x) 0 box.deferred in
            box.right <- right;*)
          Printf.eprintf "Setting position to %d\n%!" (pos.column + box.right);
          ppf.position <- { pos with column = pos.column + box.right }
        ) ppf.open_boxes;
      Deque.push_front ppf.open_boxes  main;
    Printf.eprintf "Right position %d\n%!" ppf.position.column


  let eager_reeval box =
     if box.eager <> Strict then
        begin match box.kind with
        | HoV _ | B _ -> box.eager <- Strict
        | HV _ -> box.eager <- Lazy
        | H | V _ ->  box.eager <- Strict
        end

  let update_boxes_when_empty ppf =

    match Deque.length ppf.open_boxes with
    | 0 -> ()
    | 1 ->
      let box = Deque.peek_front ppf.open_boxes in
      eager_reeval box
    | _ ->
      let box = Deque.take_front ppf.open_boxes in
      ppf.after_boxes := { box with right = 0 } :: !(ppf.after_boxes);
      Printf.eprintf "Box transfer: open: %d ⇒ after: %d \n"
        (Deque.length ppf.open_boxes) (List.length !(ppf.after_boxes))


  let rec dequeue ppf = match Deque.peek_front ppf.open_boxes with
    | exception Deque.Empty -> ()
    | b ->
      match (Queue.peek b.deferred).fmt with
      | exception Queue.Empty ->
        Printf.eprintf "empty queue\n%!";
        update_boxes_when_empty ppf
      | Literal s ->
        Printf.eprintf "dequeued «%s», starting at %d\n%!" s ppf.position.column;
        ignore (Queue.pop b.deferred); eager_string s ppf;
        ppf.backtrack <- ppf.position;
        Printf.eprintf "dequeue: backtrack set at: %d\n%!" ppf.position.column;
        dequeue ppf
      | Break br ->
        begin
        Printf.eprintf "dequeued stopped at a break\n%!";
          match b.kind with
          | V n ->
            ignore (Queue.pop b.deferred);
            newline b.indent (n + br.indent + b.indent) ppf;
            dequeue ppf
          | H -> ignore (Queue.pop b.deferred); space br.space ppf;
            dequeue ppf
          | HoV _ | HV _ | B _ -> ()
        end
      | _ -> assert false


  let dequeue ppf =
    ppf.position <- ppf.backtrack;
    dequeue ppf;
    go_right ppf

  let transform_box_after_break = function
    | { kind = HV n; _ } as b -> { b with kind = V n }
    | b -> b

  let rec propagate_break ppf =
    Printf.eprintf "Break propagation\n%!";
    match Deque.peek_front ppf.open_boxes  with
    | exception Deque.Empty -> ()
    | b ->
      match (Queue.peek b.deferred).fmt with
      | exception Queue.Empty ->
        update_boxes_when_empty ppf; propagate_break ppf
      | Break { indent; _ } ->
        let b = transform_box_after_break b in
        ppf.position <- ppf.backtrack;
        newline b.indent (indent + b.indent + box_indent b.kind) ppf;
        ignore @@ Queue.pop b.deferred;
        dequeue ppf
      | Literal s -> Printf.eprintf "still at «%s»\n%!" s; assert false
      | _ -> assert false

  let to_literal f ppf =
     let b = Buffer.create 17 in
     let core = core_buffer b in
     let backtrack = ppf.backtrack in
     Printf.eprintf "literal printing: bactrack at %d\n%!" backtrack.column;
     f { ppf with core };
     ppf.backtrack <- backtrack;
     let s = Buffer.contents b in
     len s, { fmt = Literal s }


  let at_top ppf = Deque.is_empty ppf.open_boxes

  let string s ppf =
    if deferred ppf then begin
      Printf.eprintf "Defer: «%s»\n%!" s;
      let l, fmt = to_literal (eager_string s) ppf in
      let last = Deque.peek_back ppf.open_boxes in
      last.right <- last.right + l;
      Queue.add fmt last.deferred;
      (if len s + ppf.position.column >= ppf.geometry.margin then
         (
           Printf.eprintf "Going beyond margin at %d>%d\n%!"
             (len s + ppf.position.column) ppf.geometry.margin;
           propagate_break ppf)
      )
    end else (
      Printf.eprintf "Eagerly print «%s»\n%!" s;
      eager_string s ppf;
      (*      if at_top ppf then *)
        (
          ppf.backtrack <- ppf.position;
          Printf.eprintf "Bactrack set to %d\n%!" ppf.position.column;
        )
    )

  let as_space ppf {fmt} =
    match fmt with
    | Break br -> space br.space ppf
    | Literal s -> eager_string s ppf
    | _ -> assert false

  let as_newline plus ppf {fmt} =
    match fmt with
    | Break br -> newline plus (br.indent + plus) ppf
    | Literal s -> eager_string s ppf
    | _ -> assert false

  let reopen ppf =
    match !(ppf.after_boxes) with
    | [] -> ()
    | a :: q ->
      if Deque.is_empty ppf.open_boxes then (
        Deque.push_front ppf.open_boxes a;
        ppf.after_boxes:= q;
        a.right <- ppf.position.column;
        Printf.eprintf "reopening %a box, indent:%d\n%!" pp_box a.kind a.indent;
        ppf.backtrack <- {
            Geometry.line = ppf.position.line;
            column = ppf.position.column; indent = { line = 0; column = a.indent}
          }
      )

  let close_box ppf =
    Printf.eprintf "Open boxes count: %d + %d ⇒ %d\n%!"
      (Deque.length ppf.open_boxes)
      (List.length !(ppf.after_boxes))
      (-1 + Deque.length ppf.open_boxes + List.length !(ppf.after_boxes));
    Printf.eprintf "close_box, %a\n%!" pp_pos ppf;
    reopen ppf;
    Printf.eprintf "close_box, after reopen, %a\n%!" pp_pos ppf;
    match Deque.take_back ppf.open_boxes with
    | exception Deque.Empty -> assert false
    | box ->
      reopen ppf;
      Printf.eprintf "close_box, after second reopen, %a\n%!" pp_pos ppf;
      match Deque.peek_back ppf.open_boxes with
      | exception Deque.Empty ->
        Printf.eprintf "Print last box\n%!";
          Queue.iter (as_space ppf) box.deferred
      | b ->
        let subprinter = match b.kind with
          | V plus -> as_newline (plus + b.indent) ppf
          | _ -> as_space ppf in
        let _, fmt =
          to_literal
            (fun ppf -> Queue.iter subprinter box.deferred)
            { ppf with position = ppf.backtrack }
        in
        Queue.add fmt b.deferred; dequeue ppf;
        Printf.eprintf "close_box, end with %a\n%!" pp_pos ppf


  let eagerness ppf  =
    match (Deque.peek_back ppf.open_boxes).eager with
    | exception Deque.Empty -> Strict
    | e -> e

  let open_eagerness eagerness (kind:box) =
    match eagerness, (kind: box) with
    | Strict, _ -> Strict
    | Lazy_up_to_break, V _ -> Lazy_up_to_break
    | (Lazy | Lazy_up_to_break ), _ -> Lazy

  let update_eagerness_on_break ppf =
    match Deque.peek_back ppf.open_boxes with
    | exception Deque.Empty -> ()
    | box ->
      if box.eager = Strict then
        match box.kind with
        | H | V _ -> ()
        | HoV _ | B _ -> box.eager <- Lazy_up_to_break
        | HV _ -> box.eager <- Lazy

  let open_box kind ppf =
    Printf.eprintf "Open boxes count: %d ⇒ %d\n%!"
      (Deque.length ppf.open_boxes)
      (1 + Deque.length ppf.open_boxes);
    let eager = open_eagerness (eagerness ppf) kind in
    Printf.eprintf "Open box %a position: %d\n%!" pp_box kind ppf.position.column;
    let box = { kind; deferred = Queue.create (); right=0; eager;
                indent = ppf.position.column  } in
    Deque.push_back ppf.open_boxes box;
    ppf.backtrack <- ppf.position

  let keep_backtrack f ppf =
    let r = ppf.backtrack in
    f ppf;
    ppf.backtrack <- r


  let break ~space:s ~indent:i ppf =
    Printf.eprintf "At break start, %a\n" pp_pos ppf;
    let need_newline = ppf.position.column + s > ppf.geometry.margin in
    update_eagerness_on_break ppf;
    if deferred ppf then begin
      Printf.eprintf "Defer break <%d %d>\n" s i;
      Printf.eprintf "Before break, %a\n" pp_pos ppf;
      let box = Deque.peek_back ppf.open_boxes in
      Queue.add { fmt = Break {space=s;indent=i} } box.deferred;
      box.right <- box.right + s;
      Printf.eprintf "Box.right: %d\n%!" box.right;
      ppf.position <- { ppf.position with column = box.right};
      (if need_newline then
         propagate_break ppf
       else if box.eager = Lazy_up_to_break then
         ( Printf.eprintf "Dequeue up to new break\n%!";
           match Queue.pop box.deferred with
           | exception Queue.Empty -> ()
           | br -> as_space ppf br; dequeue ppf) );
      Printf.eprintf "After break, %a\n" pp_pos ppf;
    end
    else
      match Deque.peek_back ppf.open_boxes with
      | exception Deque.Empty -> space s ppf
      | {kind=H; _} -> space s ppf
      | {kind=V n; indent = more; _ } -> newline more (n+i+more) ppf
      | _ -> assert false

end


module Null_semantic = struct
  type data = unit
  type printer = t captured
  let init () = ()
  let box _data _tag _tag_data= None
  let open_printer _ _ _ = (), (fun _ -> ())
  let close_printer _ =  (), (fun _ -> ())
end


module Box_semantic = struct
  type data = unit
  type printer = t captured
  let init () = ()
  let box (type a) () (tag: a tag)  (i:a): box option = Some(match tag with
    | B -> B i
    | H -> H
    | V -> V i
    | HV -> HV i
    | HoV -> HoV i
    | _ -> raise Unknown_tag)

  let open_printer _ _ _ = (), (fun _ -> ())
  let close_printer _ =  (), (fun _ -> ())
end


type formatter = t
type printer = t captured


let core_chan ch =
  let string {content;range} =
    output_substring ch content range.start (range.stop-range.start) in
  let space n = for _ = 1 to n do
      output_string ch " "
    done in
  let indent x = space x.Geometry.Indentation.column in
  { string; space; indent; break = (fun () -> output_string ch "\n"); }

let null_semantic =
  (module Null_semantic: tag_semantic with type data = unit
                                       and type printer = printer  )

let box_semantic =
  (module Box_semantic: tag_semantic with type data = unit
                                       and type printer = printer  )



let with_sem f = fun ?(geometry=Geometry.default) ?tag_semantic x ->
  let tag_semantic =
  match tag_semantic with
  | None -> init box_semantic
  | Some x -> init x in
  { core = f x; open_tags = []; open_boxes = Deque.create (); tag_semantic; geometry;
    position = Geometry.start; backtrack = Geometry.start;
    after_boxes = ref ([]:_ list)
  }

let chan ?geometry = with_sem core_chan ?geometry
let buffer ?geometry = with_sem core_buffer ?geometry

let stdout = chan Pervasives.stdout
let stderr = chan Pervasives.stderr


type tag_name= Name: 'any tag -> tag_name


let string = Implementation.string
let int d: printer = string (string_of_int d)
let float f: printer = string (string_of_float f)

exception No_open_tag
type exn += Mismatched_close: {expected:'any tag; got:'other tag} -> exn


let rec eval:
  type free right.
  formatter -> (free,unit,right,formatter) format
  -> (free,right,unit) iargs
  -> unit  =
  fun ppf fmt iargs -> match fmt with
    | [] -> ()
    | Literal s :: q  ->
      string s ppf;
      eval ppf q iargs
    | Captured f:: q -> let g, iargs = f iargs in g ppf; eval ppf q iargs
    | Open_tag {tag;data} :: q ->
      let S s = ppf.tag_semantic in
      let module Sem = (val s.semantic) in
      let with_box, open_box =
        match Sem.box s.data tag data with
        | None -> false, ignore
        | Some b -> true, Implementation.open_box b in
      let open_tags: _ list = Open_tag { tag; with_box } :: ppf.open_tags in
      let sdata, p = Sem.open_printer s.data tag data in
      open_box ppf;
      p ppf;
      eval { ppf with open_tags;
                      tag_semantic = S { s with data=sdata } } q iargs
    | Close_any_tag :: q ->
      begin match ppf.open_tags with
        | [] -> raise No_open_tag
        | Open_tag r :: tags -> close_tag r.with_box tags ppf q iargs
      end
    | Close_tag ctag :: q ->
      begin match ppf.open_tags with
        | [] -> raise No_open_tag
        | Open_tag {tag; _ } :: _ when Name tag <> Name ctag ->
          raise (Mismatched_close {expected=tag;got=ctag})
        | Open_tag {tag; with_box} :: tags ->
          close_tag with_box tags ppf q iargs
      end
    | Break {space; indent} :: q ->
      Implementation.break ~space ~indent ppf;
      eval ppf q iargs



and close_tag: type any free right.
  bool -> open_tag list
  -> formatter -> (free,unit,right,formatter) format -> (free,right,unit) iargs
  ->  unit = fun with_box open_tags ppf q iargs ->
  let S s = ppf.tag_semantic in
  let module Sem = (val s.semantic) in
  let acc, p =  Sem.close_printer s.data in
  Printf.eprintf "Eval start %a\n%!" Implementation.pp_pos ppf;
  p ppf;
  if with_box then Implementation.close_box ppf;
  Printf.eprintf "Eval %a\n%!" Implementation.pp_pos ppf;
  let ppf = { ppf with open_tags;
                       tag_semantic = S { s with data= acc}
            } in
  eval ppf q iargs


let eval ppf fmt args = eval ppf fmt (Format.make args)
