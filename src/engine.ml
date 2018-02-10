
open Format

module Deque = CCDeque


let box_indent: Format.box -> _ = function
  | H -> 0
  | V n | B n | HV n | HoV n -> n

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
  spec : t Spec.t;

  mutable open_tags: open_tag list;
  mutable backtrack: Geometry.position;
  mutable position: Geometry.position;
  open_boxes: open_box CCDeque.t;
  after_boxes: open_box list ref;
}


  let pp_box ch (x:box) = Printf.fprintf ch (match x with
    | H -> "H"
    | V _ ->  "V"
    | HoV _ -> "HoV"
    | B _ -> "B"
    | HV _ -> "HV"
    )

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
    ppf.spec.phy.break ();
    ppf.spec.phy.indent indent

  let space n = fun ppf ->
    ppf.position <- { ppf.position with column = ppf.position.column + n };
    ppf.spec.phy.space n

  let eager_string s ppf =
    ppf.position <- { ppf.position with column = ppf.position.column + len s };
    ppf.spec.phy.string (Spec.all s)

  let deferred ppf = match (Deque.peek_back ppf.open_boxes).eager with
    | exception Deque.Empty -> false
    | Strict -> false
    | Lazy | Lazy_up_to_break -> true


  let len_lit {fmt} = match fmt with
    | Literal s -> len s
    | Break br -> br.space
    | _ -> assert false

  let go_right ppf =
    Printf.eprintf "Go right\n%!";
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
          let right = Queue.fold (fun r x -> r + len_lit x) 0 box.deferred in
            box.right <- right;
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
     let phy = Spec.buffer b in
     let backtrack = ppf.backtrack in
     Printf.eprintf "literal printing: bactrack at %d\n%!" backtrack.column;
     f { ppf with spec = { ppf.spec with phy} };
     ppf.backtrack <- backtrack;
     let s = Buffer.contents b in
     Printf.eprintf "Printed literal: «%s»\n%!" s;
     len s, { fmt = Literal s }


  let at_top ppf = Deque.is_empty ppf.open_boxes

  let string s ppf =
    if deferred ppf then begin
      Printf.eprintf "Defer: «%s»\n%!" s;
      let l, fmt = to_literal (eager_string s) ppf in
      let last = Deque.peek_back ppf.open_boxes in
      last.right <- last.right + l;
      Queue.add fmt last.deferred;
      (if len s + ppf.position.column >= ppf.spec.geometry.margin then
         (
           Printf.eprintf "Going beyond margin at %d>%d\n%!"
             (len s + ppf.position.column) ppf.spec.geometry.margin;
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
    | Literal s -> Printf.eprintf "eager printing (nl) «%s»\n%!" s;
      eager_string s ppf
    | _ -> assert false

  let reopen ppf =
    match !(ppf.after_boxes) with
    | [] -> ()
    | a :: q ->
      if Deque.is_empty ppf.open_boxes then (
        Deque.push_front ppf.open_boxes a;
        ppf.after_boxes:= q;
        a.right <- ppf.position.column;
        Printf.eprintf "reopening %a box, indent:%d, %a\n%!" pp_box a.kind a.indent
          pp_pos ppf
        ;
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
        Printf.eprintf "Print last box!\n%!";
          Queue.iter (as_space ppf) box.deferred
      | b ->
        let subprinter ppf = match box.kind with
          | V plus -> Printf.eprintf "V last words\n%!";
            as_newline (plus + box.indent) ppf
          | _ -> as_space ppf in
        let _, fmt =
          Printf.eprintf "Literal subprinting\n%!";
          ppf.position <- ppf.backtrack;
          Printf.eprintf "Backtrack to %a\n" pp_pos ppf;
          to_literal
            (fun ppf -> Queue.iter (subprinter ppf) box.deferred)
            ppf
        in
        Queue.add fmt b.deferred;
        b.right <- b.right + len_lit fmt;
        dequeue ppf;
        Printf.eprintf "close_box, end with %a\n%!" pp_pos ppf


  let eagerness ppf  =
    match (Deque.peek_back ppf.open_boxes).eager with
    | exception Deque.Empty -> Strict
    | e -> e

  let open_eagerness eagerness (kind:box) =
    match eagerness, (kind: box) with
    | Strict, _ -> Strict
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
    let need_newline = ppf.position.column + s > ppf.spec.geometry.margin in
    update_eagerness_on_break ppf;
    if deferred ppf then begin
      Printf.eprintf "Defer break <%d %d>\n" s i;
      Printf.eprintf "Before break, %a\n" pp_pos ppf;
      let box = Deque.peek_back ppf.open_boxes in
      Queue.add { fmt = Break {space=s;indent=i} } box.deferred;
      box.right <- ppf.position.column + s;
      Printf.eprintf "Box.right: %d\n%!" box.right;
      ppf.position <- { ppf.position with column = box.right};
      (if need_newline then
         propagate_break ppf
       else if box.eager = Lazy_up_to_break then
         ( Printf.eprintf "Dequeue up to new break\n%!";
           if Queue.length box.deferred > 1 then (
             let f = Queue.pop box.deferred in
             as_space ppf f; dequeue ppf
           )));
       Printf.eprintf "After break, %a\n" pp_pos ppf;
    end
    else
      match Deque.peek_back ppf.open_boxes with
      | exception Deque.Empty -> space s ppf
      | {kind=H; _} -> space s ppf
      | {kind=V n; indent = more; _ } -> newline more (n+i+more) ppf
      | _ -> assert false

let string s ppf = string s ppf ; ppf
let open_box b ppf = open_box b ppf; ppf
let break ~space ~indent ppf = break ~space ~indent ppf; ppf
let close_box ppf = close_box ppf; ppf
