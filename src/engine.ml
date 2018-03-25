(** Formatting engine implementation *)
open Defs

let default_box: box = H
type open_box_on_the_left = {indent:int;kind:box}

type resolved_lit =
  | Str of string
  | Space of int
  | Newline of int
  | Box of resolved_lit list
type suspended_lit =
    Break of break_data | Lit of resolved_lit

module Q = Bigraded_fqueue


module Move = struct
  type t =
    | Relative of int
    | Absolute of {nl:int; indent:int}

  let ( +> ) t x = match t with
    | Relative n -> Relative (n + x)
    | Absolute {nl;indent} -> Absolute {nl; indent = indent + x}

  let ( + ) x y = match x,y with
    | Absolute {nl; indent}, Relative n -> Absolute {nl; indent = indent + n}
    | Relative k, Relative n -> Relative (k + n)
    | Relative _ , (Absolute _ as a) -> a
    | Absolute {nl;_} , Absolute a -> Absolute { a with nl = nl + a.nl}

  let absolute n = Absolute { nl=1; indent= n }
(*
  let pp ppf = function
    | Relative n -> Printf.fprintf ppf "+%d" n
    | Absolute n -> Printf.fprintf ppf "{\\n×%d; +%d}" n.nl n.indent
*)
  let decrease k = function
    | Relative n -> Relative (n - k)
    | Absolute _ as a -> a

  let decrease_all k = function
    | Relative n -> Relative (n - k)
    | Absolute a -> Absolute { a with indent = a.indent - k }

  let commit_line l = function
    | Absolute { nl; indent } when nl = l -> Relative indent
    | Absolute n -> Absolute { n with nl = n.nl - l }
    | Relative _ as r -> r

  let pos = function Relative n | Absolute {indent = n; _ } -> n
  let pure n = Relative n
end

type 'a position =
  {
    phy: 'a Raw.t; (** the underlying driver *)
    current:int; (** we are currently writing at position *)
    kind:box; (** with this box context *)
    indent:int; (** and at this indentation *)
    last_indent:int (** the previous line break was done with this indent *)
  }

type suspended = {
  break:break_data option;
  (** We are in the suspended mode because we don't know how to interpret
      this break yet *)
  after: (suspended_lit, box * int) Bigraded_fqueue.t;
  (** we already have computed these token after the block *)
  right: Move.t (** and we are currently at this position < margin *);
}


type status =
  | Direct
  (** We are directly writing to the logical device *)

  | Suspended of suspended
  (**We are blocked at an ambiguous break, waiting for the decision on
     how to interpret it *)

  | Hide of { boxes: int }
  (** We are discarding data waiting for the end of the current hidden box *)

type 'a t = {
  context: open_box_on_the_left list;
  position: 'a position;
  status:status;
}

let make context status position =
  { context;status; position }
let update ppf position = make ppf.context ppf.status position

module G = Geometry
module I = Geometry.Indentation

let box_indent: box -> int = function
  | B n | HV n | HoV n | V n-> n
  | H | HH | Hide -> 0

let newline_indent ~max_indent ~more pos =
  min (pos.indent + more + box_indent pos.kind) max_indent

let phyline max_indent more pos =
  let column = newline_indent ~max_indent ~more pos in
  { pos with
    phy = pos.phy#break#indent {I.line = 0; column};
    current = column;
    last_indent = column
  }

let reindent n: box -> box = function
  | B _ -> B n | HV _ -> HV n | HoV _ -> HoV n | V _ -> V n
  | H -> H | Hide -> Hide | HH -> HH

let phyreset kind (phy:_ Raw.t) =
  let indent = 0 in
  let kind = reindent 0 kind in
  { phy = phy#break#indent {I.line = 1; column=indent};
    current = indent; indent; last_indent = indent; kind
  }

let newline geom more pos =
  phyline geom.G.max_indent more pos

let physpace  n pos =
  { pos with phy = pos.phy#space n;
    current = n + pos.current }
let phystring s pos =
  let r = Raw.all s in
  { pos with phy = pos.phy#string r;
             current = pos.current + pos.phy#len r
  }

let direct_string context c s  =
  make context Direct (phystring s c)
let is_vertical (x: box) = match x with V _ -> true | _ -> false
let max_indent geom = geom.G.max_indent

let commit_resolved_literal max_indent lits c =
  let rec elt (n,c) =  function
    | Str s -> n, phystring s c
    | Newline more -> n + 1, phyline max_indent more c
    | Space sp -> n, physpace sp c
    | Box b ->
      let n, inside =
        List.fold_left elt (n,{ c with indent = c.current; kind = B 0 }) b in
      n, { c with current = inside.current; phy = inside.phy } in
  List.fold_left elt c lits

let rec advance_to_next_ambiguity geom context stream right c =
  match Q.take_front stream with
  | Major(((HH:box), _), after) ->
    (* horizontal or hidden boxes immediately suspend the output until
       their resolution *)
    let status = Suspended { break = None; after; right } in
    make context status c
  | Major ((b , _), rest) ->
    (* When we meet a box without any ambiguity on its position,
       we put the current box on the left stack, and focus on this
       new box *)
    let indent = c.current in
    let context: open_box_on_the_left list =
      { kind = c.kind; indent = c.indent } :: context in
    advance_to_next_ambiguity geom context rest right
      { c with indent; kind = b }
  | Minor(Break b, after) ->
    begin match c.kind with
      | V _ -> (* this happens with suspended V box nested inside other boxes *)
        let right = Move.commit_line 1 right in
        c |> phyline (max_indent geom) b.indent
        |> advance_to_next_ambiguity geom context after right
      | _ ->
        let status = Suspended { break = Some b; after; right } in
        make context status c
    end
  | Minor(Lit s, rest) ->
    let newlines, c = commit_resolved_literal (max_indent geom) [s] (0,c) in
    let right = Move.commit_line newlines right in
    advance_to_next_ambiguity geom context rest right c
  | Empty -> make context Direct c

let rec hide context stream count c =
  match Q.take_front stream with
  | Major (((Hide:box), _), after) -> hide context after (count +1) c
  | Major(_, after) | Minor(_,after) -> hide context after (count + 1 ) c
  | Empty -> make context (Hide {boxes=1}) c

let rec advance_to_next_box max_indent stream right c =
  match Q.take_front stream with
  | Major _ -> c, right, stream
  | Minor(Break b, stream) ->
    let right = Move.decrease_all b.space right in
    c |> phyline max_indent b.indent
    |> advance_to_next_box max_indent stream right
  | Minor(Lit s, stream) ->
    let n, c' = (0,c) |> commit_resolved_literal max_indent [s] in
    let right = if n = 0 then Move.(right +> (c.current - c'.current) )
      else right in
    advance_to_next_box max_indent stream (Move.commit_line n right) c'
  | Empty -> c, right, Q.empty

let sbreak sd = match sd.break with
  | None -> { indent = 0; space = 0 }
  | Some b -> b

let actualize_break geom context sd c =
  let before = c.current in
  let br = sbreak sd in
  let c = newline geom br.indent c in
  match c.kind with
  | HH -> hide context sd.after 1 c
  | HV n ->
    let indent =
      newline_indent ~max_indent:(max_indent geom) ~more:br.indent c in
    let diff = indent - before - br.space in
    let c, right, stream =
      advance_to_next_box (max_indent geom) sd.after Move.(sd.right +> diff)
        { c with kind = V n } in
    advance_to_next_ambiguity geom context stream right c
  | _ ->
    let right =
      Move.decrease (before + br.space - c.current) sd.right in
    advance_to_next_ambiguity geom context sd.after right c

let rec string geom s ppf =
  match ppf.status with
  | Direct ->
    direct_string ppf.context ppf.position s
  | Suspended sd ->
    let right = Move.( sd.right +> ppf.position.phy#len (Raw.all s) ) in
    if Move.pos right > geom.G.margin then
         ppf.position
      |> actualize_break geom ppf.context sd
      |> string geom s
    else
      let after = Q.push_min (Lit (Str s)) sd.after in
      { ppf with status = Suspended { sd with after; right } }
  | Hide _ -> ppf


let reactivate position (context: open_box_on_the_left list) =
   match context with
     | [] ->
       { position with indent = 0; kind = default_box }, ([]: _ list)
     (* empty box *)
     | {kind;indent} :: q ->
       { position with indent; kind }, q


let translate_break (b:break_data) box =
  if is_vertical box then Newline (b.indent + box_indent box) else Space b.space

let resolve_box bx stream =
  let translate bx = function
    | Lit s -> s
    | Break b -> translate_break b bx in
  let x = stream |> Sequence.map (translate bx) |> Sequence.to_list in
  Box x

let coalesce geom context sp c =
  match Q.take_major_back sp.after with
  | Some (rest, (b,_)), last_box_content ->
    let after = match b with
      | Hide -> rest
      | _ -> let lit = resolve_box b last_box_content in
        Q.push_min (Lit lit) rest in
    make context (Suspended {sp with after}) c
  | None, last_box_content ->
    let b = c.kind in
    (*    let c, context = reactivate c context in*)
    match b with
    | Hide -> make context Direct c
    | _ ->
      let lit = resolve_box b last_box_content in
      let c = physpace (sbreak sp).space c in
      let c = (0, c)
              |> commit_resolved_literal (max_indent geom) [lit]
              |> snd in
      let c, context = reactivate c context in
      make context Direct c

let close_box geom ppf =
  match ppf.status with
  | Direct ->
    let position, context = reactivate ppf.position ppf.context in
    { ppf with position; context }
  | Suspended sp -> coalesce geom ppf.context sp ppf.position
  | Hide {boxes=0} ->
    let position, context = reactivate ppf.position ppf.context in
    { ppf with position; context }
  | Hide {boxes} -> { ppf with status = Hide {boxes=boxes - 1} }


let eager_indent: box -> _ = function
  | B _ -> true
  | _ -> false

let last_active_box pos sp =
  match Q.peek_major_back sp.after with
  | Some (b,pos) -> b, pos
  | None ->  pos.kind, pos.indent

let rec break geom br (ppf: _ t) =
  let c = ppf.position in
  match ppf.status with
  | Hide _ -> ppf
  | Direct -> begin match c.kind with
      | Hide -> ppf
      | H -> update ppf (physpace br.space c)
      | V _ -> update ppf (newline geom br.indent c)
      | HoV _ | B _ | HV _ | HH as b ->
        if c.current + br.space > geom.G.margin
        || (eager_indent b &&
            c.indent + br.indent + box_indent b < c.last_indent)
        then
          let kind: box = match c.kind with
            | HV n -> V n
            | HH -> Hide
            | HoV _ | B _ | H | V _ | Hide as b -> b in
          { c  with kind }
            |> newline geom br.indent
            |> update ppf
          else
            let status =
              Suspended { break = Some br;
                          after = Q.empty;
                          right = Move.pure (c.current + br.space);
                        } in
            { ppf with status } end
  | Suspended sd ->
    let right = match last_active_box c sd with
      | V k, m ->
        Move.( sd.right + absolute k +> br.indent +> m )
      | _ -> Move.(sd.right +> br.space ) in
    let after = Q.push_min (Break br) sd.after  in
    let b = c.kind in
    if Move.pos right > geom.G.margin
    || (eager_indent b && c.indent + br.indent + box_indent b < c.last_indent)
    then
      c
      |> actualize_break geom ppf.context
        { sd with right = Move.commit_line 1 sd.right}
      |> break geom br
    else match c.kind with
      | HoV _ | B _ when not (Q.secondary after) ->
        c
        |> physpace (sbreak sd).space
        |> advance_to_next_ambiguity geom ppf.context after right
      | _ ->
          { ppf with status = Suspended { sd with after; right } }

let rec full_break geom br (ppf:_ t) =
  let pos = ppf.position in
  match ppf.status with
  | Hide _ -> ppf
  | Direct ->
    pos |> newline geom br |> update ppf
  | Suspended sd ->
    match pos.kind with
    | HoV _ | B _ when not (Q.secondary sd.after) ->
      pos |> physpace (sbreak sd).space
      |> advance_to_next_ambiguity geom ppf.context sd.after sd.right
      |> full_break geom br
    | _ ->
      pos |> actualize_break geom ppf.context sd |> full_break geom br

let box_always_suspends = function
  | (HH:box) -> true
  | _ -> false

let rec open_box geom b (ppf: _ t) =
  let pos = ppf.position in
  match ppf.status with
  | Direct ->
    if pos.current > geom.G.box_margin then
      phyreset pos.kind pos.phy |> update ppf
      |> open_box geom b
    else
      let position = { pos with kind = b; indent = pos.current } in
      let context: open_box_on_the_left list =
        {indent = pos.indent; kind = pos.kind } :: ppf.context in
      let ppf = { ppf with context; position } in
      if box_always_suspends b then
        let status =
          Suspended {after = Q.empty; break = None;
                     right = Move.pure position.current} in
        { ppf with status }
      else ppf
  | Suspended s ->
    if Move.pos s.right > geom.G.box_margin then
      pos |> actualize_break geom ppf.context s |> open_box geom b
    else
      let status = Suspended
          { s with after = Q.push_maj (b, Move.pos s.right) s.after } in
      { ppf with status }
  | Hide r -> { ppf with status = Hide { boxes = r.boxes + 1 } }

type ('a,'b) prim = Geometry.t -> 'a -> 'b t -> 'b t
let start phy =
  { status = Direct;
    position= {current=0;indent=0; kind=default_box; last_indent=0; phy };
    context = [] }

let close_box geom () = close_box geom
let flush (ppf: _ t) = ppf.position.phy#flush
