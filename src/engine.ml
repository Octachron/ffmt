(** Formatting engine implementation *)
open Defs
let debug fmt = Printf.ifprintf stderr ("debug engine: "^^fmt ^^ "\n%!")

let default_box: box = H

let pp_box (ppf:out_channel): box -> unit = function
  | V n -> Printf.fprintf ppf "V %d" n
  | HV n -> Printf.fprintf ppf "HV %d" n
  | HoV n -> Printf.fprintf ppf "HoV %d" n
  | B n -> Printf.fprintf ppf "B %d" n
  | H -> Printf.fprintf ppf "H"

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
  let pp ppf = function
    | Relative n -> Printf.fprintf ppf "+%d" n
    | Absolute n -> Printf.fprintf ppf "{\\n×%d; +%d}" n.nl n.indent

  let decrease k x =
    let r = match x with
    | Relative n -> Relative (n - k)
    | Absolute _ as a -> a in
  debug "decreasing %a by %d yields %a" pp x k pp r;
  r

  let decrease_all k x =
    let r = match x with
    | Relative n -> Relative (n - k)
    | Absolute a -> Absolute { a with indent = a.indent - k } in
  debug "decreasing %a by %d yields %a" pp x k pp r;
  r

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
  break:break_data;
  (** We are in the suspended mode because we don't know how to interpret
      this break yet *)
  after: (suspended_lit, box * Move.t) Bigraded_fqueue.t;
  (** we already have computed these token after the block *)
  right: Move.t (** and we are currently at this position < margin *);
}


type status =
  | Direct
  (** We are directly writing to the logical device *)

  | Suspended of suspended
  (**We are blocked at an ambiguous break, waiting for the decision on
     how to interpret it *)

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
  | H -> 0

let newline_indent ~max_indent ~more pos =
  min (pos.indent + more + box_indent pos.kind) max_indent
let phyline max_indent more pos =
  let indent = newline_indent ~max_indent ~more pos in
  debug "line break ⇒ %d" indent;
  { pos with
    phy = pos.phy#break#indent {I.line = 0; column=indent};
    current = indent;
    last_indent = indent
  }

let reindent n: box -> box = function
  | B _ -> B n | HV _ -> HV n | HoV _ -> HoV n | V _ -> V n
  | H -> H

let phyreset kind (phy:_ Raw.t) =
  let indent = 0 in
  let kind = reindent 0 kind in
  debug "line reset";
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
  debug "direct printing «%s», %d ⇒ %d" s pos.current
    (pos.current + pos.phy#len r);
  { pos with phy = pos.phy#string r;
             current = pos.current + pos.phy#len r
  }

let direct_string context c s  =
  make context Direct (phystring s c)

let rec suspended_lit_len phy indent current: _ list -> _  = function
  | Str s :: q ->
    debug "suspended lit str «%s» %a ⇒ %a" s Move.pp current
      Move.pp Move.(current +> phy#len (Raw.all s));
    suspended_lit_len phy indent Move.(current +> phy#len (Raw.all s)) q
  | Newline more :: q->
    debug "suspended newline %a ⇒ %a" Move.pp current
      Move.pp Move.( current + absolute indent +> more );
    suspended_lit_len phy indent
      Move.(current + absolute indent +> more) q
  | Space sp :: q->
    debug "suspended space %a ⇒ %a" Move.pp current
      Move.pp Move.(current +> sp);
    suspended_lit_len phy indent Move.(current +> sp) q
  | Box box :: q ->
    debug "Computed box len";
    suspended_lit_len phy indent
      (suspended_lit_len phy (Move.pos current) current box) q
  | [] -> debug "lit: ⇒ %a" Move.pp current; current

let is_vertical (x: box) = match x with V _ -> true | _ -> false

let suspended_len_tok phy tok ((kind:box),indent,current) =
  kind, indent,
  match tok with
  | Lit l ->
    debug "suspendend len lit";
    suspended_lit_len phy indent current [l]
  | Break b ->
    match kind with
    | V more ->
      debug "resolved V box break i:%d m:%d bi:%d, %a ⇒ %a"
        indent more b.indent Move.pp current Move.pp
        Move.( current + absolute indent +> more +> b.indent );
      Move.(current + absolute indent +> more +> b.indent)
    | _ ->
      debug "suspended break %a ⇒ %a"
        Move.pp current Move.pp Move.(current +> b.space);
      Move.( current +> b.space )

let suspended_len_box (kind,_) (_,_,current) =
  (kind, Move.pos current, current)

let suspended_len phy stream space direct =
  let _, _, all =
    Q.fold (suspended_len_tok phy) suspended_len_box stream
      (direct.kind, direct.indent, Move.(pure direct.current +> space)) in
  debug "reevaluted size: %d ⇒ %a "direct.current Move.pp all;
  all

let max_indent geom = geom.G.max_indent


let recheck info ppf =
  debug "ppf check:%s" info;
  match ppf.status with
  | Suspended sd ->
    let after =  suspended_len ppf.position.phy sd.after
        sd.break.space ppf.position in
    let right = sd.right in
    debug "expected right position: %a; recomputed: %a" Move.pp right
      Move.pp after;
    assert (after = right ) ; ppf
  | _ -> ppf

let check info ppf = ignore (recheck info ppf)

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
  | Major ((b , _), rest) ->
    debug "advance open box %a" pp_box b;
    (* When we meet a box without any ambiguity on its position,
       we put the current box on the left stack, and focus on this
       new box *)
    let indent = c.current in
    let context: open_box_on_the_left list =
      { kind = c.kind; indent = c.indent } :: context in
    advance_to_next_ambiguity geom context rest right
      { c with indent; kind = b }
  | Minor(Break b, after) ->
    debug "anam: minor break: right at start: %a" Move.pp right;
    begin
      match c.kind with
      | V _ -> (* this happens with suspended V box nested inside other boxes *)
        let right = Move.commit_line 1 right in
        c |> phyline (max_indent geom) b.indent
        |> advance_to_next_ambiguity geom context after right
      | _ ->
        let status =
          let right =
            match c.kind with V _ -> Move.commit_line 1 right | _ -> right in
          let rright = suspended_len c.phy after b.space c in
          debug "anam: expected right point: %a; got: %a%!"
            Move.pp right Move.pp rright;
          assert (right = rright );

          Suspended {
            break = b;
            after;
            right;
          } in
        debug "Advance up to new break, stop at %d" c.current;
        make context status c
    end
  | Minor(Lit s, rest) ->
    debug "advance lit";
    let newlines, c = commit_resolved_literal (max_indent geom) [s] (0,c) in
    let right = Move.commit_line newlines right in
    let rright = suspended_len c.phy rest 0 c in
    assert (right = rright );
    debug "after lit: right %a" Move.pp right;
    advance_to_next_ambiguity geom context rest right c
  | Empty ->
    debug "empty stream: stop advance at %d" c.current;
    make context Direct c

let rec advance_to_next_box max_indent stream right c =
  match Q.take_front stream with
  | Major _ ->
    debug "Next box reached at %d" c.current;
    c, right, stream
  | Minor(Break b, stream) ->
    let indent = newline_indent ~max_indent ~more:b.indent c in
    debug "indent in hv: %d" indent;
    let r = right in
    let right = Move.decrease_all b.space right in
    debug "next box: break %a ⇒ %a" Move.pp r Move.pp right;
    c |> phyline max_indent b.indent
    |> advance_to_next_box max_indent stream right
  | Minor(Lit s, stream) ->
    debug "next box lit starting at %d" c.current;
    let n, c' = (0,c) |> commit_resolved_literal max_indent [s] in
    let r = right in
    let right = if n = 0 then Move.(right +> (c.current - c'.current) )
      else right in
    debug "next box: lit %a ⇒ %a" Move.pp r Move.pp right;
    advance_to_next_box max_indent stream (Move.commit_line n right) c'
  | Empty -> c, right, Q.empty

let advance_to_next_ambiguity geom ctx stream right c =
  debug "Advance up to new break, start at %d" c.current;
  advance_to_next_ambiguity geom ctx stream right c

let actualize_break geom context sd c =
  let before = c.current in
  (*let indent =
    newline_indent ~max_indent:(max_indent geom) ~more:sd.break.indent c in*)
  let c = newline geom sd.break.indent c in
  (*  let right = Move.commit_line 1 Move.(sd.right + absolute indent) in*)
  match c.kind with
  | HV n ->
    let indent =
      newline_indent ~max_indent:(max_indent geom) ~more:sd.break.indent c in
    let diff = indent - before -sd.break.space in
    debug "HV break, before:%d, indent:%d, diff:%d %a" before indent diff
    Move.pp sd.right;
    let c, right, stream =
      advance_to_next_box (max_indent geom) sd.after Move.(sd.right +> diff)
        { c with kind = V n } in
    debug "HV break stopped at %a" Move.pp right;
    recheck "actualized break" @@
      advance_to_next_ambiguity geom context stream right c
  | _ ->
    debug "Standard break";
    let right =
      Move.decrease (before + sd.break.space - c.current) sd.right in
    advance_to_next_ambiguity geom context sd.after right c

let rec string geom s ppf =
  check "string start" ppf;
  match ppf.status with
  | Direct ->
    direct_string ppf.context ppf.position s
  | Suspended sd ->
    let right = Move.( sd.right +> ppf.position.phy#len (Raw.all s) ) in
    debug "suspended printing «%s» %d|(%a) ⇒ (%a) > %d ? "
      s ppf.position.indent Move.pp sd.right Move.pp right geom.G.margin;
    if Move.pos right > geom.G.margin then
         ppf.position
      |> actualize_break geom ppf.context sd
      |> string geom s
    else
      ( debug "suspending «%s» pos: %a" s Move.pp right;
        let after = Q.push_min (Lit (Str s)) sd.after in
        recheck "string stop"
          { ppf with status = Suspended { sd with after; right } }
     )



let reactivate position (context: open_box_on_the_left list) =
   match context with
     | [] ->
       debug "reopened empty";
       { position with indent = 0; kind = default_box }, ([]: _ list)
     (* empty box *)
     | {kind;indent} :: q ->
       debug "Reopened %a ⇒ %d" pp_box kind indent;
       { position with indent; kind }, q


let translate_break (b:break_data) box =
  if is_vertical box then Newline (b.indent + box_indent box) else Space b.space

let resolve_box bx stream =
  debug "resolving %a box" pp_box bx;
  let translate bx = function
    | Lit s -> s
    | Break b -> translate_break b bx in
  let x = stream |> Sequence.map (translate bx) |> Sequence.to_list in
  Box x

let coalesce geom context sp c =
  match Q.take_major_back sp.after with
  | Some (rest, (b,_)), last_box_content ->
    let lit = resolve_box b last_box_content in
    let after = Q.push_min (Lit lit) rest in
    make context (Suspended {sp with after}) c
  | None, last_box_content ->
    let b = c.kind in
    (*    let c, context = reactivate c context in*)
    let lit = resolve_box b last_box_content in
    let c = physpace sp.break.space c in
    let c = (0, c)
            |> commit_resolved_literal (max_indent geom) [lit]
            |> snd in
    let c, context = reactivate c context in
    make context Direct c

let close_box geom ppf =
  check "close box" ppf;
  match ppf.status with
  | Direct ->
    debug "close box %a" pp_box ppf.position.kind;
    let position, context = reactivate ppf.position ppf.context in
    { ppf with position; context }
  | Suspended sp ->
    (match Q.peek_major_back sp.after with
     |None ->
       debug "final coalesce %a " pp_box ppf.position.kind
     | Some (b,_) -> debug "intermediary coalesce %a" pp_box b
    );
    coalesce geom ppf.context sp ppf.position |> recheck "close box end"

let eager_indent: box -> _ = function
  | B _ -> true
  | _ -> false

let last_active_box pos sp =
  match Q.peek_major_back sp.after with
  | Some (b,pos) -> b, pos
  | None ->  pos.kind, Move.pure pos.indent

let rec break geom br (ppf: _ t) =
  debug "break {space=%d;indent=%d}" br.space br.indent;
  check "break" ppf;
  let c = ppf.position in
  match ppf.status with
  | Direct -> begin match c.kind with
      | H -> update ppf (physpace br.space c)
      | V _ -> update ppf (newline geom br.indent c)
      | b ->
        if c.current + br.space > geom.G.margin
        || (eager_indent b &&
            c.indent + br.indent + box_indent b < c.last_indent)
        then
            let kind: box = match c.kind with HV n -> V n | b -> b in
            { c  with kind }
            |> newline geom br.indent
            |> update ppf
          else
            let status =
              Suspended { break = br;
                          after = Q.empty;
                          right = Move.pure (c.current + br.space);
                        } in
            { ppf with status } end
  | Suspended sd ->
    let right = match last_active_box c sd with
      | V k, m ->
        Move.( sd.right + absolute k +> br.indent +> Move.pos m )
      | _ -> Move.(sd.right +> br.space ) in
    debug "computed position after break: %a" Move.pp right;
    let after = Q.push_min (Break br) sd.after  in
    debug "waiting for break resolution at %d" c.current;
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
        debug "Eager break resolution in Hov/B case at %a" Move.pp right;
        c
        |> physpace sd.break.space
        |> advance_to_next_ambiguity geom ppf.context after right
      | _ ->
        debug "break in %a ⇒ %a"  pp_box c.kind Move.pp right;
        recheck "break end"
          { ppf with status = Suspended { sd with after; right } }


let rec full_break geom br (ppf:_ t) =
  check "full break" ppf;
  let pos = ppf.position in
  match ppf.status with
  | Direct ->
    pos |> newline geom br |> update ppf
  | Suspended sd ->
    match pos.kind with
    | HoV _ | B _ when not (Q.secondary sd.after) ->
      pos |> physpace sd.break.space
      |> advance_to_next_ambiguity geom ppf.context sd.after sd.right
      |> full_break geom br
    | _ ->
      debug "breaking with a full break at %d" pos.current;
      pos |> actualize_break geom ppf.context sd |> full_break geom br

let rec open_box geom b (ppf: _ t) =
  check "open_box" ppf;
  debug "opening %a" pp_box b;
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
      { ppf with context; position }
  | Suspended s ->
    debug "pos: %a > box margin %d ?" Move.pp s.right geom.G.box_margin;
    if Move.pos s.right > geom.G.box_margin then
      pos |> actualize_break geom ppf.context s |> open_box geom b
    else
      let status = Suspended
          { s with after = Q.push_maj (b,s.right) s.after } in
      { ppf with status }

type ('a,'b) prim = Geometry.t -> 'a -> 'b t -> 'b t
let start phy =
  { status = Direct;
    position= {current=0;indent=0; kind=default_box; last_indent=0; phy };
    context = [] }

let close_box geom () = close_box geom
let flush (ppf: _ t) = ppf.position.phy#flush
