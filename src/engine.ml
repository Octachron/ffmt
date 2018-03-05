(** Formatting engine implementation *)

let debug fmt = Printf.ifprintf stderr ("debug engine: "^^fmt ^^ "\n%!")

let pp_box (ppf:out_channel): Format.box -> unit = function
  | V n -> Printf.fprintf ppf "V %d" n
  | HV n -> Printf.fprintf ppf "HV %d" n
  | HoV n -> Printf.fprintf ppf "HoV %d" n
  | B n -> Printf.fprintf ppf "B %d" n
  | H -> Printf.fprintf ppf "H"


open Format

type open_box_on_the_left = {indent:int;kind:box}

type resolved_lit =
  | Str of string
  | Space of int
  | Newline of int
  | Box of resolved_lit list
type suspended_lit =
    Break of break_data | Lit of resolved_lit | Open_box of box

module S = struct
  module M= Map.Make(struct
      type t = int
      let compare (x:int) (y:int) = compare x y
    end)

  type 'a t = 'a M.t
  let front = M.min_binding
  let back = M.max_binding

  let max x = fst @@ back x
  let min x = fst @@ front x
  let last x = snd @@ back x
  let first x = snd @@ front x

  let push_back x m =
    if M.cardinal m = 0 then
      M.add 0 x m
    else
      let max = max m in
      M.add (max +1) x m

  let take pos m =
    let max, elt =  pos m in
    elt, M.remove max m

  let cardinal = M.cardinal
  let empty = M.empty
  let fold = M.fold
end

type context =
  { indent: int;
    left_context: open_box_on_the_left list;
    (** on the left, we have those inactive boxes *)
  }

type position =
  {
    current:int; (** we are currently writing at position *)
    kind:box; (** with this box context *)
    indent:int; (** and at this indentation *)
    last_indent:int (** the previous line break was done with this indent *)
  }

type suspended = {
  break:break_data;
  (** We are in the suspended mode because we don't know how to interpret
      this break yet *)
  after_block: suspended_lit S.t;
  (** we already have computed these token after the block *)
  right: int (** and we are currently at this position < margin *);
}


type status =
  | Direct
  (** We are directly writing to the logical device *)

  | Suspended of suspended
  (**We are blocked at an ambiguous break, waiting for the decision on
     how to interpret it *)

let secondary_box str =
  S.M.exists (fun _ -> function Open_box _ -> true | _ -> false) str

let only_boxes str =
  S.M.for_all (fun _ -> function Open_box _ -> true | _ -> false) str


type 'a t = {
  context: open_box_on_the_left list;
  position: position;
  status:status;
  phy: 'a Raw.t
}

type 'a cursor = { position:position; phy: 'a Raw.t }
let cursor (ppf:'a t) = { position = ppf.position; phy=ppf.phy }
let all context status (c:_ cursor) =
  { context;status; phy=c.phy; position= c.position }
let update ppf cursor = all ppf.context ppf.status cursor

let len = String.length

module G = Geometry
module I = Geometry.Indentation


let box_indent: box -> int = function
  | B n | HV n | HoV n | V n-> n
  | H -> 0

let phyline max_indent more {position=d; phy} =
  let indent = min (d.indent + more + box_indent d.kind) max_indent in
  debug "line break ⇒ %d" indent;
  { phy = phy#break#indent {I.line = 0; column=indent};
    position ={ d with current = indent; last_indent = indent }
  }

let reindent n: box -> box = function
  | B _ -> B n | HV _ -> HV n | HoV _ -> HoV n | V _ -> V n
  | H -> H

let phyreset kind (phy:_ Raw.t) =
  let indent = 0 in
  let kind = reindent 0 kind in
  debug "line reset";
  { phy = phy#break#indent {I.line = 1; column=indent};
    position={ current = indent; indent; last_indent = indent; kind }
  }


let newline geom more (c: _ cursor) =
  phyline geom.G.max_indent more c

let physpace  n {phy; position} =
  { phy = phy#space n;
    position = { position with current = n + position.current } }
(*
let space n d ppf =
  { ppf with status = Direct (physpace n d ppf.logical.phy) }
*)

let phystring s (c: _ cursor) =
  debug "direct printing «%s», %d ⇒ %d" s c.position.current
    (c.position.current + len s);
  { phy = c.phy#string (Raw.all s);
   position ={ c.position with current = c.position.current + len s }
  }

let direct_string context c s  =
  let {position;phy} = phystring s c in
  { context; status=Direct; position; phy }

let rec suspended_lit_len indent current: _ list -> _  = function
  | Str s :: q ->
    debug "str %d ⇒ %d" current (current + len s);
    suspended_lit_len indent (current + len s) q
  | Newline more :: q->
      suspended_lit_len (indent + more) (indent + more) q
  | Space sp :: q-> suspended_lit_len indent (current + sp) q
  | Box box :: q ->
    suspended_lit_len indent (suspended_lit_len current current box) q
  | [] -> debug "lit: ⇒ %d" current; current

let is_vertical (x: box) = match x with V _ -> true | _ -> false

let suspended_len_tok direct = function
  | Break b -> begin
      match direct.kind with
      | V more -> { direct with current = direct.indent + more + b.indent }
      | _ ->
        debug "break %d ⇒ %d" direct.current (direct.current + b.space);
        { direct with current = direct.current + b.space }
    end
  | Open_box v -> { direct with indent = direct.current; kind = v }
  | Lit l ->
    { direct with current =
                    suspended_lit_len direct.indent direct.current [l]}

let suspended_len stream direct =
  let all =
    S.fold (fun _key tok direct -> suspended_len_tok direct tok)
      stream direct in
  debug "reevaluted size: %d ⇒ %d "direct.current all.current;
  all.current

let max_indent geom = geom.G.max_indent

let commit_resolved_literal max_indent lits c =
  let rec elt c =  function
    | Str s -> phystring s c
    | Newline more -> phyline max_indent more c
    | Space sp -> physpace sp c
    | Box b ->
      let d = c.position in
      let inside = List.fold_left elt
          { c with position = {d with indent = d.current; kind = B 0 } }
          b in
      { c with position = { d with current = inside.position.current } } in
  List.fold_left elt c lits

let rec advance_to_next_ambiguity geom context stream (c: _ cursor) =
  match S.(take front) stream with
  | Open_box b, rest ->
    debug "advance open box %a" pp_box b;
    (* When we meet a box without any ambiguity on its position,
       we put the current box on the left stack, and focus on this
       new box *)
    let indent = c.position.current in
    let context: open_box_on_the_left list =
      { kind = c.position.kind; indent = c.position.indent } :: context in
    advance_to_next_ambiguity geom context rest
      { c with position = { c.position with indent; kind = b } }
  | Break b, rest ->
    let status =
      Suspended {
                  break = b;
                  after_block = rest;
                  right = suspended_len stream c.position;
                } in
    debug "Advance up to new break, stop at %d" c.position.current;
    all context status c
  | Lit s, rest ->
    debug "advance lit";
       c
    |> commit_resolved_literal (max_indent geom) [s]
    |> advance_to_next_ambiguity geom context rest
  | exception Not_found ->
    debug "empty stream: stop advance at %d" c.position.current;
    all context Direct c

let rec advance_to_next_box max_indent stream c =
  match S.(take front) stream with
  | Open_box b, stream -> { c with position = { c.position with kind = b } }
  | Break b, stream ->
    c |> phyline max_indent b.indent
    |> advance_to_next_box max_indent stream
  | Lit s, stream ->
    c |> commit_resolved_literal max_indent [s]
      |> advance_to_next_box max_indent stream
  | exception Not_found -> c

let advance_to_next_ambiguity geom ctx stream c =
  debug "Advance up to new break, start at %d" c.position.current;
  advance_to_next_ambiguity geom ctx stream c

let actualize_break geom context sd c =
  let c = newline geom sd.break.indent c in
  match c.position.kind with
  | HV n ->
    { c with position = { c.position with kind = V n } }
    |> advance_to_next_box (max_indent geom) sd.after_block
    |> all context Direct
  | b ->
    advance_to_next_ambiguity geom context sd.after_block c


let rec string geom s ppf =
  match ppf.status with
  | Direct ->
    direct_string ppf.context (cursor ppf) s
  | Suspended sd ->
    let right = sd.right + len s in
    debug "suspended printing «%s» %d|%d ⇒ %d > %d ? "
      s ppf.position.indent sd.right right geom.G.margin;
    if right > geom.G.margin then
         cursor ppf
      |> actualize_break geom ppf.context sd
      |> string geom s
    else
      ( debug "suspending «%s»" s;
        let after_block = S.push_back (Lit (Str s)) sd.after_block in
        { ppf with status = Suspended { sd with after_block; right } }
     )



let reactivate position (context: open_box_on_the_left list) =
   match context with
     | [] ->
       debug "reopened empty";
       { position with indent = 0; kind = H }, ([]: _ list)
     (** empty box *)
     | {kind;indent} :: q ->
       debug "Reopened %a ⇒ %d" pp_box kind indent;
       { position with indent; kind }, q


let translate_break (b:break_data) box =
  if is_vertical box then Newline (b.indent + box_indent box) else Space b.space

let last_box (context: open_box_on_the_left list) str =
  let rec search mx =
    match S.M.find_opt mx str with
    | Some Open_box b -> b
    | Some _ -> search (mx - 1)
    | None -> match context with
      | [] -> H
      | a :: _ -> a.kind in
  search (S.max str)


let rec resolve_box bx sp stream lits =
  match S.(take back) stream with
  | Open_box b, stream ->
    debug "end coalesce, right:%d" sp.right;
    Box lits, Some stream
  | Lit s, stream -> resolve_box bx sp stream (s :: lits)
  | Break b, stream ->
    let lit = translate_break b bx in
    resolve_box bx sp stream (lit :: lits)
  | exception Not_found ->
      let lit = translate_break sp.break bx in
      Box (lit :: lits), None

let coalesce geom context sp c =
  let bx = last_box context sp.after_block in
  let lit, rest = resolve_box bx sp sp.after_block [] in
  match rest with
  | None ->
    let position, context = reactivate c.position context in
    { c with position }
    |> commit_resolved_literal (max_indent geom) [lit]
    |> all context Direct
  | Some rest ->
    let after_block = S.push_back (Lit lit) rest  in
    all context (Suspended { sp with after_block }) c

let close_box geom ppf =
  match ppf.status with
  | Direct ->
    debug "close box %a" pp_box ppf.position.kind;
    let position, context = reactivate ppf.position ppf.context in
    { ppf with position; context }
  | Suspended sp ->
    debug "coalesce box %a" pp_box ppf.position.kind;
    coalesce geom ppf.context sp (cursor ppf)

let eager_indent: box -> _ = function
  | B _ -> true
  | _ -> false

let rec break geom br (ppf: _ t) =
  debug "break {space=%d;indent=%d}" br.space br.indent;
  let c = cursor ppf in
  match ppf.status with
  | Direct -> begin match c.position.kind with
      | H -> update ppf (physpace br.space c)
      | V more -> update ppf (newline geom br.indent c)
      | b ->
        if c.position.current + br.space > geom.G.margin
        || (eager_indent b &&
            c.position.indent + br.indent + box_indent b
            < c.position.last_indent)
        then
            let kind: box = match c.position.kind with HV n -> V n | b -> b in

            { c with position = {c.position with kind }}
            |> newline geom br.indent
            |> update ppf
          else
            let status =
              Suspended { break = br;
                          after_block = S.empty;
                          right = c.position.current + br.space;
                        } in
            { ppf with status } end
  | Suspended sd ->
    let right = sd.right + br.space in
    let after_block = S.push_back (Break br) sd.after_block  in
    debug "waiting for break resolution at %d" c.position.current;
    let b = c.position.kind in
    if right > geom.G.margin
    || (eager_indent b && c.position.indent + br.indent + box_indent b <
                          c.position.last_indent)
    then
      { position = ppf.position; phy = ppf.phy }
      |> actualize_break geom ppf.context sd
      |> break geom br
    else match c.position.kind with
    | HoV _ | B _ when not (secondary_box sd.after_block) ->
         cursor ppf
      |> physpace sd.break.space
      |> advance_to_next_ambiguity geom ppf.context after_block
    | _ ->
      let right = suspended_len after_block c.position in
      debug "break in not hov ⇒ %d" right;
      { ppf with status = Suspended { sd with after_block; right  } }


let rec full_break geom br (ppf:_ t) =
  let pos = ppf.position in
  match ppf.status with
  | Direct ->
    ppf |> cursor |> newline geom br |> update ppf
  | Suspended sd ->
    match pos.kind with
    | HoV _ | B _ when not (secondary_box sd.after_block) ->
      ppf |> cursor |> physpace sd.break.space
      |> advance_to_next_ambiguity geom ppf.context sd.after_block
      |> full_break geom br
    | _ ->
      debug "breaking with a full break at %d" pos.current;
      ppf |> cursor |> actualize_break geom ppf.context sd
      |> full_break geom br

let rec open_box geom b (ppf: _ t) =
  let pos = ppf.position in
  match ppf.status with
  | Direct ->
    if pos.current > geom.G.box_margin then
      phyreset pos.kind ppf.phy |> update ppf
      |> open_box geom b
    else
      let position = { pos with kind = b; indent = pos.current } in
      let context: open_box_on_the_left list =
        {indent = pos.indent; kind = pos.kind } :: ppf.context in
      { ppf with context; position }
  | Suspended s ->
    if s.right > geom.G.box_margin then
      ppf |> cursor |> actualize_break geom ppf.context s
      |> open_box geom b
    else
      let status = Suspended
          { s with after_block = S.push_back (Open_box b) s.after_block } in
      { ppf with status }

type ('a,'b) prim = Geometry.t -> 'a -> 'b t -> 'b t
let start phy =
  { status = Direct;
    position= {current=0;indent=0; kind=H; last_indent=0 };
    context = []; phy }

let close_box geom () = close_box geom

let flush (ppf: _ t) = ppf.phy#flush
(*
type 'a prim = Geometry.t -> Raw.t -> 'a -> t -> t
let lift f geom phy = f {geom;phy}

let string = lift string
let break = lift  break
let full_break = lift full_break
let close_box geom phy ()  =  close_box {geom;phy}
let open_box = lift open_box
*)
