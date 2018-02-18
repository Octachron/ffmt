(** Formatting engine implementation *)

type dev = { geom: Geometry.t; phy: Spec.phy }

let debug fmt = Printf.fprintf stderr ("debug engine: "^^fmt ^^ "\n%!")

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


type t = {
  context: open_box_on_the_left list;
  position: position;
  status:status;
}

let len = String.length

module I = Geometry.Indentation


let box_indent: box -> int = function
  | B n | HV n | HoV n | V n-> n
  | H -> 0

let phyline max_indent more (d:position) (phy:Spec.phy) =
    let indent = min (d.indent + more + box_indent d.kind) max_indent in
    phy.break ();
    phy.indent {I.line = 0; column=indent};
    debug "line break ⇒ %d" indent;
    { d with current = indent; last_indent = indent }

let reindent n: box -> box = function
  | B _ -> B n | HV _ -> HV n | HoV _ -> HoV n | V _ -> V n
  | H -> H

let phyreset kind (phy:Spec.phy) =
  let indent = 0 in
  let kind = reindent 0 kind in
  phy.break ();
  phy.indent {I.line = 1; column=indent};
  debug "line reset";
  { current = indent; indent; last_indent = indent; kind }


let newline {geom;phy} more (d:position) =
  phyline geom.max_indent more d phy

let physpace (phy:Spec.phy) n (d:position) =
  phy.space n;
  { d with current = n + d.current }
(*
let space n d ppf =
  { ppf with status = Direct (physpace n d ppf.logical.phy) }
*)

let phystring s d (phy:Spec.phy) =
  debug "direct printing «%s», %d ⇒ %d" s d.current (d.current + len s);
  phy.string (Spec.all s);
  { d with current = d.current + len s }

let direct_string phy context d s  =
  let position = phystring s d phy in
  let status = Direct in
  { context; status; position }

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

let max_indent dev = dev.geom.max_indent

let commit_resolved_literal max_indent phy (d:position) =
  let rec elt phy d =  function
    | Str s -> phystring s d phy
    | Newline more -> phyline max_indent more d phy
    | Space sp -> physpace phy sp d
    | Box b ->
      let inside = List.fold_left (elt phy)
          {d with indent = d.current; kind = B 0 } b in
      { d with current = inside.current } in
  List.fold_left (elt phy) d

let rec advance_to_next_ambiguity dev context (position:position) stream =
  match S.(take front) stream with
  | Open_box b, rest ->
    debug "advance open box %a" pp_box b;
    (* When we meet a box without any ambiguity on its position,
       we put the current box on the left stack, and focus on this
       new box *)
    let indent = position.current in
    let context: open_box_on_the_left list =
      { kind = position.kind; indent = position.indent } :: context in
    advance_to_next_ambiguity dev context
      { position with indent; kind = b }  rest
  | Break b, rest ->
    let status =
      Suspended {
                  break = b;
                  after_block = rest;
                  right = suspended_len stream position;
                } in
    debug "Advance up to new break, stop at %d" position.current;
    { status; position; context }
  | Lit s, rest ->
    debug "advance lit";
    let direct =
      commit_resolved_literal (max_indent dev) dev.phy position [s] in
    advance_to_next_ambiguity dev context direct rest
  | exception Not_found ->
    debug "empty stream: stop advance at %d" position.current;
    { context; status = Direct; position }

let rec advance_to_next_box max_indent (position:position) phy stream =
  match S.(take front) stream with
  | Open_box b, stream -> { position with kind = b }
  | Break b, stream ->
    advance_to_next_box max_indent
      (phyline max_indent b.indent position phy) phy stream
  | Lit s, stream ->
    advance_to_next_box max_indent
      (commit_resolved_literal max_indent phy position [s]) phy stream
  | exception Not_found -> position

let advance_to_next_ambiguity dev ctx position stream =
  debug "Advance up to new break, start at %d" position.current;
  advance_to_next_ambiguity dev ctx position stream

let actualize_break dev context sd position =
  let position = newline dev sd.break.indent position in
  match position.kind with
  | HV n ->
    let position = advance_to_next_box (max_indent dev)
        { position with kind = V n } dev.phy sd.after_block in
   { context; position; status = Direct }
  | b ->
    advance_to_next_ambiguity dev context position sd.after_block


let rec string dev s ppf =
  match ppf.status with
  | Direct ->
    direct_string dev.phy ppf.context ppf.position s
  | Suspended sd ->
    let right = sd.right + len s in
    debug "suspended printing «%s» %d|%d ⇒ %d > %d ? "
      s ppf.position.indent sd.right right dev.geom.margin;
    if right > dev.geom.margin then
      ppf.position
      |> actualize_break dev ppf.context sd
      |> string dev s
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

let coalesce dev context position sp =
  let bx = last_box context sp.after_block in
  let lit, rest = resolve_box bx sp sp.after_block [] in
  match rest with
  | None ->
    let position, context = reactivate position context in
    let position =
      commit_resolved_literal (max_indent dev) dev.phy position [lit] in
    { position; context; status = Direct }
  | Some rest ->
    let after_block = S.push_back (Lit lit) rest  in
    { position; context; status = Suspended { sp with after_block } }

let close_box dev ppf =
  match ppf.status with
  | Direct ->
    debug "close box %a" pp_box ppf.position.kind;
    let position, context = reactivate ppf.position ppf.context in
    { ppf with position; context }
  | Suspended sp ->
    debug "coalesce box %a" pp_box ppf.position.kind;
    coalesce dev ppf.context ppf.position sp

let eager_indent: box -> _ = function
  | B _ -> true
  | _ -> false

let rec break dev br ppf =
  debug "break {space=%d;indent=%d}" br.space br.indent;
  let pos = ppf.position in
  match ppf.status with
  | Direct -> begin match pos.kind with
      | H -> { ppf with position = physpace dev.phy br.space pos }
      | V more -> { ppf with position = newline dev br.indent pos }
      | b ->
        if pos.current + br.space > dev.geom.margin
        || (eager_indent b &&
            pos.indent + br.indent + box_indent b  < pos.last_indent)
        then
            let kind: box = match pos.kind with HV n -> V n | b -> b in
            let position = newline dev br.indent { pos with kind } in
            { ppf with position }
          else
            let status =
              Suspended { break = br;
                          after_block = S.empty;
                          right = pos.current + br.space;
                        } in
            { ppf with status } end
  | Suspended sd ->
    let right = sd.right + br.space in
    let after_block = S.push_back (Break br) sd.after_block  in
    debug "waiting for break resolution at %d" pos.current;
    let b = pos.kind in
    if right > dev.geom.margin
    || (eager_indent b && pos.indent + br.indent + box_indent b <
                          pos.last_indent)
    then
      ppf.position |> actualize_break dev ppf.context sd |> break dev br
    else match pos.kind with
    | HoV _ | B _ when not (secondary_box sd.after_block) ->
      let position = physpace dev.phy sd.break.space pos in
      advance_to_next_ambiguity dev ppf.context position after_block
    | _ ->
      let right = suspended_len after_block pos in
      debug "break in not hov ⇒ %d" right;
      { ppf with status = Suspended { sd with after_block; right  } }


let rec full_break dev br ppf =
  let pos = ppf.position in
  match ppf.status with
  | Direct ->
    { ppf with position = newline dev br pos }
  | Suspended sd ->
    match pos.kind with
    | HoV _ | B _ when not (secondary_box sd.after_block) ->
      let position = physpace dev.phy sd.break.space pos in
      advance_to_next_ambiguity dev ppf.context position sd.after_block
      |> full_break dev br
    | _ ->
      debug "breaking with a full break at %d" pos.current;
      pos |> actualize_break dev ppf.context sd |> full_break dev br

let rec open_box dev b ppf =
  let pos = ppf.position in
  match ppf.status with
  | Direct ->
    if pos.current > dev.geom.box_margin then
      let ppf = { ppf with position = phyreset pos.kind dev.phy } in
      open_box dev b ppf
    else
      let position = { pos with kind = b; indent = pos.current } in
      let context: open_box_on_the_left list =
        {indent = pos.indent; kind = pos.kind } :: ppf.context in
      { ppf with context; position }
  | Suspended s ->
    if s.right > dev.geom.box_margin then
      pos |> actualize_break dev ppf.context s |> open_box dev b
    else
      let status = Suspended
          { s with after_block = S.push_back (Open_box b) s.after_block } in
      { ppf with status }


let start =
  { status = Direct;
    position= {current=0;indent=0; kind=H; last_indent=0 };
    context = [] }

type 'a prim = Geometry.t -> Spec.phy -> 'a -> t -> t
let lift f geom phy = f {geom;phy}

let string = lift string
let break = lift  break
let full_break = lift full_break
let close_box geom phy ()  =  close_box {geom;phy}
let open_box = lift open_box
