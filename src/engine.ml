
let debug fmt = Printf.fprintf stderr ("debug engine: "^^fmt ^^ "\n%!")

let pp_box (ppf:out_channel): Format.box -> unit = function
  | V n -> Printf.fprintf ppf "V %d" n
  | HV n -> Printf.fprintf ppf "HV %d" n
  | HoV n -> Printf.fprintf ppf "HoV %d" n
  | B n -> Printf.fprintf ppf "B %d" n
  | H -> Printf.fprintf ppf "H"


open Format

type break = { space: int; indent: int }

type blocked_box = S_HV of int | S_HoV of int | S_B of int

type open_box_on_the_left = {indent:int;kind:box}

type resolved_lit =
    Str of string | Break of break
  | Box of {vertical:bool; more:int; content:resolved_lit list}
type suspended_lit =
    Break of break | Lit of resolved_lit | Open_box of box

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
  break:break; (** because we don't know how to interpret this break *)
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

let last_box current str =
  let rec search mx =
    match S.M.find_opt mx str with
    | Some Open_box b -> b
    | Some _ -> search (mx - 1)
    | None -> current in
  search (S.max str)

let secondary_box str =
  S.M.exists (fun _ -> function Open_box _ -> true | _ -> false) str

let only_boxes str =
  S.M.for_all (fun _ -> function Open_box _ -> true | _ -> false) str


type t = {
  logical: t Spec.t;
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


let newline more (d:position) ppf =
  phyline ppf.logical.geometry.max_indent more d ppf.logical.phy

let physpace n (d:position) (phy:Spec.phy) =
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

let direct_string s d ppf =
  let position = phystring s d ppf.logical.phy in
  let status = Direct in
  { ppf with status; position }

let rec suspended_lit_len vert indent current: _ list -> _  = function
  | Str s :: q ->
    debug "str %d ⇒ %d" current (current + len s);
    suspended_lit_len vert indent (current + len s) q
  | Break br :: q->
    if vert then suspended_lit_len vert (indent + br.indent) (indent + br.indent) q
    else suspended_lit_len vert indent (current + br.space) q
  | Box box :: q ->
    suspended_lit_len vert (indent + box.more)
      (suspended_lit_len box.vertical current current box.content) q
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
    let vertical = is_vertical direct.kind  in
    { direct with current =
                    suspended_lit_len vertical direct.indent direct.current [l]}

let suspended_len stream direct =
  let all =
    S.fold (fun _key tok direct -> suspended_len_tok direct tok)
      stream direct in
  debug "reevaluted size: %d ⇒ %d "direct.current all.current;
  all.current

let to_sbox: Format.box -> _ = function
  | HV n -> S_HV n
  | HoV n -> S_HoV n
  | B n -> S_B n
  | H | V _ -> raise (Invalid_argument "H or V box cannot be suspended")

let max_indent ppf = ppf.logical.geometry.max_indent

let commit_resolved_literal max_indent phy (d:position) =
  let rec elt phy d =  function
    | Str s -> phystring s d phy
    | Break br ->
      if is_vertical d.kind then phyline max_indent br.indent d phy
      else physpace br.space d phy
    | Box b ->
      let kind: box = if b.vertical then V b.more else d.kind in
      let inside =
        List.fold_left (elt phy) {d with indent = d.current; kind} b.content in
      { d with current = inside.current } in
  List.fold_left (elt phy) d

let rec advance_to_next_ambiguity (position:position) stream (ppf:t) =
  match S.(take front) stream with
  | Open_box b, rest ->
    debug "advance open box %a" pp_box b;
    (* When we meet a box without any ambiguity on its position,
       we put the current box on the left stack, and focus on this
       new box *)
    let indent = position.current in
    let context: open_box_on_the_left list =
      { kind = position.kind; indent = position.indent } :: ppf.context in
    advance_to_next_ambiguity
      { position with indent; kind = b } rest { ppf with context }
  | Break b, rest ->
    let status =
      Suspended {
                  break = b;
                  after_block = rest;
                  right = suspended_len stream position;
                } in
    debug "Advance up to new break, stop at %d" position.current;
    { ppf with status; position }
  | Lit s, rest ->
    debug "advance lit";
    let direct =
      commit_resolved_literal (max_indent ppf) ppf.logical.phy position [s] in
    advance_to_next_ambiguity direct rest ppf
  | exception Not_found ->
    debug "empty stream: stop advance at %d" position.current;
    { ppf with status = Direct; position }

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

let advance_to_next_ambiguity position stream ppf =
  debug "Advance up to new break, start at %d" position.current;
  advance_to_next_ambiguity position stream ppf

let actualize_break sd ppf =
  let position = newline sd.break.indent ppf.position ppf in
  match ppf.position.kind with
  | HV n ->
    let position = advance_to_next_box (max_indent ppf)
        { position with kind = V n } ppf.logical.phy sd.after_block in
   { ppf with position; status = Direct }
  | b ->
    ppf |> advance_to_next_ambiguity position sd.after_block


let rec string s ppf =
  match ppf.status with
  | Direct ->
    direct_string s ppf.position ppf
  | Suspended sd ->
    let right = sd.right + len s in
    debug "suspended printing «%s» %d|%d ⇒ %d > %d ? "
      s ppf.position.indent sd.right right ppf.logical.geometry.margin;
    if right > ppf.logical.geometry.margin then
      ppf |> actualize_break sd |> string s
    else
      ( debug "suspending «%s»" s;
        let after_block = S.push_back (Lit (Str s)) sd.after_block in
        { ppf with status = Suspended { sd with after_block; right } }
     )



let reactivate position ppf =
   match ppf.context with
     | [] ->
       debug "reopened empty";
       { position with indent = 0; kind = H }, ([]: _ list)
     (** empty box *)
     | {kind;indent} :: q ->
       debug "Reopened %a ⇒ %d" pp_box kind indent;
       { position with indent; kind }, q

let rec coalesce ppf sp stream lits =
  match S.(take back) stream with
  | Open_box b, stream ->
    debug "end coalesce, right:%d" sp.right;
    let status =
      let box =
        Box { vertical=is_vertical b; more = box_indent b; content = lits } in
      Suspended { sp with
                  after_block  = S.push_back (Lit box) stream; right = sp.right } in
    { ppf with status }
  | Lit s, stream -> coalesce ppf sp stream (s :: lits)
  | Break b, stream -> coalesce ppf sp stream (Break b :: lits)
  | exception Not_found ->
    let c =
      Box {vertical=is_vertical ppf.position.kind;
           more = box_indent ppf.position.kind;
           content = Break sp.break :: lits} in
    let status =
      Suspended { sp with after_block  = S.push_back (Lit c) stream } in
    let direct, (context: _ list) =
      reactivate ppf.position ppf in
    let stream = S.push_back (Lit c) S.empty in
    advance_to_next_ambiguity direct stream { ppf with context; status }

let close_box ppf =
  match ppf.status with
  | Direct ->
    debug "close box %a" pp_box ppf.position.kind;
    let position, context = reactivate ppf.position ppf in
    { ppf with position; context }
  | Suspended sp ->
    debug "coalesce box %a" pp_box ppf.position.kind;
    coalesce ppf sp sp.after_block []

let eager_indent: box -> _ = function
  | B _ -> true
  | _ -> false

let rec break br ppf =
  debug "break {space=%d;indent=%d}" br.space br.indent;
  let pos = ppf.position in
  match ppf.status with
  | Direct -> begin match pos.kind with
      | H -> { ppf with position = physpace br.space pos ppf.logical.phy }
      | V more -> { ppf with position = newline br.indent pos ppf }
      | b ->
        if pos.current + br.space > ppf.logical.geometry.margin
        || (eager_indent b &&
            pos.indent + br.indent + box_indent b  < pos.last_indent)
        then
            let kind: box = match pos.kind with HV n -> V n | b -> b in
            let position = newline br.indent { pos with kind } ppf in
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
    if right > ppf.logical.geometry.margin
    || (eager_indent b && pos.indent + br.indent + box_indent b <
                          pos.last_indent)
    then
      ppf |> actualize_break sd |> break br
    else match pos.kind with
    | HoV _ | B _ when not (secondary_box sd.after_block) ->
      let position = physpace sd.break.space pos ppf.logical.phy in
      advance_to_next_ambiguity position after_block ppf
    | _ ->
      let right = suspended_len after_block pos in
      debug "break in not hov ⇒ %d" right;
      { ppf with status = Suspended { sd with after_block; right  } }


let rec full_break br ppf =
  let pos = ppf.position in
  match ppf.status with
  | Direct ->
    { ppf with position = newline br pos ppf }
  | Suspended sd ->
    match pos.kind with
    | HoV _ | B _ when not (secondary_box sd.after_block) ->
      let position = physpace sd.break.space pos ppf.logical.phy in
      ppf |> advance_to_next_ambiguity position sd.after_block |> full_break br
    | _ ->
      debug "breaking with a full break at %d" pos.current;
      ppf |> actualize_break sd |> full_break br

let ntag ppf = List.length ppf.logical.open_tags 
let rec open_box b ppf =
  debug "open box %a/%d" pp_box b (ntag ppf) ;
  let pos = ppf.position in
  match ppf.status with
  | Direct ->
    if pos.current > ppf.logical.geometry.box_margin then
      let ppf = { ppf with position = phyreset pos.kind ppf.logical.phy } in
      open_box b ppf
    else
      let position = { pos with kind = b; indent = pos.current } in
      let context: open_box_on_the_left list =
        {indent = pos.indent; kind = pos.kind } :: ppf.context in
      { ppf with context; position }
  | Suspended s ->
    if s.right > ppf.logical.geometry.box_margin then
      ppf |> actualize_break s |> open_box b
    else
      let status = Suspended
          { s with after_block = S.push_back (Open_box b) s.after_block } in
      { ppf with status }
