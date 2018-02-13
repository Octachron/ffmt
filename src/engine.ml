
let debug fmt = Printf.eprintf ("debug: "^^fmt ^^ "\n%!")

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

type direct =
  {
    position:int; (** we are currently writing at position *)
    kind:box; (** with this box context *)
    indent:int; (** and at this indentation *)
    last_indent:int (** the previous line break was done with this indent *)
  }

type suspension =
      { blocked_at:int; (** we are blocked at this position *)
        break:break; (** because we don't know how to interpret this break *)
        box: Format.box; (** in this box context *)
        indent: int; (** with this indentation *)
        after_block: suspended_lit S.t;
        (** we already have computed these token after the block *)
        right: int (** and we are currently at this position < margin *);
        last_indent: int; (** the last line break was done with this indent *)
      }

let last_box sp str =
  let rec search mx =
    match S.M.find_opt mx str with
    | Some Open_box b -> b
    | Some _ -> search (mx - 1)
    | None -> sp.box in
  search (S.max str)

let secondary_box str =
  S.M.exists (fun _ -> function Open_box _ -> true | _ -> false) str

type status =
  | Direct of direct
  (** We are directly writing to the logical device *)

  | Suspended of suspension
  (**We are blocked at an ambiguous break, waiting for the decision on
     how to interpret it *)

type t = {
  logical: t Spec.t;
  context: open_box_on_the_left list;
  status:status;
}

let len = String.length

module I = Geometry.Indentation


let box_indent: box -> int = function
  | B n | HV n | HoV n | V n-> n
  | H -> 0

let phyline more (d:direct) (phy:Spec.phy) =
    let indent = d.indent + more + box_indent d.kind in
    phy.break ();
    phy.indent {I.line = 0; column=indent};
    debug "line break ⇒ %d" indent;
    { d with position = indent; last_indent = indent }

let newline more (d:direct) ppf =
  phyline more d ppf.logical.phy

let physpace n d (phy:Spec.phy) =
  phy.space n;
  { d with position = n + d.position }
(*
let space n d ppf =
  { ppf with status = Direct (physpace n d ppf.logical.phy) }
*)

let phystring s d (phy:Spec.phy) =
  debug "direct printing «%s», %d ⇒ %d" s d.position (d.position + len s);
  phy.string (Spec.all s);
  { d with position = d.position + len s }

let direct_string s d ppf =
  let status = Direct (phystring s d ppf.logical.phy) in
  { ppf with status }

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
      | V more -> { direct with position = direct.indent + more + b.indent }
      | _ ->
        debug "break %d ⇒ %d" direct.position (direct.position + b.space);
        { direct with position = direct.position + b.space }
    end
  | Open_box v -> { direct with indent = direct.position; kind = v }
  | Lit l ->
    let vertical = is_vertical direct.kind  in
    { direct with position =
                    suspended_lit_len vertical direct.indent direct.position [l]}

let suspended_len stream direct =
  let all =
    S.fold (fun _key tok direct -> suspended_len_tok direct tok)
      stream direct in
  debug "reevaluted size: %d ⇒ %d "direct.position all.position;
  all.position

let to_sbox: Format.box -> _ = function
  | HV n -> S_HV n
  | HoV n -> S_HoV n
  | B n -> S_B n
  | H | V _ -> raise (Invalid_argument "H or V box cannot be suspended")

let commit_resolved_literal phy (d:direct) =
  let rec elt phy d =  function
    | Str s -> phystring s d phy
    | Break br ->
      if is_vertical d.kind then phyline br.indent d phy
      else physpace br.space d phy
    | Box b ->
      let kind: box = if b.vertical then V b.more else d.kind in
      let inside =
        List.fold_left (elt phy) {d with indent = d.position; kind} b.content in
      { d with position = inside.position } in
  List.fold_left (elt phy) d

let rec advance_to_next_ambiguity (direct:direct) stream (ppf:t) =
  match S.(take front) stream with
  | Open_box b, rest ->
    debug "advance open box %a" pp_box b;
    (* When we meet a box without any ambiguity on its position,
       we put the current box on the left stack, and focus on this
       new box *)
    let indent = direct.position in
    let context: open_box_on_the_left list =
      { kind = direct.kind; indent = direct.indent } :: ppf.context in
    advance_to_next_ambiguity
      { direct with indent; kind = b } rest { ppf with context }
  | Break b, rest ->
    let status =
      Suspended { blocked_at = direct.position;
                  break = b;
                  indent = direct.indent;
                  box = direct.kind;
                  after_block = rest;
                  right = suspended_len stream direct;
                  last_indent = direct.last_indent;
                } in
    debug "Advance up to new break, stop at %d" direct.position;
    { ppf with status }
  | Lit s, rest ->
    debug "advance lit";
    let direct = commit_resolved_literal ppf.logical.phy direct [s] in
    advance_to_next_ambiguity direct rest ppf
  | exception Not_found ->
    debug "empty stream: stop advance at %d" direct.position;
    { ppf with status = Direct direct }

let rec advance_to_next_box (direct:direct) phy stream =
  match S.(take front) stream with
  | Open_box b, stream -> Direct { direct with kind = b }
  | Break b, stream -> advance_to_next_box (phyline b.indent direct phy) phy stream
  | Lit s, stream ->
    advance_to_next_box (commit_resolved_literal phy direct [s]) phy stream
  | exception Not_found -> Direct direct

let advance_to_next_ambiguity direct stream ppf =
  debug "Advance up to new break, start at %d" direct.position;
  advance_to_next_ambiguity direct stream ppf

let actualize_break sd right ppf after =
  debug "margin exceeded %d>%d" right ppf.logical.geometry.margin;
  let direct =
    { position = sd.blocked_at; indent = sd.indent;
      kind = sd.box; last_indent = sd.last_indent } in
  let direct = newline sd.break.indent direct ppf in
  match sd.box with
  | HV n ->
    let status = advance_to_next_box
        { direct with kind = V n } ppf.logical.phy after in
    { ppf with status }
  | b ->
    advance_to_next_ambiguity direct after ppf


let string s ppf =
  match ppf.status with
  | Direct d ->
    direct_string s d ppf
  | Suspended sd ->
    let right = sd.right + len s in
    let after_block = S.push_back (Lit (Str s)) sd.after_block in
    debug "suspended printing «%s» %d|%d ⇒ %d > %d ? "
      s sd.indent sd.right right ppf.logical.geometry.margin;
    if right > ppf.logical.geometry.margin then
      actualize_break sd right ppf after_block
    else
     ( debug "suspending «%s»" s;
      { ppf with status = Suspended { sd with after_block; right } }
     )

let open_box b ppf =
  debug "open box %a" pp_box b;
  match ppf.status with
  | Direct d ->
    let status = Direct { d with kind = b; indent = d.position } in
    let context: open_box_on_the_left list =
      {indent = d.indent; kind = d.kind } :: ppf.context in
    { ppf with context; status }
  | Suspended s ->
    let status = Suspended
        { s with after_block = S.push_back (Open_box b) s.after_block } in
    { ppf with status }

let reactivate position last_indent ppf =
   match ppf.context with
     | [] ->
       debug "reopened empty";
       { position; indent = 0; kind = H; last_indent }, ([]: _ list)
     (** empty box *)
     | {kind;indent} :: q ->
       debug "Reopened %a ⇒ %d" pp_box kind indent;
       { position; indent; kind; last_indent }, q

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
      Box {vertical=is_vertical sp.box; more = box_indent sp.box;
           content = Break sp.break :: lits} in
    let status =
      Suspended { sp with after_block  = S.push_back (Lit c) stream } in
    let direct, (context: _ list) =
      reactivate sp.blocked_at sp.last_indent ppf in
    let stream = S.push_back (Lit c) S.empty in
    advance_to_next_ambiguity direct stream { ppf with context; status }

let close_box ppf =
  debug "close box";
  match ppf.status with
  | Direct d ->
    let direct, context = reactivate d.position d.last_indent ppf in
    { ppf with status = Direct direct; context }
  | Suspended sp ->
    debug "coalesce";
    coalesce ppf sp sp.after_block []

let eager_indent: box -> _ = function
  | B _ -> true
  | _ -> false

let break br ppf =
  match ppf.status with
  | Direct d -> begin match d.kind with
      | H -> { ppf with status = Direct (physpace br.space d ppf.logical.phy) }
      | V more -> { ppf with status = Direct (newline more d ppf) }
      | b ->
        if d.position + br.space > ppf.logical.geometry.margin
        || (eager_indent b && d.indent + br.indent + box_indent b  < d.last_indent)
        then
            let kind: box = match d.kind with HV n -> V n | b -> b in
            let direct = newline br.indent { d with kind } ppf in
            { ppf with status = Direct direct }
          else
            let status =
              Suspended { blocked_at = d.position;
                          break = br;
                          box = b;
                          after_block = S.empty;
                          indent = d.indent;
                          right = d.position + br.space;
                          last_indent = d.last_indent
                        } in
            { ppf with status } end
  | Suspended sd ->
    let right = sd.right + br.space in
    let after_block = S.push_back (Break br) sd.after_block  in
    let direct =
      { position = sd.blocked_at; indent = sd.indent; kind = sd.box;
        last_indent = sd.last_indent } in
    debug "waiting for break resolution at %d" direct.position;
    let b = sd.box in
    if right > ppf.logical.geometry.margin
       || (eager_indent b && sd.indent + br.indent + box_indent b < sd.last_indent)
    then
      actualize_break sd right ppf after_block
    else match sd.box with
    | HoV _ | B _ when not (secondary_box sd.after_block) ->
      let direct = physpace sd.break.space direct ppf.logical.phy in
      advance_to_next_ambiguity direct after_block ppf
    | _ ->
      let right = suspended_len after_block direct in
      debug "break in not hov ⇒ %d" right;
      { ppf with status = Suspended { sd with after_block; right  } }
