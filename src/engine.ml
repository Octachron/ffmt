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
    Break of break_data | Lit of resolved_lit

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

  let is_empty = M.is_empty

  let take_exn pos m =
    let max, elt =  pos m in
    (elt, M.remove max m)

  let take pos m =
    if is_empty m then None
    else Some(take_exn pos m)

  let cardinal = M.cardinal
  let empty = M.empty
  let fold = M.fold

  type ('a,'b) graded =
    | Single of 'a t
    | Double of {first:'a t ; middle:'b; last:'a t}
    | More of { first:'a t; middle:('b * 'a t) t; penultimate:'b; last:'a t}

  type ('a,'b) answer =
    | Minor of 'a * ('a,'b) graded
    | Major of 'b * ('a,'b) graded
    | Empty

  let take_front = function
    | Single s ->
      begin match take front s with
        | None -> Empty
        | Some(x,r) -> Minor(x, Single r)
      end
    | More r ->
      begin match take front r.first with
        | None ->
          let (mj, first), middle = take_exn front r.middle in
          if is_empty middle then
            Major(mj,Double{first;middle = r.penultimate;last=r.last})
          else
            Major(mj, More { r with first; middle } )
        | Some (x,first) -> Minor(x, More { r with first } )
      end
    | Double r -> match take front r.first with
      | None -> Major(r.middle, Single r.last)
      | Some(x,first) ->
        Minor(x, Double { r with first })

  let take_back = function
    | Single s ->
      begin match take back s with
        | None -> Empty
        | Some(x,r) -> Minor(x, Single r)
      end
    | More r ->
      begin match take back r.last with
        | None ->
          let (mj, last), middle = take_exn back r.middle in
          if is_empty middle then
            Major(mj,Double{first=r.first ;middle = r.penultimate;last})
          else
            Major(mj, More { r with middle; last } )
        | Some (x,last) -> Minor(x, More { r with last } )
      end
    | Double r -> match take back r.last with
      | None -> Major(r.middle, Single r.first)
      | Some(x,last) ->
        Minor(x, Double { r with last })

  let take_major_back = function
    | Single s -> Single empty, None, s
    | Double r -> Single r.first, Some r.middle, r.last
    | More r ->
      let (p,last), middle = take_exn back r.middle in
      let rest =
        if is_empty middle then
          Double {first=r.first; middle = p; last }
        else
          More { r with middle; last; penultimate = p } in
      rest, Some r.penultimate, r.last

  let push_min (minor:'a): ('a,'b) graded -> ('a,'b) graded = function
    | Single l -> Single (push_back minor l)
    | Double r -> Double { r with last = push_back minor r.last }
    | More r -> More { r with last = push_back minor r.last }

  let push_maj major = function
    | Single first -> Double { first; middle=major; last=empty}
    | Double r ->
      More { first= r.first; middle = push_back (r.middle, r.last) empty;
             penultimate = major; last = empty }
    | More r ->
      More { r with middle = push_back (r.penultimate, r.last) empty;
             penultimate = major; last = empty }

  let fold_minor f x acc =
    fold (fun _key x -> f (`Minor x) ) x acc
  let fold f gr acc = match gr with
    | Single l -> fold_minor f  l acc
    | Double r ->
      acc |> fold_minor f r.first
      |> f (`Major r.middle)
      |> fold_minor f r.last
    | More r ->
      acc |> fold_minor f r.first
      |> fold
        (fun _key (x,s) acc -> acc |> f (`Major x) |> fold_minor f s )
        r.middle
      |> f (`Major r.penultimate)
      |> fold_minor f r.last

  let empty = Single empty
  let is_empty = function
    | Single s -> is_empty s
    | _ -> false

end

type context =
  { indent: int;
    left_context: open_box_on_the_left list;
    (** on the left, we have those inactive boxes *)
  }

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
  after: (suspended_lit, box) S.graded;
  (** we already have computed these token after the block *)
  right: int (** and we are currently at this position < margin *);
}


type status =
  | Direct
  (** We are directly writing to the logical device *)

  | Suspended of suspended
  (**We are blocked at an ambiguous break, waiting for the decision on
     how to interpret it *)

let secondary_box = (function S.Single _ -> false | _ -> true)
(*
let only_boxes str =
  S.M.for_all (fun _ -> function Open_box _ -> true | _ -> false) str
*)

type 'a t = {
  context: open_box_on_the_left list;
  position: 'a position;
  status:status;
}

let pos (ppf:'a t) = ppf.position
let make context status position =
  { context;status; position }
let update ppf position = make ppf.context ppf.status position

module G = Geometry
module I = Geometry.Indentation


let box_indent: box -> int = function
  | B n | HV n | HoV n | V n-> n
  | H -> 0

let phyline max_indent more pos =
  let indent = min (pos.indent + more + box_indent pos.kind) max_indent in
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
(*
let space n d ppf =
  { ppf with status = Direct (physpace n d ppf.logical.phy) }
*)

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
    debug "str %d ⇒ %d" current (current + phy#len (Raw.all s));
    suspended_lit_len phy indent (current + phy#len (Raw.all s)) q
  | Newline more :: q->
      suspended_lit_len phy (indent + more) (indent + more) q
  | Space sp :: q-> suspended_lit_len phy indent (current + sp) q
  | Box box :: q ->
    suspended_lit_len phy indent (suspended_lit_len phy current current box) q
  | [] -> debug "lit: ⇒ %d" current; current

let is_vertical (x: box) = match x with V _ -> true | _ -> false

let suspended_len_tok phy tok direct = match tok with
  | `Minor Break b -> begin
      match direct.kind with
      | V more -> { direct with current = direct.indent + more + b.indent }
      | _ ->
        debug "break %d ⇒ %d" direct.current (direct.current + b.space);
        { direct with current = direct.current + b.space }
    end
  | `Major v -> { direct with indent = direct.current; kind = v }
  | `Minor Lit l ->
    let current =
        suspended_lit_len phy direct.indent direct.current [l] in
    { direct with current}

let suspended_len phy stream direct =
  let all =
    S.fold (suspended_len_tok phy) stream direct in
  debug "reevaluted size: %d ⇒ %d "direct.current all.current;
  all.current

let max_indent geom = geom.G.max_indent

let commit_resolved_literal max_indent lits c =
  let rec elt c =  function
    | Str s -> phystring s c
    | Newline more -> phyline max_indent more c
    | Space sp -> physpace sp c
    | Box b ->
      let inside =
        List.fold_left elt { c with indent = c.current; kind = B 0 } b in
      { c with current = inside.current; phy = inside.phy } in
  List.fold_left elt c lits

let rec advance_to_next_ambiguity geom context stream c =
  match S.take_front stream with
  | Major (b, rest) ->
    debug "advance open box %a" pp_box b;
    (* When we meet a box without any ambiguity on its position,
       we put the current box on the left stack, and focus on this
       new box *)
    let indent = c.current in
    let context: open_box_on_the_left list =
      { kind = c.kind; indent = c.indent } :: context in
    advance_to_next_ambiguity geom context rest
      { c with indent; kind = b }
  | Minor(Break b, rest) ->
    let status =
      Suspended {
                  break = b;
                  after = rest;
                  right = suspended_len c.phy stream c;
                } in
    debug "Advance up to new break, stop at %d" c.current;
    make context status c
  | Minor(Lit s, rest) ->
    debug "advance lit";
       c
    |> commit_resolved_literal (max_indent geom) [s]
    |> advance_to_next_ambiguity geom context rest
  | Empty ->
    debug "empty stream: stop advance at %d" c.current;
    make context Direct c

let rec advance_to_next_box max_indent stream c =
  match S.take_front stream with
  | Major(b, stream) -> { c with kind = b }
  | Minor(Break b, stream) ->
    c |> phyline max_indent b.indent
    |> advance_to_next_box max_indent stream
  | Minor(Lit s, stream) ->
    c |> commit_resolved_literal max_indent [s]
      |> advance_to_next_box max_indent stream
  | Empty -> c

let advance_to_next_ambiguity geom ctx stream c =
  debug "Advance up to new break, start at %d" c.current;
  advance_to_next_ambiguity geom ctx stream c

let actualize_break geom context sd c =
  let c = newline geom sd.break.indent c in
  match c.kind with
  | HV n ->
    { c with kind = V n }
    |> advance_to_next_box (max_indent geom) sd.after
    |> make context Direct
  | b ->
    advance_to_next_ambiguity geom context sd.after c


let rec string geom s ppf =
  match ppf.status with
  | Direct ->
    direct_string ppf.context ppf.position s
  | Suspended sd ->
    let right = sd.right + ppf.position.phy#len (Raw.all s) in
    debug "suspended printing «%s» %d|%d ⇒ %d > %d ? "
      s ppf.position.indent sd.right right geom.G.margin;
    if right > geom.G.margin then
         ppf.position
      |> actualize_break geom ppf.context sd
      |> string geom s
    else
      ( debug "suspending «%s»" s;
        let after = S.push_min (Lit (Str s)) sd.after in
        { ppf with status = Suspended { sd with after; right } }
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

(*
let last_box (context: open_box_on_the_left list) gr =
  match S.take_major_back gr with
  | Some b, c -> b, c
  | None, rest ->
    match context with
    | [] -> (H: Format.box), rest
    | a :: ctx -> a.kind, rest*)

let resolve_box bx sp stream =
  let rec resolve_box bx sp stream (lits: _ list) =
    match S.(take back) stream with
    | Some(Lit s, stream) -> resolve_box bx sp stream (s :: lits)
    | Some(Break b, stream) ->
      let lit = translate_break b bx in
      resolve_box bx sp stream (lit :: lits)
    | None ->
      debug "end coalesce, right:%d" sp.right;
      Box lits
  in
  debug "resolving %a box" pp_box bx;
  resolve_box bx sp stream []

let coalesce geom context sp c =
  match S.take_major_back sp.after with
  | rest, Some b, last_box_content ->
    let lit = resolve_box b sp last_box_content in
    let after = S.push_min (Lit lit) rest in
    make context (Suspended {sp with after}) c
  | rest, None, last_box_content ->
    let c, context = reactivate c context in
    let lit = resolve_box c.kind sp last_box_content in
    c
    |> commit_resolved_literal (max_indent geom) [lit]
    |> make context Direct

let close_box geom ppf =
  match ppf.status with
  | Direct ->
    debug "close box %a" pp_box ppf.position.kind;
    let position, context = reactivate ppf.position ppf.context in
    { ppf with position; context }
  | Suspended sp ->
    debug "coalesce box %a" pp_box ppf.position.kind;
    coalesce geom ppf.context sp ppf.position

let eager_indent: box -> _ = function
  | B _ -> true
  | _ -> false

let rec break geom br (ppf: _ t) =
  debug "break {space=%d;indent=%d}" br.space br.indent;
  let c = ppf.position in
  match ppf.status with
  | Direct -> begin match c.kind with
      | H -> update ppf (physpace br.space c)
      | V more -> update ppf (newline geom br.indent c)
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
                          after = S.empty;
                          right = c.current + br.space;
                        } in
            { ppf with status } end
  | Suspended sd ->
    let right = sd.right + br.space in
    let after = S.push_min (Break br) sd.after  in
    debug "waiting for break resolution at %d" c.current;
    let b = c.kind in
    if right > geom.G.margin
    || (eager_indent b && c.indent + br.indent + box_indent b < c.last_indent)
    then
      c
      |> actualize_break geom ppf.context sd
      |> break geom br
    else match c.kind with
    | HoV _ | B _ when not (secondary_box after) ->
         c
      |> physpace sd.break.space
      |> advance_to_next_ambiguity geom ppf.context after
    | _ ->
      let right = suspended_len c.phy after c in
      debug "break in not hov ⇒ %d" right;
      { ppf with status = Suspended { sd with after; right  } }


let rec full_break geom br (ppf:_ t) =
  let pos = ppf.position in
  match ppf.status with
  | Direct ->
    pos |> newline geom br |> update ppf
  | Suspended sd ->
    match pos.kind with
    | HoV _ | B _ when not (secondary_box sd.after) ->
      pos |> physpace sd.break.space
      |> advance_to_next_ambiguity geom ppf.context sd.after
      |> full_break geom br
    | _ ->
      debug "breaking with a full break at %d" pos.current;
      pos |> actualize_break geom ppf.context sd |> full_break geom br

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
      { ppf with context; position }
  | Suspended s ->
    if s.right > geom.G.box_margin then
      pos |> actualize_break geom ppf.context s |> open_box geom b
    else
      let status = Suspended
          { s with after = S.push_maj b s.after } in
      { ppf with status }

type ('a,'b) prim = Geometry.t -> 'a -> 'b t -> 'b t
let start phy =
  { status = Direct;
    position= {current=0;indent=0; kind=H; last_indent=0; phy };
    context = [] }

let close_box geom () = close_box geom

let flush (ppf: _ t) = ppf.position.phy#flush
(*
type 'a prim = Geometry.t -> Raw.t -> 'a -> t -> t
let lift f geom phy = f {geom;phy}

let string = lift string
let break = lift  break
let full_break = lift full_break
let close_box geom phy ()  =  close_box {geom;phy}
let open_box = lift open_box
*)
