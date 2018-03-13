module Q = CCFQueue

type ('a,'b) t =
  | Single of 'a Q.t
  | Double of {first:'a Q.t ; middle:'b; last:'a Q.t}
  | More of { first:'a Q.t;
              middle:('b * 'a Q.t) Q.t;
              penultimate:'b;
              last:'a Q.t}

type ('a,'b) answer =
  | Minor of 'a * ('a,'b) t
  | Major of 'b * ('a,'b) t
  | Empty

let take_front = function
  | Single s ->
    begin match Q.take_front s with
      | None -> Empty
      | Some(x,r) -> Minor(x, Single r)
    end
  | More r ->
    begin match Q.take_front r.first with
      | None ->
        let (mj, first), middle = Q.take_front_exn r.middle in
        if Q.is_empty middle then
          Major(mj,Double{first;middle = r.penultimate;last=r.last})
        else
          Major(mj, More { r with first; middle } )
      | Some (x,first) -> Minor(x, More { r with first } )
    end
  | Double r -> match Q.take_front r.first with
    | None -> Major(r.middle, Single r.last)
    | Some(x,first) ->
      Minor(x, Double { r with first })

let take_back = function
  | Single s ->
    begin match Q.take_back s with
      | None -> Empty
      | Some(r, x) -> Minor(x, Single r)
    end
  | More r ->
    begin match Q.take_back r.last with
      | None ->
        let middle, (mj, last) = Q.take_back_exn r.middle in
        if Q.is_empty middle then
          Major(mj,Double{first=r.first ;middle = r.penultimate;last})
        else
          Major(mj, More { r with middle; last } )
      | Some (last, x) -> Minor(x, More { r with last } )
    end
  | Double r -> match Q.take_back r.last with
    | None -> Major(r.middle, Single r.first)
    | Some(last,x) ->
      Minor(x, Double { r with last })

let take_major_back = function
  | Single s -> Single Q.empty, None, Q.to_seq s
  | Double r -> Single r.first, Some r.middle, Q.to_seq r.last
  | More r ->
    let middle, (p,last) = Q.take_back_exn r.middle in
    let rest =
      if Q.is_empty middle then
        Double {first=r.first; middle = p; last }
      else
        More { r with middle; last; penultimate = p } in
    rest, Some r.penultimate, Q.to_seq r.last

let push_min (minor:'a): ('a,'b) t -> ('a,'b) t = function
  | Single l -> Single (Q.snoc l minor)
  | Double r -> Double { r with last = Q.snoc r.last minor }
  | More r -> More { r with last =  Q.snoc r.last minor }

let push_maj major = function
  | Single first -> Double { first; middle=major; last=Q.empty}
  | Double r ->
    More { first= r.first; middle = Q.singleton (r.middle, r.last);
           penultimate = major; last = Q.empty }
  | More r ->
    More { r with middle = Q.singleton (r.penultimate, r.last) ;
                  penultimate = major; last = Q.empty }

let fold_minor f x acc =
  Q.fold (fun acc x -> f x acc ) acc x
let fold (type b) (f: _ -> b -> b) (g: _ -> b -> b) gr acc = match gr with
  | Single l -> fold_minor f l acc
  | Double r ->
    acc |> fold_minor f r.first
    |> g r.middle
    |> fold_minor f r.last
  | More r ->
    acc |> fold_minor f r.first
    |> (fun acc -> Q.fold
           (fun acc (x,s) -> acc |> g x |> fold_minor f s )
           acc
           r.middle
       )
    |> g r.penultimate
    |> fold_minor f r.last

let empty = Single Q.empty
let is_empty = function
  | Single s -> Q.is_empty s
  | _ -> false



let secondary = (function Single _ -> false | _ -> true)
