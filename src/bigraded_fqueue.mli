type (+'a,+'b) t
type ('a,'b) answer =
  | Minor of 'a * ('a,'b) t
  | Major of 'b * ('a,'b) t
  | Empty

val take_front: ('a,'b) t -> ('a,'b) answer
val take_back: ('a,'b) t -> ('a,'b) answer
val take_major_back:('a,'b) t -> ('a,'b) t * 'b option * 'a Sequence.t

val push_min: 'a -> ('a,'b) t -> ('a,'b) t
val push_maj: 'b -> ('a,'b) t -> ('a,'b) t

val empty: ('a,'b) t
val is_empty: ('a,'b) t -> bool
val secondary: ('a,'b) t -> bool

val fold:
  ([`Minor of 'a|`Major of 'b] -> 'acc -> 'acc) -> ('a,'b) t -> 'acc -> 'acc
