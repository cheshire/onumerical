(** Number type: anything comparable which supports basic arithmetic *)
type t with sexp, compare (* OK what does compare generate? *)

(** Constructors *)
val zero : t
val one : t
val of_int : int -> t
val infinity : t

(** Arithmetics *)
val (+/) : t -> t -> t
val (-/) : t -> t -> t
val ( */) : t -> t -> t
val ( //) : t -> t -> t

val abs : t -> t

(** Negation *)
val (~/) : t -> t

(** Comparison *)
val compare : t -> t -> int
val (<=/) : t -> t -> bool
val (>=/) : t -> t -> bool
val (=/) : t -> t -> bool
val (</) : t -> t -> bool
val (>/) : t -> t -> bool
val max : t -> t -> t
val min : t -> t -> t

(** Pretty-printing *)
val to_string : t -> string
