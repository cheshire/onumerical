(** Number type: anything comparable which supports basic arithmetic *)
type t with sexp

(** Constructors *)
val zero : t
val one : t
val of_int : int -> t

(** Arithmetics *)
val (+/) : t -> t -> t
val (-/) : t -> t -> t
val ( */) : t -> t -> t
val ( //) : t -> t -> t

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

(** Pretty-printing to a string of set width (extra spaces go to the left). *)
val to_string_with_padding : t -> no_chars:int -> string
