open Core.Std

(** Number type: anything comparable which supports basic arithmetic *)
type t with compare

(** Constructors *)
val zero : t (* TODO: function or a constant? *)
val one : t
val infinity : t
val of_int : int -> t

(** Arithmetics *)
val (+/) : t -> t -> t
val (-/) : t -> t -> t
val ( */) : t -> t -> t
val ( //) : t -> t -> t

(** Negation *)
val (~/) : t -> t

(** Pretty-printing *)
val to_string : t -> string

(** Pretty-printing to a string of set width (extra spaces go to the left). *)
val to_string_with_padding : t -> no_chars:int -> string
