(** A module for dealing with vectors *)
open Core.Std

module Make
    (Number : module type of NumberIntf) (* Number type parametrization *)
    :
sig

    (** Vector datatype *)
    type t = Number.t array

    (** Create a vector of a given size with a given initial value *)
    val create : int -> Number.t -> t

    (** Sum of all the elements inside the vector *)
    val sum : t -> Number.t

    (** Dot-product of two vectors *)
    val dot : t -> t -> Number.t

    (** Smallest element in the vector, fails if empty. *)
    val vmin_exn : t -> Number.t

    (** Largest element in the vector, fails if empty. *)
    val vmax_exn : t -> Number.t

    (** Arithmetic operations on two vectors *)
    val (++) : t -> t -> t
    val (//) : t -> t -> t
    val (--) : t -> t -> t

    (** Arithmetic operation on the vector and a scalar *)
    val (--.) : t -> Number.t -> t
    val (++.) : t -> Number.t -> t
    val ( **. ) : t -> Number.t -> t
    val (//.) : t -> Number.t -> t

    (** Pretty-print the vector to string *)
    val to_string : t -> string
end
