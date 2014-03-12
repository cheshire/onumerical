(** A module for dealing with vectors *)
open Core.Std

module Make
    (Number : module type of Number) (* Number type parametrization *)
    :
sig

    (** Individual element in the vector *)
    type element_t = Number.t

    (** Vector datatype *)
    type t = element_t array

    (** Create a vector of a given size with a given initial value *)
    val create : int -> element_t -> t

    (** Sum of all the elements inside the vector *)
    val sum : t -> element_t

    (** Dot-product of two vectors *)
    val dot : t -> t -> element_t

    (** Smallest element in the vector *)
    val vmin : t -> element_t

    (** Largest element in the vector *)
    val vmax : t -> element_t

    (** Position of the smallest element *)
    val vargmin : t -> int

    (** Position of the largest element *)
    val vargmax : t -> int

    (** Arithmetic operations on two vectors *)
    val (++) : t -> t -> t
    val (//) : t -> t -> t
    val (--) : t -> t -> t

    (** Arithmetic operation on the vector and a scalar *)
    val (--.) : t -> element_t -> t
    val (++.) : t -> element_t -> t
    val ( **. ) : t -> element_t -> t
    val (//.) : t -> element_t -> t

    (** Pretty-print the vector to string *)
    val to_string : t -> string
end
