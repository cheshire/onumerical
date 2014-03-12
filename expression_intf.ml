(** Linear Expression type used for parametrizing expressions *)
module type S = sig
    type t

    type var_t
    type number_t

    (** Arithmetics on multiple expressions *)
    val (++) : t -> t -> t
    val (--) : t -> t -> t

    (** Operations with constants *)
    val (++.) : t -> number_t -> t
    val (--.) : t -> number_t -> t
    val ( **.) : t -> number_t -> t
    val (//.) : t -> number_t -> t

    (** Unary operations *)
    val (~~) : t -> t

    (** Factory methods *)
    val of_var : var_t -> t
    val of_const : number_t -> t

    (** Deconstructing expressions *)
    (** Coefficient associated with the given variable in a given expression *)
    val coeff : t -> var_t -> number_t

    (** Value of the constant associated with the given expression *)
    val const : t -> number_t
end

