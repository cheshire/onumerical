(** Linear Expression type used for parametrizing expressions *)

open Core.Std

module Make
    (Var : module type of Var_intf) (* Variable type parametrization *)
    (Number : module type of Number_intf) (* Number type parametrization *)
    :
sig

    (** Coefficient map type *)
    type coeffs_t = (Var.t, Number.t) Map.Poly.t with sexp

    (** Expression type definition *)
    type t = {
        coeffs: coeffs_t;
        constant: Number.t
    } with sexp

    (** Arithmetics on multiple expressions *)
    val (++) : t -> t -> t
    val (--) : t -> t -> t

    (** Operations with constants *)
    val (++.) : t -> Number.t -> t
    val (--.) : t -> Number.t -> t
    val ( **.) : t -> Number.t -> t
    val (//.) : t -> Number.t -> t

    (** Unary operations *)
    val (~~) : t -> t

    (** Factory methods *)
    val of_var : Var.t -> t
    val of_const : Number.t -> t

    (** Build an expression from the associative list [Var.t], [Number.t], and a
     * constant, provided as a separate argument *)
    val of_assoc_list_and_const : (Var.t * Number.t) list -> Number.t -> t

    (** Deconstructing expressions *)
    (** Coefficient associated with the given variable in a given expression *)
    val coeff : t -> Var.t -> Number.t

    (** Value of the constant associated with the given expression *)
    val const : t -> Number.t

    (** Return coefficients associated with all variables *)
    val coeffs : t -> coeffs_t

    (** Substitute a variable with an expression *)
    val substitute : t -> var:Var.t -> substitution:t -> t

    (** Converts a list of expression into one expression, where each variable
     * appearing in at least one the input expressions maps to a coefficient of
     * one. *)
    val vars_used : t list -> coeffs_t

    val to_string : t -> string
end

