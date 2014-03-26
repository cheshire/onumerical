(** Linear Expression type used for parametrizing expressions *)

open Core.Std

module Make
    (Var : module type of VarIntf) (* Variable type parametrization *)
    (Number : module type of NumberIntf) (* Number type parametrization *)
    :
sig

    (** Variable identifier type *)
    type var_t = Var.t

    (** Number type *)
    type number_t = Number.t

    (** Coefficient map type *)
    type coeffs_t = (var_t, number_t) Map.Poly.t

    (** Expression type definition *)
    type t = {
        coeffs: coeffs_t;
        constant: number_t
    }

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

    (** Build an expression from the associative list [var_t], [number_t], and a
     * constant, provided as a separate argument *)
    val of_assoc_list_and_const : (var_t * number_t) list -> number_t -> t

    (** Deconstructing expressions *)
    (** Coefficient associated with the given variable in a given expression *)
    val coeff : t -> var_t -> number_t

    (** Value of the constant associated with the given expression *)
    val const : t -> number_t

    (** Return coefficients associated with all variables *)
    val coeffs : t -> coeffs_t

    (** Substitute a variable with an expression *)
    val substitute : t -> var:var_t -> substitution:t -> t

    (** Converts a list of expression into one expression, where each variable
     * appearing in at least one the input expressions maps to a coefficient of
     * one. *)
    val vars_used : t list -> coeffs_t
end

