(**
 * Linear expression type parametrized over the number type and the
 * variable type. *)

open Core.Std

type var_t
type number_t

(** Coefficient map type *)
type coeffs_t = (var_t, number_t) Map.Poly.t with sexp, compare

(** Expression type definition *)
type t = {
    coeffs: coeffs_t;
    constant: number_t;
} with sexp, compare

(** { Arithmetics on multiple expressions } *)
val (++) : t -> t -> t
val (--) : t -> t -> t

(** { Operations with constants } *)
val (++.) : t -> number_t -> t
val (--.) : t -> number_t -> t
val ( **.) : t -> number_t -> t
val (//.) : t -> number_t -> t

(** Unary operations *)
val (~~) : t -> t

(** { Factory methods } *)
val of_var : var_t -> t
val of_const : number_t -> t

(** Build an expression from the associative list [var_t], [number_t], and a
 *  constant, provided as a separate argument *)
val of_assoc_list_and_const : (var_t * number_t) list -> number_t -> t

(** Serialize the expression back to the associative list *)
val to_assoc_list_and_const : t -> ((var_t * number_t) list * number_t)

(** { Deconstructing expressions } *)

(** Coefficient associated with the given variable in a given expression *)
val coeff : t -> var_t -> number_t

(** Value of the constant associated with the given expression *)
val const : t -> number_t

(** Return coefficients associated with all variables *)
val coeffs : t -> coeffs_t

(** Substitute a variable with an expression *)
val substitute : t -> var:var_t -> substitution:t -> t

(** Converts a list of expression into one expression, where each variable
 *  appearing in at least one the input expressions maps to a coefficient of
 *  one. *)
val vars_used : t list -> coeffs_t

val to_string : t -> string
