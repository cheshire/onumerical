(**
 * Converting chemical equations from strings to datastructures and back
 *)

open Core.Std

(** Serializable representation for the chemical equation 
 *  (with no coefficients) *)
type t = {lhs: formula_t; rhs: formula_t}
and formula_t = molecule_t list
and molecule_t = (atom_t  * coeff_t) list
and coeff_t = int
and atom_t = string with sexp

(** Submodule for the equation with coefficients attached to molecules *)
module Coeff_equation : sig

    (** Serializable representation for the chemical equation with
     *  coefficients *)
    type coeff_t = {lhs_c: coeff_formula_t; rhs_c: coeff_formula_t}
    and coeff_formula_t = (int * molecule_t) list with sexp

    val to_string : coeff_t -> string
end

(* Convert the equation to string, return [None] if the input is incorrect *)
val of_string : string -> t option
val to_string : t -> string

module Converter : sig

   (** Add the coefficients to the equation: convert the equation to the linear
    *  programming problem, call the dual simplex method, obtain the results and
    *  convert the results back to the coefficients!
    * NOTE: main entry point! *)
    val add_coeffs : t -> Coeff_equation.coeff_t
end
