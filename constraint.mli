(**
 * Represents a collection of constraints.
 **)
open Core.Std

(** Number type parametrization *)
module type NumberT = sig
    type t

    (** Constructors *)
    val zero : unit -> t
    val one : unit -> t
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
end

(** Variable type parametrization *)
module type VarT = sig
    (* TODO: is this the right syntax? *)
    type t with sexp, compare
end

(** Linear Expression type used for parametrizing expressions *)
module type ExpressionT = sig
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

(** Linear constraint signature. *)
module type S = sig
    type t

    type var_t
    type number_t
    module Expression : ExpressionT
        with type var_t := var_t and type number_t := number_t

    (** Conjunction of multiple constraints *)
    val (&&.) : t -> t -> t

    (** Factory methods for creating constraints *)
    val (<=.) : Expression.t -> Expression.t -> t
    val (>=.) : Expression.t -> Expression.t -> t
    val (==.) : Expression.t -> Expression.t -> t
end

module Make
    (Var : VarT)
    (Number : NumberT)
    : S with type var_t := Var.t and type number_t = Number.t

