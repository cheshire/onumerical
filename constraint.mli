(**
 * Represents a collection of constraints.
 **)
open Core.Std

module Make
    (Var : module type of Var) (* Variable type parametrization *)
    (Number : module type of Number) (* Number type parametrization *)
    :
sig
    type t

    type var_t = Var.t
    type number_t = Number.t
    module Expression : Expression_intf.S
        with type var_t := var_t and type number_t := number_t

    (** Conjunction of multiple constraints *)
    val (&&.) : t -> t -> t

    (** Factory methods for creating constraints *)
    val (<=.) : Expression.t -> Expression.t -> t
    val (>=.) : Expression.t -> Expression.t -> t
    val (==.) : Expression.t -> Expression.t -> t
end
