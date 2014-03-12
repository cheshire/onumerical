(** so this must be a bit different, since the set up is functorized now, so
 * we'll have to duplicate everything many times (sigh) *)
module type S = sig
    type t

    type solution_t

    type costant_t
    type var_t
    type constraint_t

    (** Create an optimization problem from a constraint system *)
    val create : constraint_t -> t

    val solve : t -> solution_t

    (** De-constructing the solution *)
    val value : solution_t -> constant_t
    val var_value : solution_t -> var_t -> constant_t
end

module type NumberT = module type of Number
module type VarT = module type of Var

(* Okay now where do I put the constraint? "Inside" the Simplex module? That
 * would be a bit silly, wouldn't it?.. *)
(* If I just leave it in the functor, it would be possible to parametrize
 * simplex with a different number than the one the Constraint module is
 * parametrized with =( *)
module Make
    (Number : NumberT)
    (Var : VarT)
 = sig
    module type Constraint = Constraint.S.Make NumberT VarT

    include S
        with type constant_t := NumberT.t
         and type var_t := VarT.t
         and type constraint_t := ConstraintT
end
