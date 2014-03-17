(** Performs optimization on a constraint system and an objective function *)

open Core.Std

module Make
    (Var : module type of VarIntf) (* Variable type parametrization *)
    (Number : module type of NumberIntf) (* Number type parametrization *)
    :
sig
    module Expression : module type of Expression_f.Make(Var)(Number)

    type expression_t = Expression.t
    type number_t = Number.t
    type var_t = Var.t

    (** Variable assignment type *)
    type var_map_t = (var_t, number_t) Map.Poly.t

    (** Objective function definition *)
    type objective_t =
        | Maximize of Expression.t
        | Minimize of Expression.t

    (** Optimization problem type *)
    type opt_problem_t

    (** Feasible solution description *)
    type feasible_solution_t = {
        value:number_t; variable_assignment:var_map_t}

    (** Solution of an optimization problem *)
    type opt_solution_t =
        | Unbounded
        | Unfeasible
        | Solution of feasible_solution_t

    (** Input constraint type definition *)
    module InputConstraintType : sig
        type t = LessThanZero | GreaterThanZero | EqualZero
    end

    (** Creating an optimization problem from a constraints system and an
     * objective function. *)
    val of_constraints_and_objective :
        (InputConstraintType.t * expression_t) list
            -> expression_t -> opt_problem_t

    (** Solve an optimization problem *)
    val solve : opt_problem_t -> opt_solution_t
end
