(** Performs optimization on a constraint system and an objective function. *)

open Core.Std

module Make
    (Var : module type of Var_intf) (* Variable type parametrization *)
    (Number : module type of Number_intf) (* Number type parametrization *)
    :
sig
    module Expression : module type of Expression_f.Make(Var)(Number)

    (** Variable assignment type *)
    type var_map_t = (Var.t * Number.t) list with sexp

    (** Dual variable assignment: expressions (each expression corresponds to 
     *  less-than-zero constraint in a normalized form)
     *  associated with numbers. *)
    type dual_map_t = (Expression.t * Number.t) list with sexp

    (** Objective function definition *)
    type objective_t =
        | Maximize of Expression.t
        | Minimize of Expression.t


    (** Optimization problem type *)
    type std_form_problem_t with sexp

    (** If a problem is solved, both primal and dual solutions are returned. *)
    type feasible_solution_t = {
        primal_var_assignment: var_map_t;
        dual_var_assignment: dual_map_t;
        value: Number.t;
    } with sexp

    (** Solution of an optimization problem *)
    type opt_solution_t =
        | Unbounded
        | Unfeasible
        | Solution of feasible_solution_t with sexp

    (** Input constraint type definition *)
    module InputConstraintType : sig
        type t = LessThanZero | GreaterThanZero | EqualZero with sexp
    end

    (** Input constraint type *)
    type constraint_t = InputConstraintType.t * Expression.t with sexp
    type input_constraints_t = constraint_t list with sexp

    (** Creating an optimization problem from a constraints system and an
     * objective function. *)
    val of_constraints_and_objective :
        constraint_t list -> objective_t -> std_form_problem_t

    (** Solve an optimization problem *)
    val solve : std_form_problem_t -> opt_solution_t
end
