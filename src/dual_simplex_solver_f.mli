open Core.Std

module Make
    (Number : module type of Number_intf) (* Number type parametrization *)
: sig
    module Matrix : (module type of Matrix_f.Make(Number))
    module Vector : (module type of Vector_f.Make(Number))

    type tableau_t = Matrix.t

    (** Each row has an associated basis variable.
     *  Basis is the mapping row_no -> column_no, where column_no is the
     *  column associated with the corresponding basis variable.
     *  The mapping is implicitly defined by the input variable being the array
     *  index. *)
    type basis_t = int array

    type t = {
        tableau: tableau_t;
        basis: basis_t;
    }

    (** Solution type.
     *  We never get "unbounded", as in that case the dual will be
     *  unfeasible, and we won't get a chance to run the dual simplex algorithm
     *  in the first place. *)
    type solution_type_t = Unfeasible | Solution with sexp
    type solution_t = solution_type_t * t

    (** Get the constants column out of the tableau *)
    val constants : tableau_t -> Number.t array

    (** Get the objective row *)
    val cost_row : tableau_t -> Number.t array

    (** Get the objective value.
     *  NOTE: it will not be the optimal value if tableau wasn't pivoted. *)
    val obj_value : tableau_t -> Number.t

    (** Perform dual simplex pivoting on the tableau
     *  Precondition: dual is origin-feasible <=> all cost-row entries
     *  are negative.
     *  NOTE: if the pre-condition wasn't fullfilled, the function will crash
     *  with an assertion error. *)
    val dual_simplex : t -> solution_t

end
