open Core.Std

module Make
    (Var : module type of Var_intf) (* Variable type parametrization *)
    (Number : module type of Number_intf) (* Number type parametrization *)

    =
struct
    module Expression = Expression_f.Make(Var)(Number)

    (* Optimization solver engine *)
    module SimplexSolver = Dual_simplex_solver_f.Make(Number)
    module Vector = Vector_f.Make(Number)
    module Matrix = Matrix_f.Make(Number)

    type var_map_t = (Var.t * Number.t) list with sexp

    (** Dual variable assignment: expressions associated with numbers *)
    type dual_map_t = (Expression.t * Number.t) list with sexp

    type feasible_solution_t = {
        primal_var_assignment: var_map_t;
        dual_var_assignment: dual_map_t;
        value: Number.t;
    } with sexp

    type opt_solution_t =
        | Unbounded
        | Unfeasible
        | Solution of feasible_solution_t with sexp

    type objective_t =
        | Maximize of Expression.t
        | Minimize of Expression.t

    module InputConstraintType = struct
        type t = LessThanZero | GreaterThanZero | EqualZero with sexp
    end

    type constraint_t = InputConstraintType.t * Expression.t with sexp
    type input_constraints_t = constraint_t list with sexp

    (* All constraints converted to [LessThanZero] and
     * the objective is the [Maximize] problem. *)
    type std_form_problem_t = {
        max_objective: Expression.t;
        ltz_constraints: Expression.t list;
        vars: Var.t list;
    } with sexp

    (* Change the objective to [Minimize] and all constraints to
     * [LessThanZero] *)
    let of_constraints_and_objective
            (constraints : constraint_t list)
            (objective   : objective_t)
            : std_form_problem_t =
        let module E = Expression in
        let module I = InputConstraintType in
        {
            max_objective = (
                match objective with
                    | Minimize expr -> E.(~~expr)
                    | Maximize expr -> expr
            );
            ltz_constraints = List.fold constraints ~init:[] ~f:(
                fun output_constraints constr ->
                    output_constraints @ match constr with
                        | (I.EqualZero, expr) ->
                                (* e == 0 is converted to
                                 * [e <= 0; -e <= 0] *)
                                [expr; E.(~~expr)]
                        | (I.GreaterThanZero, expr) ->
                                (* e >= 0 is converted to
                                 * [-e <= 0] *)
                                [E.(~~expr)]
                        | (I.LessThanZero, expr) ->
                                (* Less-than-zero constraints are left as-is. *)
                                [expr]
            );
            vars = Map.Poly.keys (E.vars_used
                (List.map constraints ~f:(fun (_, expr) -> expr)
            ));
        }

    (* Convert the sparse representation to the 2D array. *)
    let to_tableau (opt_problem:std_form_problem_t)
            : SimplexSolver.t
            =
        let no_variables = List.length opt_problem.vars in
        let no_constraints = List.length opt_problem.ltz_constraints in

        let expr_to_dense_list expr = 
            List.map opt_problem.vars ~f:(
                fun var -> Expression.coeff expr var) in

        (* Tableau in the nested list form *)
        let constraints = List.mapi opt_problem.ltz_constraints
            ~f:(fun idx expr ->
                (expr_to_dense_list expr)

                (* And the corresponding row of the identity matrix *)
                @ (List.init no_constraints ~f:(fun ident_idx ->
                    if ident_idx = idx then
                        Number.one
                    else
                        Number.zero
                ))

                (* We need the negation of the constant, as it is moved to the
                 * left-hand side of the equation. *)
                @ [Number.(~/ (Expression.const expr))]) in

        (* Adjoint the cost function row *)
        let tableau = constraints @ [
            (expr_to_dense_list opt_problem.max_objective)

            (* Zeros below the identity matrix. *)
            @ (List.init no_constraints ~f:(fun _ -> Number.zero))
            @ [Number.zero]
        ]  in

        (* Tableau without the identity sub-matrix *)
        let tableau_matrix = Array.of_list_map tableau ~f:(
            fun row -> Array.of_list row) in
        Log.debugf "Tableau after conversion = \n%s"
            (Matrix.to_string tableau_matrix);

        let basis = Array.init
            no_constraints ~f:(fun row_no -> row_no + no_variables) in

        {
            SimplexSolver.tableau=tableau_matrix;
            SimplexSolver.basis=basis;
        }

    (* Converts a pivoted tableau (which has to represent a feasible solution)
     * to a map of values for all variables. *)
    let tableau_to_var_value
            (tableau_solution : SimplexSolver.t)
            (vars : Var.t list)
            (lte_constraints : Expression.t list)
            : feasible_solution_t  =

        let module S = SimplexSolver in
        Log.debugf "pivoted tableau = \n%s\nbasis=\n%s"
            S.(Matrix.to_string tableau_solution.tableau)
            (Sexp.to_string (sexp_of_array sexp_of_int
                S.(tableau_solution.basis)));
        let tableau = S.(tableau_solution.tableau) in
        let basis = S.(tableau_solution.basis) in
        let no_constraints = Array.length basis in
        let no_vars = List.length vars in

        let constants_v = S.constants tableau in
        let cost_row_v = S.cost_row tableau in

        (* Get the row for which the [test_var_idx] is the basic variable,
         * or [None]. *)
        let get_basis_idx test_var_idx = Array.findi basis
            ~f:(fun _ var_idx -> (var_idx = test_var_idx)) in

        let primal_var_assignment = List.mapi vars ~f:(fun var_idx var ->
            match (get_basis_idx var_idx) with

                (* Variables not in the basis become 0. *)
                | None -> (var, Number.zero)

                (* Otherwise take the corresponding value from the constants
                 * row. *)
                | Some (constraint_idx, _) -> (
                    var,
                    constants_v.(constraint_idx)
                )
        ) in
        (* Dual var assignment: negative values of the cost row associated with
         * the auxiliary variables. *)
        Log.debugf "no_constraints = %d, cost_row = %s"
            no_constraints
            (Vector.to_string cost_row_v);
        let dual_row = Vector.(~~ (cost_row_v <|> (no_vars, 0))) in
        Log.debugf "Constraints = \n%s"
            (String.concat ~sep:"\n"
                (List.map ~f:Expression.to_string lte_constraints));
        Log.debugf "Dual Row = \n%s"
            (Vector.to_string dual_row);
        let dual_var_assignment =
            List.map2_exn
                lte_constraints
                (Array.to_list dual_row)
                ~f:(fun expr dual_row -> (expr, dual_row))
        in
        {
            primal_var_assignment=primal_var_assignment;
            dual_var_assignment=dual_var_assignment;
            value=S.obj_value tableau;
        }

    let solve (opt_problem:std_form_problem_t) : opt_solution_t =
        let module S = SimplexSolver in
        let tableau_problem = to_tableau opt_problem in
        match S.dual_simplex tableau_problem with
            | S.Unfeasible, _ -> Unfeasible
            | S.Solution, pivoted_tableau ->
                Solution
                    (tableau_to_var_value
                        pivoted_tableau
                        opt_problem.vars
                        opt_problem.ltz_constraints
                    )
end
