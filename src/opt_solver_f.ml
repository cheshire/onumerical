open Core.Std

module Make
    (Var : module type of VarIntf) (* Variable type parametrization *)
    (Number : module type of NumberIntf) (* Number type parametrization *)

    =
struct
    module Expression = Expression_f.Make(Var)(Number)

    (* Optimization solver engine *)
    module SimplexSolver = Dual_simplex_solver_f.Make(Number)

    type expression_t = Expression.t
    type number_t = Number.t
    type var_t = Var.t

    type var_map_t = (var_t, number_t) Map.Poly.t

    type feasible_solution_t = {
        value:number_t;
        variable_assignment:var_map_t}

    type opt_solution_t =
        | Unbounded
        | Unfeasible
        | Solution of feasible_solution_t

    type objective_t =
        | Maximize of Expression.t
        | Minimize of Expression.t

    module InputConstraintType = struct
        type t = LessThanZero | GreaterThanZero | EqualZero
    end

    (* All constraints converted to [LessThanZero] and
     * the objective is the [Maximize] problem. *)
    type std_form_problem_t = {
        max_objective: Expression.t;
        ltz_constraints: expression_t list;
        vars: var_t list;
    }

    (* Change the objective to [Minimize] and all constraints to
     * [LessThanZero] *)
    let of_constraints_and_objective
            (constraints : (InputConstraintType.t * expression_t) list)
            (objective   : objective_t)
            : std_form_problem_t =
        let module E = Expression in
        let module I = InputConstraintType in
        {
            max_objective = (
                match objective with
                    | Minimize expr -> expr
                    | Maximize expr -> E.(~~expr));
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

    module DualVar = struct
        type constraint_idx_t = int with sexp, compare
        type t = constraint_idx_t with sexp, compare

        (** Creates a var with a given index *)
        let create_var_with_idx (i:constraint_idx_t) : t = i
    end
    module DualExpression = Expression_f.Make(DualVar)(Number)

    type dual_problem_t = {

        (* Minimization problem *)
        min_objective: DualExpression.t;

        (* Constrained associated with a variable from a primal problem.
         * Every constraint is greater-than-zero. *)
        gtz_constraints: (DualExpression.t * Var.t) list;

        (* Each variable is associated with a constraint (in the standard form)
         * from the primal problem.
         * Note: all variables are restricted *)
        vars: (DualVar.t * Expression.t) list
    }


    (* Convert the problem to the dual *)
    (** TODO: what do I need the dual for?.. *)
    let to_dual
            (opt_problem:std_form_problem_t)
            : dual_problem_t =
        let vars = List.mapi opt_problem.ltz_constraints
            ~f:(fun idx _ -> DualVar.create_var_with_idx idx) in
        {
            min_objective = (DualExpression.of_assoc_list_and_const
                (List.map2_exn vars opt_problem.ltz_constraints
                    ~f:(
                        fun var ltz_constraint ->
                            (var, (Expression.const ltz_constraint))
                    ))
                Number.zero);

            (* Generate a new constraint from each variable. *)
            gtz_constraints = (List.map opt_problem.vars
                ~f:(fun var ->

                    (* List of coefficients for a new constraint *)
                    let coeff_list = List.map opt_problem.ltz_constraints
                        ~f:(fun expr -> Expression.coeff expr var) in

                    (* The corresponding bound can be found in the objective
                     * function *)
                    let constr = DualExpression.of_assoc_list_and_const
                        (List.zip_exn vars coeff_list)
                        Number.(~/ (Expression.coeff
                            opt_problem.max_objective var)) in
                    (constr, var)
                ));

            vars = (List.map2_exn vars opt_problem.ltz_constraints
                ~f:(fun var constr -> (var, constr)));
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
        let constraints = List.map opt_problem.ltz_constraints
            ~f:(fun expr ->
                (expr_to_dense_list expr) @ [(Expression.const expr)]) in

        (* Adjoint the cost function row *)
        let tableau = constraints @ [
            expr_to_dense_list opt_problem.max_objective @ [Number.zero]
        ]  in

        let tableau_matrix = Array.of_list_map tableau ~f:(
            fun row -> Array.of_list row) in

        let basis = Array.init
            no_constraints ~f:(fun row_no -> row_no + no_variables) in

        {
            SimplexSolver.tableau=tableau_matrix;
            SimplexSolver.basis=basis;
        }

    (* Converts a pivoted tableau to a map of values for all variables. *)
    let tableau_to_var_value
            ({SimplexSolver.tableau; SimplexSolver.basis}:SimplexSolver.t)
            (vars : var_t list)
            : var_map_t =

        let constants_v = SimplexSolver.(constants tableau) in

        let basis_idx test_var_idx = Array.find basis
            ~f:(fun var_idx -> (var_idx = test_var_idx)) in

        (* TODO: hm can we get by with just an associate list then?... *)
        Map.Poly.of_alist_exn (
            List.mapi vars ~f:(fun var_idx var ->
                match basis_idx var_idx with
                    (* Variables not in the basis become 0 *)
                    | None -> (var, Number.zero)
                    (* Otherwise take the corresponding value from the constants
                     * row *)
                    | Some idx -> (var, constants_v.(idx))
            ))

    let solve (opt_problem:std_form_problem_t) : opt_solution_t =
        let module S = SimplexSolver in
        let tableau_problem = to_tableau opt_problem in
        match S.dual_simplex tableau_problem with
            | S.Unfeasible, pivoted_tableau -> Unfeasible
            | S.Solution, pivoted_tableau ->
                Solution {
                    variable_assignment=(tableau_to_var_value
                        pivoted_tableau
                        opt_problem.vars
                    );

                    (* Negative of the bottom right entry in the tableau *)
                    value=Number.(~/ (Array.nget
                            (Array.nget S.(tableau_problem.tableau) (-1))
                        (-1)));
                }

    (* TODO: integrate with the chemistry parser *)
end
