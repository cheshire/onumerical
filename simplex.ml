open Core.Std

module Make
    (Var : module type of VarIntf) (* Variable type parametrization *)
    (Number : module type of NumberIntf) (* Number type parametrization *)

    =
struct
    module Expression = Expression_f.Make(Var)(Number)

    type expression_t = Expression.t
    type number_t = Number.t
    type var_t = Var.t

    module Matrix = Matrix_f.Make(Number)
    module Vector = Vector_f.Make(Number)

    type var_map_t = (var_t, number_t) Map.Poly.t

    type feasible_solution_t = {
        value:number_t;
        variable_assignment:var_map_t}

    (** Simplex optimization solver. *)
    module SimplexSolver = struct
        open Vector

        type tableau_t = Matrix.t

        (* Each row has an associated basis variable.
         * Basis is the mapping row_no -> column_no, where column_no is the
         * column associated with the corresponding basis variable. *)
        type basis_t = int array

        type t = {tableau:tableau_t; basis:basis_t}
        (* so where was i?... right. choosing the best datastructure for the
         * functional code. what kind of performance penalty do i get by using
         * map instead of a hashmap? I mean C++ users do use map alll over the
         * place and don't seem to be especially troubled by it (and they get
         * order for free! *)

        (** Change the basis after the pivot. **)
        let change_basis (basis:basis_t) ~row ~column =
            basis.(row) <- column

        (* Get the constants column out of the tableau *)
        let constants (tableau:tableau_t) =
            (* Number of coefficients in the equations represented in the simplex
             * tableau. **)
            let no_coeffs = ((Matrix.width tableau) - 1) in

            (* TODO: okay this slicing operator sucks. Let's just rewrite it
             * using functions, it's too confusing otherwise *)
            (Matrix.column_as_vector tableau no_coeffs) <|> (0, (-1))

        (* Perform a single pivot step *)
        let pivot_step tableau basis cost_row =
            let constants_v = constants tableau in
            let cost_row = cost_row tableau in

            (* OK instead of the smallest one how about we choose the first
             * negative one?.. *)
            let pivot_row_idx, _ = Array.findi_exn
                constants_v ~f:(fun _ el -> el < Number.zero) in

            (* Pivot row without the last element *)
            let pivot_row = tableau.(pivot_row_idx) <|> (0, (-1)) in

            (* Smallest ratio of the negative entries in the pivot row with the
             * corresponding cost row entries. NOTE: Infinity is used as a
             * guard *)
            Log.debugf "Pivot row = %s" (to_string pivot_row);
            Log.debugf "Cost row = %s" (to_string cost_row);
            let pivot_column_idx, _ = Array.foldi pivot_row
                ~init:(-1, Number.infinity)
                ~f:(fun idx current_min coeff ->
                    let (_, smallest_ratio) = current_min in
                    if coeff < Number.zero then (

                        (* error: idx out of bounds **)
                        let ratio = Number.(cost_row.(idx) // coeff) in

                        (* TODO: what kind of comparison function is used? I
                         * probably should look into that a bit more *)
                        if ratio < smallest_ratio then (idx, ratio) else
                            current_min
                    ) else current_min
                ) in
            Log.debugf "Pivot row idx = %d; Pivot column idx = %d"
                pivot_row_idx pivot_column_idx;
            assert (pivot_row_idx >= 0);
            assert (pivot_column_idx >= 0);

            change_basis basis ~row:pivot_row_idx ~column:pivot_column_idx;

            Matrix.(
                pivot tableau (Row pivot_row_idx) (Column pivot_column_idx)
            )

        let dual_simplex ({tableau; basis}:t) =

            (* Constants: column of constants, sans the cost row coefficient. **)
            let cost_row tableau = ((Array.nget tableau (-1)) <|> (0, (-1))) in

            let is_primal_feasible tableau = (vmin (constants tableau)) >=
                Number.zero in
            let is_dual_feasible tableau = (vmax (cost_row tableau)) <=
                Number.zero in
            assert (is_dual_feasible tableau);

            (** Inner tail-recursive function. **)
            let rec perform_dual_simplex (tableau:tableau_t) : tableau_t =
                Log.debug (lazy
                    (sprintf "tableau: \n%s" (Matrix.to_string tableau)));
                match is_primal_feasible tableau with
                    | true -> tableau
                    | false -> perform_dual_simplex
                        (pivot_step tableau basis cost_row) in

            {tableau=perform_dual_simplex tableau; basis=basis}
    end

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
    let to_std_form
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

    type std_form_in_tableau_t = {
        tableau: Number.t array array;
        vars: Var.t list;
        basis: int array;
    }

    (* Convert the sparse representation to the 2D array. *)
    let to_tableau (opt_problem:std_form_problem_t)
            : std_form_in_tableau_t
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
            tableau=tableau_matrix;
            vars=opt_problem.vars;
            basis=basis;
        }

    (* TODO: so what exactly is it? Dense report on all variables? *)
    type var_assignment_t = Number.t list

    (* Converts a pivoted tableau to a vector of values for all variables *)
    let tableau_to_var_value 
            (pivoted_opt_problem:std_form_in_tableau_t) : var_assignment_t =

        let constants_v = SimplexSolver.constants pivoted_opt_problem.tableau in

        let basis_idx test_var_idx = Array.find pivoted_opt_problem.basis
            ~f:(fun var_idx -> (var_idx = test_var_idx)) in

        List.mapi pivoted_opt_problem.vars ~f:(fun var_idx _ ->
            match basis_idx var_idx with
                (* Variables not in the basis become 0 *)
                | None -> Number.zero
                (* Otherwise take the corresponding value from the constants
                 * row *)
                | Some idx -> constants_v.(idx)
        )

    (* TODO: stub implementations for missing methods *)
    type opt_problem_t = std_form_problem_t
    let solve problem = Unbounded
    let of_constraints_and_objective constrs objective = {
        max_objective=Expression.of_const Number.zero;
        ltz_constraints=[];
        vars=[];
    }

end
