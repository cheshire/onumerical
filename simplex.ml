open Core.Std;;

let () = Log.color_on ()

(** Verbose assert *)
let vassert condition error = if not condition then failwith error

(** Implementation of the dual simplex algorithm *)
module Dual_Simplex = struct
    open Vector

    type tableau_t = Matrix.t

    (* Constraint row -> variable which is basic for that row *)
    type basis_t = int array

    type simplex_problem_t = {tableau:tableau_t; basis:basis_t}

    (** Change the basis after the pivot. **)
    let change_basis (basis:basis_t) ~row ~column =
        (* TODO: surely this is not the whole version.. I should do something
         * with the current value of `basis.(row)` as well *)
        basis.(row) <- column

    (* Get the constants column out of the tableau *)
    let constants (tableau:tableau_t) =
        (* Number of coefficients in the equations represented in the simplex
         * tableau. **)
        let no_coeffs = ((Matrix.width tableau) - 1) in
        (Matrix.column_as_vector tableau no_coeffs) <|> (0, (-1))

    (* Perform a single pivot step *)
    let pivot_step tableau basis cost_row =
        let constants_v = constants tableau in
        let cost_row = cost_row tableau in

        (* OK instead of the smallest one how about we choose the first
         * negative one?.. *)
        let pivot_row_idx, _ = Array.findi_exn
            constants_v ~f:(fun _ el -> el < 0.0) in

        (* Pivot row without the last element *)
        let pivot_row = tableau.(pivot_row_idx) <|> (0, (-1)) in

        (* Smallest ratio of the negative entries in the pivot row with the
         * corresponding cost row entries. NOTE: Infinity is used as a
         * guard *)
        Log.debugf "Pivot row = %s" (to_string pivot_row);
        Log.debugf "Cost row = %s" (to_string cost_row);
        let pivot_column_idx, _ = Array.foldi pivot_row
            ~init:(-1, Float.infinity)
            ~f:(fun idx current_min coeff ->
                let (_, smallest_ratio) = current_min in
                if coeff < 0.0 then (

                    (* error: idx out of bounds **)
                    let ratio = cost_row.(idx) /. coeff in
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

    let dual_simplex ({tableau; basis}:simplex_problem_t) =

        (* Constants: column of constants, sans the cost row coefficient. **)
        let cost_row tableau = ((Array.nget tableau (-1)) <|> (0, (-1))) in

        let is_primal_feasible tableau = (vmin (constants tableau)) >= 0.0 in
        let is_dual_feasible tableau = (vmax (cost_row tableau)) <= 0.0 in
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

module type Simplex_Parser_H = sig
    type opt_problem_t
    type simplex_with_mapping_t
    type assignment_t

    val to_tableau : opt_problem_t -> simplex_with_mapping_t
    val tableau_to_var_value : simplex_with_mapping_t -> assignment_t
end

(** Parametrization for the generic simplex parser. *)
module type VarType = sig
    type t
end

module GenericSimplex(Parametrization:VarType) : Simplex_Parser_H = struct
    open Dual_Simplex

    type variable_t = Parametrization.t
    and linear_expression_t = (variable_t, float) Map.Poly.t
    and constr_t = {expr:linear_expression_t; bound:float}
    and constraint_system = constr_t list
    and opt_problem_t = {
        constraints:constraint_system; objective:linear_expression_t}
    with sexp

    type assignment_t = float list with sexp

    type simplex_with_mapping_t = {
        simplex_problem: simplex_problem_t;

        (* Mapping column_no -> variable name. *)
        vars: variable_t list
    }

    (* Convert the sparse representation to the 2D array. *)
    let to_tableau (problem:opt_problem_t) : simplex_with_mapping_t =

        (* All variables in a list *)
        (* TODO: this is really quite ugly. *)
        let all_variables = Map.keys (List.fold
            problem.constraints
            ~init:Map.Poly.empty
            ~f:(fun all_vars {expr; _} ->
                Map.Poly.merge all_vars expr ~f:(fun ~key _ -> None)
            )) in

        let no_variables = List.length all_variables in
        let no_constraints = List.length problem.constraints in

        let expr_to_row (expr:linear_expression_t) =
            let row = Array.create ~len:no_variables 0.0 in
            List.iteri all_variables ~f:(
                fun idx value -> (
                    row.(idx) <- match Map.find expr value with
                        | None -> 0.0
                        | Some x -> x
                )
            );
            row in

        let constraint_matrix =
            Array.make_matrix ~dimx:no_variables ~dimy:no_constraints 0.0 in

        (* Populate the tableau *)
        List.iteri problem.constraints
            ~f:(fun constr_no {expr; bound} ->
                let constraint_row = (expr_to_row expr) in
               constraint_matrix.(constr_no) <-
                   Array.append constraint_row [| bound |]
            );

        (* Add the cost function row *)
        let tableau = Matrix.adjoint_vertical constraint_matrix
            [| Array.append (expr_to_row problem.objective) [| 0.0 |]  |] in

        let basis = Array.init
            no_constraints ~f:(fun row_no -> row_no + no_variables) in
        {
            simplex_problem={tableau; basis;};
            vars=all_variables
        }

    (* Converts a pivoted tableau to a vector of values for all variables *)
    let tableau_to_var_value (
        {simplex_problem; vars;} : simplex_with_mapping_t) : assignment_t =

        let {tableau; basis} = simplex_problem in

        let constants_v = constants tableau in

        let basis_idx test_var_idx = Array.find basis
            ~f:(fun var_idx -> (var_idx = test_var_idx)) in

        List.mapi vars ~f:(fun var_idx var ->
            match basis_idx var_idx with
                (* Variables not in the basis become 0 *)
                | None -> 0.0
                (* Otherwise take the corresponding value from the constants
                 * row *)
                | Some idx -> constants_v.(idx)
        )

end

module StringVarSimplex = GenericSimplex(String)

let main debug_mode () =
    (* TODO: change this one to parse the input value instead, and output the
     * coefficients for the variables (or, better yet, the complete equation
     * with the coefficients attached to the formulas) *)
    if debug_mode then Log.set_log_level Log.DEBUG else ();

    let tableau = [| 
        [| -1.0; -1.0; 2.0; 1.0; 0.0; 0.0; -3.0;|];
        [| -4.0; -2.0; 1.0; 0.0; 1.0; 0.0; -4.0;|];
        [| 1.0;   1.0; -4.0;0.0; 0.0; 1.0; 2.0 |];
        [| -4.0; -2.0; -1.0; 0.0; 0.0; 0.0; 0.0; |];
    |] in
    (* TODO: check?.. what kind of basis do we expect? *)
    let basis = [| 4; 5; 6; |] in
    Dual_Simplex.(
        let pivoted_tableau = dual_simplex {tableau; basis} in
        printf "\nOutput:\n%s\n" (Matrix.to_string pivoted_tableau.tableau)
    )

let command =
    Command.basic
        ~summary:"Perform the Dual Simplex Algorithm"
        ~readme:(fun () -> "Parse and optimize the value")

        (* Local open of the package + adds an anonymous flag *)
        Command.Spec.(
            empty
            +> flag "-d" no_arg ~doc:" Output the debugging information"
        )
        (** OMG but how do they do it? **)
        main

let () = Command.run ~version:"0.1" command
