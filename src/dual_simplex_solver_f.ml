open Core.Std

module Make
    (Number : module type of Number_intf) (* Number type parametrization *)
    =
struct
    module Matrix = Matrix_f.Make(Number)
    module Vector = Vector_f.Make(Number)

    (* Helper function for assertions *)
    let v cond msg = (if not cond then print_endline msg; cond)

    type tableau_t = Matrix.t

    (* Each row has an associated basis variable.
     * Basis is the mapping row_no -> column_no, where column_no is the
     * column associated with the corresponding basis variable. *)
    type basis_t = int array

    type t = {
        tableau: tableau_t;
        basis: basis_t;
    }

    type solution_type_t = Unfeasible | Solution with sexp
    type solution_t = solution_type_t * t

    type dual_solution_type_t = Unbounded | DualSolution with sexp
    type dual_solution_t = (dual_solution_type_t * (
        tableau_t * basis_t
    ))

    (** Change the basis after the pivot. **)
    let change_basis (basis:basis_t) ~row ~column =
        (* NOTE: this function *pretends* to be functional, while it is
         * actually procedural in nature. *)
        let () = basis.(row) <- column in
        basis

    (* Get the constants column out of the tableau *)
    let constants (tableau:tableau_t) =
        (* Number of coefficients in the equations represented in the simplex
         * tableau. **)
        let no_coeffs = ((Matrix.width tableau) - 1) in

        (* <|> is a slicing operator from Core. *)
        (Matrix.column_as_vector tableau no_coeffs) <|> (0, (-1))

    let cost_row (tableau:tableau_t) = 
        (* Last row without the last column. *)
        (Array.nget tableau (-1)) <|> (0, -1)

    let obj_value tableau =
        Number.(~/ Array.(nget (nget tableau (-1)) (-1)))

    (** Perform pivot on the matrix with the given row and column index
     *  Makes all elements in the given column 0 except for the given row.
     *  Makes given row have 1 in the given column. *)
    let pivot
            (m:Matrix.t)
            (Matrix.Row pivot_row_no)
            (Matrix.Column pivot_column_no)
            : Matrix.t =
        Log.debugf "Pivot row = %d, column = %d" pivot_row_no pivot_column_no;
        let row_vector = m.(pivot_row_no) in
        let pivot_element = m.(pivot_row_no).(pivot_column_no) in
        Vector.(
            let normalized_row = row_vector //. pivot_element in
            let substitute row =
                row -- (normalized_row **. row.(pivot_column_no)) in
            Array.init (Matrix.height m) ~f:(fun row_no ->
                if row_no = pivot_row_no then
                    normalized_row
                else
                    let row = m.(row_no) in
                    let ret = substitute row in
                    assert (v (ret.(pivot_column_no) = Number.zero)
                        ("Value = "^(Number.to_string ret.(pivot_column_no)))
                    );
                    ret
            )
        );;

    (** Perform a single pivot step *)
    let pivot_step tableau basis : dual_solution_t =
        let constants_v = constants tableau in
        let cost_row_v = cost_row tableau in

        let pivot_row_idx, _ = (Array.findi_exn
            constants_v ~f:(fun _ el -> Number.(el </ zero))) in

        (* Pivot row without the last element (infix slice) *)
        let pivot_row = tableau.(pivot_row_idx) <|> (0, (-1)) in

        let () = Log.debugf "Pivot row = %s" (Vector.to_string pivot_row) in
        let () = Log.debugf "Cost row = %s" (Vector.to_string cost_row_v) in

        (* Find the smallest ratio of the negative entries in the
         * pivot row with the corresponding cost row entries.
         * The value will stay [None] iff there are no negative entries in the
         * pivot row. *)
        let pivot_column_idx_value = Array.foldi pivot_row
            ~init:None
            ~f:(
                fun idx current_min coeff ->
                    let open Number in
                    if coeff </ zero then (
                        let ratio = cost_row_v.(idx) // coeff in
                        match current_min with
                            | Some (current_min_idx, current_min_ratio) ->
                                if ratio </ current_min_ratio then
                                    Some (idx, ratio)
                                else
                                    Some (current_min_idx, current_min_ratio)
                            | None -> Some (idx, ratio)
                    ) else
                        current_min
            ) in

        match pivot_column_idx_value with
            (* If no suitable pivot column was found, that means that
             * the dual is _unbounded_, and thus the primal is unfeasible. *)
            | None -> (Unbounded, (tableau, basis))
            | Some (pivot_column_idx, _) ->

                Log.debugf "Pivot row idx = %d; Pivot column idx = %d"
                    pivot_row_idx pivot_column_idx;

                assert (pivot_row_idx >= 0);
                assert (pivot_column_idx >= 0);
                let basis = change_basis
                    basis ~row:pivot_row_idx ~column:pivot_column_idx in
                let tableau = pivot tableau
                    (Matrix.Row pivot_row_idx)
                    (Matrix.Column pivot_column_idx) in

                (DualSolution, (tableau, basis))

    let dual_simplex ({tableau; basis} : t) : solution_t =
        let open Number in

        (* Constants: column of constants, sans the cost row coefficient. **)
        let cost_row_v tableau = cost_row tableau in

        let is_primal_feasible tableau =
            (Vector.vmin_exn (constants tableau)) >=/ zero in
        let is_dual_feasible tableau =
            (Vector.vmax_exn (cost_row_v tableau)) <=/ Number.zero in

        (** Assert that the tableau is dual-feasible. *)
        if (not (is_dual_feasible tableau)) then failwith
            "The Dual Simplex algorithm can work only with the tableaus
            which are 0-feasible in the dual representation";

        (** Inner tail-recursive function. **)
        let rec perform_dual_simplex
                (tableau:tableau_t)
                (basis:basis_t)
                : solution_t =
            Log.debug (lazy
                (sprintf "tableau: \n%s" (Matrix.to_string tableau)));
            match is_primal_feasible tableau with
                | true -> (Solution, {tableau=tableau; basis=basis})
                | false ->
                    let pivotted_tableau = (pivot_step tableau basis) in
                    match pivotted_tableau with
                        (* If the dual is unbounded, the primal
                         * is unfeasible *)
                        | Unbounded, (tableau, basis) ->
                            (Unfeasible, {tableau; basis})
                        | DualSolution, (tableau, basis) ->
                            (perform_dual_simplex tableau basis) in

        perform_dual_simplex tableau basis
end
