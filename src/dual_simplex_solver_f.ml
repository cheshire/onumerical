open Core.Std

module Make
    (Number : module type of NumberIntf) (* Number type parametrization *)
    =
struct
    module Matrix = Matrix_f.Make(Number)
    module Vector = Vector_f.Make(Number)

    (* Helper function for assertions *)
    let v cond msg = (if not cond then print_endline msg; cond)

    type tableau_t = Matrix.t

    type t = {
        tableau: tableau_t;
        basis: int array;
    }

    type solution_type_t = Unfeasible | Solution with sexp
    type solution_t = solution_type_t * t

    (* Each row has an associated basis variable.
     * Basis is the mapping row_no -> column_no, where column_no is the
     * column associated with the corresponding basis variable. *)
    type basis_t = int array

    (** Change the basis after the pivot. **)
    let change_basis (basis:basis_t) ~row ~column =
        basis.(row) <- column

    (* Get the constants column out of the tableau *)
    let constants (tableau:tableau_t) =
        (* Number of coefficients in the equations represented in the simplex
         * tableau. **)
        let no_coeffs = ((Matrix.width tableau) - 1) in

        (* <|> is a slicing operator from Core. *)
        (Matrix.column_as_vector tableau no_coeffs) <|> (0, (-1))

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
    let pivot_step tableau basis cost_row =
        let constants_v = constants tableau in
        let cost_row = cost_row tableau in

        let pivot_row_idx, _ = match
            (* Find the first element which is smaller than zero. *)
            (Array.findi constants_v ~f:(fun _ el -> Number.(el </ zero)))
            with

                (* TODO: but i don't want to failwith. I want to... hm say
                 * that the problem is actually unfeasible?.. *)
                | None -> failwith "Unbounded"
                | Some (a, b) -> (a, b) in

        (* Pivot row without the last element (infix slice) *)
        let pivot_row = tableau.(pivot_row_idx) <|> (0, (-1)) in

        let () = Log.debugf "Pivot row = %s" (Vector.to_string pivot_row) in
        let () = Log.debugf "Cost row = %s" (Vector.to_string cost_row) in

        (* Find the smallest ratio of the negative entries in the
         * pivot row with the
         * corresponding cost row entries. *)
        let pivot_column_idx, _ = Array.foldi pivot_row
            ~init:(-1, None)
            ~f:(
                fun idx (current_min_idx, current_min) coeff ->
                    let open Number in
                    if coeff </ zero then (
                        let ratio = cost_row.(idx) // coeff in
                        match current_min with
                            | Some min_ratio ->
                                if ratio </ min_ratio then
                                    (idx, Some ratio)
                                else
                                    (current_min_idx, current_min)
                            | None -> (idx, Some ratio)
                    ) else
                        (current_min_idx, current_min)
            ) in
        Log.debugf "Pivot row idx = %d; Pivot column idx = %d"
            pivot_row_idx pivot_column_idx;

        (* TODO: OK and what happens if it's not? We're unfeasible? *)
        assert (pivot_row_idx >= 0);
        assert (pivot_column_idx >= 0);

        change_basis basis ~row:pivot_row_idx ~column:pivot_column_idx;

        pivot tableau
            (Matrix.Row pivot_row_idx) (Matrix.Column pivot_column_idx)

    let dual_simplex ({tableau; basis} : t) : solution_t =
        let open Number in

        (* Constants: column of constants, sans the cost row coefficient. **)
        let cost_row tableau = ((Array.nget tableau (-1)) <|> (0, (-1))) in

        let is_primal_feasible tableau =
            (Vector.vmin_exn (constants tableau)) >=/ zero in
        let is_dual_feasible tableau =
            (Vector.vmax_exn (cost_row tableau)) <=/ Number.zero in

        (** Assert that the tableau is dual-feasible. *)
        if (not (is_dual_feasible tableau)) then failwith
            "The Dual Simplex algorithm can work only with the tableaus
            which are 0-feasible in the dual representation";

        (** Inner tail-recursive function. **)
        let rec perform_dual_simplex (tableau:tableau_t) : tableau_t =
            Log.debug (lazy
                (sprintf "tableau: \n%s" (Matrix.to_string tableau)));
            match is_primal_feasible tableau with
                | true -> tableau
                | false -> perform_dual_simplex

                    (* now pivot_step can return "Unfeasible" as well! *)
                    (pivot_step tableau basis cost_row) in

        (Solution, {tableau=perform_dual_simplex tableau; basis=basis;})
end
