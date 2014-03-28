open Core.Std
open OUnit2

module Solver = Opt_solver_f.Make(Str_var)(Float_number)
module Expression = Solver.Expression

open Solver
open Solver.InputConstraintType

let tests = "opt_solver" >::: [
    "main" >:: (fun _ ->
        (* Helper function for expression conversion *)
        let e_conv (coeffs, const) = Expression.of_assoc_list_and_const
            coeffs const in
        let opt_conv constrs obj =
            of_constraints_and_objective
            (List.map constrs ~f:(fun constr -> (LessThanZero,
                                                    (e_conv constr))))
            (Maximize (e_conv obj)) in
        let constraints = [
            ([("x1", -1.0); ("x2", -1.0); ("x3", 2.0)], 3.0);
            ([("x1", -4.0); ("x2", -2.0); ("x3", 1.0)], 4.0);
            ([("x1", 1.0); ("x2", 1.0); ("x3", -4.0)], -2.0);
        ] in
        let objective = ([("x1", -4.0); ("x2", -2.0); ("x3", -1.0)], 0.0) in

        let opt_problem = opt_conv constraints objective in
        let solution = solve opt_problem in
        let feasible_solution = match solution with
            | Solution f -> f
            | _ -> failwith "The system should be solved" in

        assert_equal 8.5 feasible_solution.value
            ~printer: Float_number.to_string
            ~msg: "Checking the solution value";
        assert_equal
            [("x1", 0.0); ("x2", 4.0); ("x3", 0.5)]
            feasible_solution.primal_var_assignment
            ~msg: "Primal variable assignment should work"
            ~printer: (fun s -> string_of_sexp (sexp_of_var_map_t s))
            (* Comparison which ignores the ordering *)
            ~cmp: (fun a b -> (Map.Poly.equal Float.equal
                    (Map.Poly.of_alist_exn a) (Map.Poly.of_alist_exn b))
            );
        ()
    )
]
