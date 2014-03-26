open Core.Std
open OUnit2

(* Logging initialization *)
let () =
    Log.color_on ();
    Log.set_log_level Log.DEBUG

module Simplex = Dual_simplex_solver_f.Make(Float_number)
module IntVector = Vector_f.Make(Int_number)

(* TODO: tests with rationals *)
module SimplexTest = struct
    open Simplex

    let main = "main" >::: [
        "test_var_parsing" >:: (fun test_ctxt ->
            ()
        );

        (** Optimal value obtained *)
        "test_dual_simplex_float" >:: (fun test_ctxt ->

            let tableau = [|
                [|-1.0; -1.0; 2.0; 1.0; 0.0; 0.0; -3.0|];
                [|-4.0; -2.0; 1.0; 0.0; 1.0; 0.0; -4.0|];
                [|1.0; 1.0; -4.0; 0.0; 0.0; 1.0; 2.0|];

                (* cost row *)
                [|-4.0; -2.0; -1.0; 0.0; 0.0; 0.0; 0.0|];
            |] in
            let solution_type, pivoted_tableau = 
                dual_simplex {tableau=tableau; basis=[|3; 4; 5|]} in
            let expected_tableau = [|
                [|1.0; 1.0; 0.0; -2.0; 0.0; -1.0; 4.0|];
                [|-2.0; 0.0; 0.0; -3.5; 1.0; -1.5; 3.5|];
                [|0.0; 0.0; 1.0; -0.5; 0.0; -0.5; 0.5|];

                (* cost row *)
                [|-2.0; 0.0; 0.0; -4.5; 0.0; -2.5; 8.5|]
            |] in
            assert_equal solution_type Solution
                ~printer:(fun a -> string_of_sexp
                    (sexp_of_solution_type_t a))
                ~msg:"The solution should be found";
            assert_equal expected_tableau pivoted_tableau.tableau
                ~printer:Matrix.to_string
                ~msg:"Checking that the dual simplex algorithm pivots as
                expected";
            assert_equal [|1; 4; 2|] pivoted_tableau.basis
                ~printer:IntVector.to_string
                ~msg:"Basis should be computed";
            (* TODO: check basis *)
            ()
        )
    ]
end

let () = run_test_tt_main SimplexTest.main
