open Core.Std
open OUnit2

module Simplex = Dual_simplex_solver_f.Make(Float_number)

open Simplex

let solution_type_to_str a = Sexp.to_string (sexp_of_solution_type_t a)

let tests = "dual_simplex" >::: [
    "bounded" >:: (fun _ ->
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
        assert_equal Solution solution_type
            ~printer:solution_type_to_str
            ~msg:"The solution should be found";
        assert_equal expected_tableau pivoted_tableau.tableau
            ~printer:Matrix.to_string
            ~msg:"Checking that the dual simplex algorithm pivots as
            expected";
        assert_equal [|1; 4; 2|] pivoted_tableau.basis
            ~printer:(fun x -> Sexp.to_string (sexp_of_array sexp_of_int x))
            ~msg:"Basis should be computed";
        ()
    );

    "unbounded" >:: (fun _ ->
        let tableau = [|
            [|-1.0; -1.0; 2.0; 1.0; 0.0; 0.0; -3.0|];
            [|-4.0; -2.0; 1.0; 0.0; 1.0; 0.0; -4.0|];
            [|1.0; 1.0; -1.0; 0.0; 0.0; 1.0; 2.0|];
            [|-4.0; -2.0; -1.0; 0.0; 0.0; 0.0; 0.0|];
        |] in
        let solution_type, _ = 
            dual_simplex {tableau=tableau; basis=[|3; 4; 5|]} in
        assert_equal Unfeasible solution_type
            ~printer:solution_type_to_str
            ~msg:"The tableau should be deemed unfeasible.";
        ()
    )
]
