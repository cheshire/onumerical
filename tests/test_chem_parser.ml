open Core.Std
open OUnit2
open Chem_parser

let tests = "chem_formula_parser" >::: [
    "test_parsing" >:: (fun _ ->
        let input_str = "H2SO4 + Ca -> CaSO2 + H2 + O2" in
        let equation = of_string input_str in

        (* Constructor helper *)
        let a atom coeff = (Atom atom, Coeff coeff) in
        let expected_tree = {
            lhs = [
                [(a "H" 2); (a "S" 1); (a "O" 4)];
                [(a "Ca" 1);]
            ];
            rhs = [
                [(a "Ca" 1); (a "S" 1); (a "O" 2)];
                [(a "H" 2)];
                [(a "O" 2)];
            ];
        } in
        assert_equal expected_tree equation
            ~printer:to_string
            ~msg:"Checking that the expression is parsed correctly";
        assert_equal input_str (to_string equation)
            ~msg:"Conversion back to the string should be identity";
        ()
    );
]
