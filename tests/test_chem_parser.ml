open Core.Std
open OUnit2
open Chem_parser

let tests = "chem_formula_parser" >::: [
    "test_parsing" >:: (fun _ ->
        let input_str = "H2SO4 + Ca -> CaSO2 + H2 + O2" in
        let equation = match of_string input_str with
            | Some e -> e
            | None -> assert_failure "Equation should be parsed" in

        let expected_tree = {
            lhs = [
                [("H", 2); ("S", 1); ("O", 4)];
                [("Ca", 1);]
            ];
            rhs = [
                [("Ca", 1); ("S", 1); ("O", 2)];
                [("H", 2)];
                [("O", 2)];
            ];
        } in
        assert_equal expected_tree equation
            ~printer:to_string
            ~msg:"Checking that the expression is parsed correctly";
        assert_equal input_str (to_string equation)
            ~msg:"Conversion back to the string should be identity";
        ()
    );

    "test_add_coeffs" >:: (fun _ ->
        let open Coeff_equation in
        let input_eqn = {
            lhs = [
                [("S", 1);];
                [("H", 2); ("S", 1); ("O", 4)];
            ];
            rhs = [
                [("S", 1); ("O", 2)];
                [("H", 2); ("O", 1)];
            ];
        } in
        let expected_eqn_with_coeff = {
            lhs_c = [
                (1, [("S", 1);]);
                (2, [("H", 2); ("S", 1); ("O", 4)]);
            ];
            rhs_c = [
                (3, [("S", 1); ("O", 2)]);
                (2, [("H", 2); ("O", 1)]);
            ];
        } in
        assert_equal expected_eqn_with_coeff (Converter.add_coeffs input_eqn)
            ~printer:Coeff_equation.to_string
            ~msg:"Coefficients should be added correctly";
        ()
    );
]
