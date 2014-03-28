open Core.Std
open OUnit2
open Chem_parser

(* Constructor helper *)
let a atom coeff = (Atom atom, Coeff coeff)

let tests = "chem_formula_parser" >::: [
    "test_parsing" >:: (fun _ ->
        let input_str = "H2SO4 + Ca -> CaSO2 + H2 + O2" in
        let equation = match of_string input_str with
            | Some e -> e
            | None -> assert_failure "Equation should be parsed" in

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

    "test_add_coeffs" >:: (fun _ ->
        let open Coeff_equation in
        let input_eqn = {
            lhs = [
                [(a "S" 1);];
                [(a "H" 2); (a "S" 1); (a "O" 4)];
            ];
            rhs = [
                [(a "S" 1); (a "O" 2)];
                [(a "H" 2); (a "O" 1)];
            ];
        } in
        let expected_eqn_with_coeff = {
            lhs_c = [
                (1, [(a "S" 1);]);
                (1, [(a "H" 2); (a "S" 1); (a "O" 4)]);
            ];
            rhs_c = [
                (3, [(a "S" 1); (a "O" 2)]);
                (2, [(a "H" 2); (a "O" 1)]);
            ];
        } in
        assert_equal expected_eqn_with_coeff (Converter.add_coeffs input_eqn)
            ~printer:Coeff_equation.to_string
            ~msg:"Coefficients should be added correctly";
        ()
    );
]
