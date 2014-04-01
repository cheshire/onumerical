(** Command-Line interface *)

open Core.Std
open Chem_parser

let read_and_print () =
    (** Read the equation without the coefficients from the stdin,
     *  write the equation with coefficients to stdout *)
    let input = read_line () in
    match of_string input with
        | None -> print_endline ""
        | Some eqn ->
            match Converter.add_coeffs eqn with
                | Converter.Unsolvable -> print_endline
                    "The equation is unsatisfiable"
                | Converter.OutputSolution coeffs ->
                    print_endline (Coeff_equation.to_string coeffs)

(** Run the main loop *)
let () = read_and_print ()
