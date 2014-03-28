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
            print_endline (Coeff_equation.to_string (Converter.add_coeffs eqn))

(** Keep reading the input and converting it *)
let rec loop () =
    let() = read_and_print () in
    loop ()

(** Run the main loop *)
let () = loop ()
