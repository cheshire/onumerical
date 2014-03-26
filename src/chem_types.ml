open Core.Std
open Re2

type equation_t = {lhs: formula_t; rhs: formula_t}
and formula_t = molecule_t list
and molecule_t = Molecule of (atom_t, coeff_t) Map.Poly.t
and coeff_t = Coeff of int
and atom_t = Atom of string with sexp

type equation_with_coeffs_t = {
    lhs: formula_with_coeffs_t; rhs: formula_with_coeffs_t}
(* TODO: map vs associative list *)
and formula_with_coeffs_t = (molecule_t * int) list with sexp

let converter (input:string) : equation_t =
    let matches = Regex.(split (of_string (escape "->")) input) in
    match matches with
    | lhs::rhs::[] ->

        (* Splits molecule to atoms. E.g. H2SO4 -> Molecule
         * [(H, 2), (S, 1), (O, 4)] *)
        let atom_splitter (f:string) : molecule_t = Molecule (
            Map.Poly.of_alist_exn
                (List.map
                    ~f:(fun m ->
                        (Atom (Regex.Match.get_exn (`Index 1) m),
                         Coeff (
                            match (Regex.Match.get (`Index 2) m) with
                                | Some "" -> 1
                                | Some x -> Int.of_string x
                                | None -> 1
                        ))
                    )
                    (Regex.get_matches_exn
                        (Regex.of_string "([A-Z][a-z]*)([0-9]?)") f
                    ))
            ) in

        (* Splits the formula to molecules. E.g. H2SO4 + Ca ->
            * [(Molecule ...), (Molecule ...)] *)
        let molecule_splitter (f:string) : formula_t =
            List.map ~f:atom_splitter
                Regex.(split (of_string (escape "+")) f) in
        {lhs=molecule_splitter lhs; rhs=molecule_splitter rhs}
    | _ -> failwith "Incorrect input";;

let test () =
    converter "H2SO4 + Ca -> CaSO2 + H2 + O2";;
