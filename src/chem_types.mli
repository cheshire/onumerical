open Core.Std

(* TODO: do I need to declare the type definition as well? Or just the type
 * name? Presumably it depends on what the users of `chem_types` will do with
 * types --- if they want to parse them, they will have to use the actual
 * datastructure. *)
type equation_t = {lhs: formula_t; rhs: formula_t}
and formula_t = molecule_t list
and molecule_t = Molecule of (atom_t, coeff_t) Map.Poly.t
and coeff_t = Coeff of int
and atom_t = Atom of string with sexp

type equation_with_coeffs_t = {
    lhs: formula_with_coeffs_t; rhs: formula_with_coeffs_t}
and formula_with_coeffs_t = (molecule_t * int) list with sexp

val converter : string -> equation_t

