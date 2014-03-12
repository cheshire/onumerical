open Core.Std

open Simplex
open Chem_types

open Simplex_Parser

let equation_to_opt_problem (eq:equation_t) : opt_problem_t =
    (* TODO *)
    {constraints=[]; objective=(Map.empty ~comparator:String.comparator)}

let opt_problem_to_equation_with_coeffs
    (problem:opt_problem_t) : equation_with_coeffs_t = 

    (* TODO *)
    {lhs=[];
     rhs=[]}
