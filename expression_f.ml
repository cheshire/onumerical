open Core.Std

module Make
    (Var : module type of VarIntf)
    (Number : module type of NumberIntf)
    =
struct
    type var_t = Var.t
    type number_t = Number.t
    type coeffs_t = (var_t, number_t) Map.Poly.t

    type t = {
        coeffs: coeffs_t;
        constant: Number.t
    }

    let _merge_expressions (expr1:t) (expr2:t) ~f : t =
        {coeffs=Map.Poly.merge expr1.coeffs expr2.coeffs ~f:(
            fun ~key:_ value ->
                    match value with
                        | `Left a -> Some a
                        | `Right b -> Some b
                        | `Both (a, b) -> Some (f a b)
            );
        constant=f expr1.constant expr2.constant}

    let (++) = _merge_expressions ~f:Number.(+/)
    let (--) = _merge_expressions ~f:Number.(-/)

    (* Helper function: modify the constant value in the expression using the
     * function *)
    let _modify_constant ({coeffs=expr_coeffs; constant=expr_constant}:t)
            (const:number_t) ~f : t =
        {coeffs=expr_coeffs; constant=(f expr_constant const)}

    let (++.) = _modify_constant ~f:Number.(+/)
    let (--.) = _modify_constant ~f:Number.(-/)

    let _modify_expression_by_constant (expr:t) (constant:number_t) ~f : t =
        {coeffs=Map.Poly.map expr.coeffs ~f:(fun value -> f value constant);
         constant=(f expr.constant constant)}

    let ( **.) = _modify_expression_by_constant ~f:Number.( */)
    let (//.) = _modify_expression_by_constant ~f:Number.(//)

    (* Expression negation *)
    let (~~) expr = expr **. (Number.(~/ one ))

    let of_var (var:var_t) : t =
        {coeffs=Map.Poly.singleton var Number.one;
         constant=Number.zero}

    let of_const (const:number_t) : t =
        {coeffs=Map.Poly.empty;
         constant=const}

    let of_assoc_list_and_const assoc_list const =
        {coeffs=Map.Poly.of_alist_exn assoc_list;
         constant=const}

    let coeff (expr:t) (var:var_t) : number_t =
        match (Map.Poly.find expr.coeffs var) with
            | Some c -> c
            | None -> Number.zero

    let const (expr:t) : number_t = expr.constant

    let coeffs (expr:t) : coeffs_t = expr.coeffs

    let substitute (expr:t) ~var ~substitution : t =
        let sub_expr = (substitution **. (coeff expr var)) in
        expr ++ sub_expr

    let vars_used (expressions:t list) : coeffs_t =
        List.fold expressions ~init:Map.Poly.empty
            ~f:(fun all_keys expr ->
                Map.Poly.merge all_keys (coeffs expr)~f:(
                    fun ~key:_ _ -> Some Number.one))
end
