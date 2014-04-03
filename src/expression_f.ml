open Core.Std

module Make
    (Var : module type of Var_intf)
    (Number : module type of Number_intf)
    =
struct
    type coeffs_t = (Var.t, Number.t) Map.Poly.t with sexp, compare

    type t = {
        coeffs: coeffs_t;
        constant: Number.t
    } with sexp, compare

    let _merge_expressions (expr1:t) (expr2:t) ~f ~f2 : t =
        {coeffs=Map.Poly.merge expr1.coeffs expr2.coeffs ~f:(
            fun ~key:_ value ->
                    match value with
                        | `Left a -> Some a
                        | `Right b -> Some (f2 b)
                        | `Both (a, b) ->
                            let coeff = (f a b) in
                            if coeff = Number.zero then None else Some coeff
            );
        constant=f expr1.constant expr2.constant}

    let (++) = _merge_expressions ~f:Number.(+/) ~f2:Fn.id
    let (--) = _merge_expressions ~f:Number.(-/) ~f2:Number.(~/)

    (* Helper function: modify the constant value in the expression using the
     * function *)
    let _modify_constant ({coeffs=expr_coeffs; constant=expr_constant}:t)
            (const:Number.t) ~f : t =
        {coeffs=expr_coeffs; constant=(f expr_constant const)}

    let (++.) = _modify_constant ~f:Number.(+/)
    let (--.) = _modify_constant ~f:Number.(-/)

    let _modify_expression_by_constant (expr:t) (constant:Number.t) ~f : t =
        {coeffs=Map.Poly.map expr.coeffs ~f:(fun value -> f value constant);
         constant=(f expr.constant constant)}

    let ( **.) = _modify_expression_by_constant ~f:Number.( */)
    let (//.) = _modify_expression_by_constant ~f:Number.(//)

    (* Expression negation *)
    let (~~) expr = expr **. (Number.(~/ one ))

    let of_var (var:Var.t) : t =
        {coeffs=Map.Poly.singleton var Number.one;
         constant=Number.zero}

    let of_const (const:Number.t) : t =
        {coeffs=Map.Poly.empty;
         constant=const}

    let of_assoc_list_and_const assoc_list const =
        (* Ignore the variables associated with the coefficient of zero. *)
        let alist = List.filter assoc_list ~f:(
            fun (_, coeff) -> 
                if coeff = Number.zero then
                    false
                else
                    true) in
        {coeffs=Map.Poly.of_alist_exn alist; constant=const}

    let to_assoc_list_and_const expr = (
            Map.Poly.to_alist expr.coeffs, expr.constant)

    let coeff (expr:t) (var:Var.t) : Number.t =
        match (Map.Poly.find expr.coeffs var) with
            | Some c -> c
            | None -> Number.zero

    let const (expr:t) : Number.t = expr.constant

    let coeffs (expr:t) : coeffs_t = expr.coeffs

    let substitute (expr:t) ~var ~substitution : t =
        let sub_expr = (substitution **. (coeff expr var)) in
        expr ++ sub_expr

    let vars_used (expressions:t list) : coeffs_t =
        List.fold expressions ~init:Map.Poly.empty
            ~f:(fun all_keys expr ->
                Map.Poly.merge all_keys (coeffs expr) ~f:(
                    fun ~key:_ _ -> Some Number.one))

    let to_string {coeffs; constant} =
        let coeffs = Map.Poly.fold coeffs ~init:[] ~f:(
            fun ~key:var ~data:coeff accum ->
                (
                    sprintf "%s (%s)"
                        (Number.to_string coeff)
                        (Var.to_string var)
                ) :: accum
        ) in
        sprintf "%s + %s"
            (String.concat ~sep:" + " coeffs)
            (Number.to_string constant)
end
