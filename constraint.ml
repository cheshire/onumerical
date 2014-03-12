open Core.Std

module Make
    (Var : module type of Var)
    (Number : module type of Number)
    =
struct

    type var_t = Var.t
    type number_t = Number.t

    module Expression : Expression_intf.S
        with type var_t := var_t and type number_t := number_t = struct

        type var_t = Var.t with sexp, compare

        (* Used inside the datastructure for differentiating between variables and
         * constants *)
        type store_var_t = Var of var_t | Constant with sexp, compare
        type number_t = Number.t

        module VarMap = Map.Make(
            struct type t = store_var_t with sexp, compare end
        )

        type t = number_t VarMap.t

        let _merge_expressions (expr1:t) (expr2:t) ~f : t =
            VarMap.merge expr1 expr2 ~f:(
                fun ~key:_ value ->
                    match value with
                    (* hmm okay now how do I deal with those
                     * things over here?... *)
                    | `Left a -> Some a
                    | `Right b -> Some b
                    | `Both (a, b) -> Some (f a b)
            )

        let (++) = _merge_expressions ~f:Number.(+/)
        let (--) = _merge_expressions ~f:Number.(-/)

        (* Helper function: modify the constant value in the expression using the
         * function *)
        let _modify_constant (expr:t) (const:number_t) ~f : t =
            VarMap.change expr Constant (function
                | None -> Some const
                | Some c -> Some (f const  c)
            )

        let (++.) = _modify_constant ~f:Number.(+/)
        let (--.) = _modify_constant ~f:Number.(-/)

        let _modify_expression_by_constant (expr:t) (constant:number_t) ~f =
            VarMap.map expr ~f:(fun value -> f value constant)

        let ( **.) = _modify_expression_by_constant ~f:Number.( */)
        let (//.) = _modify_expression_by_constant ~f:Number.(//)

        (* Expression negation *)
        let (~~) expr = expr **. (Number.(~/ one ))

        let of_var (var:var_t) : t = VarMap.singleton
                (Var var) Number.one

        let of_const (const:number_t) : t = VarMap.singleton
                Constant const

        let coeff (expr:t) (var:var_t) : number_t = match (VarMap.find expr (Var var)) with
            | Some c -> c
            | None -> Number.zero

        let const (expr:t) : number_t = match (VarMap.find expr Constant) with
            | Some c -> c
            | None -> Number.zero
    end

    type constraint_type_t = EqualZero | LessThanZero | GreaterThanZero
    type constraint_t = (constraint_type_t * Expression.t)

    (* Constraint system *)
    (* TODO: better datastructure? *)
    type t = constraint_t list

    let (&&.) (constraint1:t) (constraint2:t) : t = constraint1 @ constraint2

    (* Helper function *)
    let _constraint_from_expr
        (expression1:Expression.t)
        (expression2:Expression.t) ~constraint_type : t =
        [(constraint_type, Expression.(expression1 -- expression2))]

    let (<=.) = _constraint_from_expr ~constraint_type:LessThanZero
    let (>=.) = _constraint_from_expr ~constraint_type:GreaterThanZero
    let (==.) = _constraint_from_expr ~constraint_type:EqualZero
end
