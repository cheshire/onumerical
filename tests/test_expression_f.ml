open Core.Std
open OUnit2

module Expression = Expression_f.Make(Str_var)(Float_number)

let tests = "expressions" >::: [
    "main" >:: (fun _ ->
        let a = Expression.of_assoc_list_and_const
            [("x", 1.0); ("z", 2.0)] 5.0 in
        let b = Expression.of_assoc_list_and_const
            [("x", -1.0); ("z", 1.0)] 3.0 in
        let c = Expression.(a ++ b) in
        assert_equal ([("z", 3.0)], 8.0)
            (Expression.to_assoc_list_and_const c)
            ~printer: (fun (coeffs, c) ->
                    (Expression.of_assoc_list_and_const coeffs c) |>
                    Expression.to_string
                )
                ~msg: "Testing addition"
    )
]
