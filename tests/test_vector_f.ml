open Core.Std
open OUnit2

module Vector = Vector_f.Make(Float_number)

let tests = "vectors" >::: [
    "all" >:: (fun _ ->
        let a = [| 0.0; 3.0; 4.0 |] in
        let b = [| 2.0; 8.0; 2.0 |] in
        assert_equal 32.0 (Vector.dot a b)
            ~printer: Float.to_string
            ~msg: "Checking dot-product";
        assert_equal [| 2.0; 11.0; 6.0 |] (Vector.(a ++ b))
            ~printer: Vector.to_string
            ~msg: "Checking addition";
        assert_equal [| -2.0; -5.0; 2.0 |] (Vector.(a -- b))
            ~printer: Vector.to_string
            ~msg: "Checking subtraction";
        ()
    )
]
