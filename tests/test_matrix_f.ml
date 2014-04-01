open Core.Std
open OUnit2

module Matrix = Matrix_f.Make(Float_number)

let tests = "matrixes" >::: [
    "matrix multiplication" >:: (fun _ ->
        let a = [|
            [|2.0; 3.0; 4.0|];
            [|1.0; 9.0; 1.0|];
            [|0.5; -1.0; 0.0|];
        |] in
        let b = [|
            [|2.0; 3.0; 4.0|];
            [|3.0; 6.0; 3.0|];
            [|0.5; -1.0; 3.0|];
        |] in
        let expected = [|
            [|15.0; 20.0; 29.0|];
            [|29.5; 56.0; 34.0|];
            [|-2.0; -4.5; -1.0|];
        |] in
        assert_equal expected Matrix.(a *** b)
            ~printer: Matrix.to_string
            ~msg: "Checking dot-product";
        ()
    )
]

