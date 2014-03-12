open Core.Std

type element_t = float
type t = float array array

type column_idx_t = Column of int
type row_idx_t = Row of int

let create ~height ~width (initial:element_t) : t =
    Array.make_matrix ~dimx:width ~dimy:height initial

let create_identity (size:int) : t = 
    Array.init size ~f:(
        fun idx_row -> Array.init size ~f:(
            fun idx_column -> if idx_row = idx_column then 1.0 else 0.0))

let (+++) m1 m2 = Array.mapi ~f:(Vector.(fun idx v -> v ++ m2.(idx))) m1

let height m = Array.length m
let width m = if (height m = 0) then 0 else (Array.length m.(0))

(* O(N) *)
let column_as_vector m column_no =
    Array.init (height m) ~f:(fun row_no -> m.(row_no).(column_no))

let to_string m =
    String.concat_array ~sep:"\n"
        (Array.map ~f:(fun v -> (Vector.to_string v)) m)

let map m f = Array.map ~f:(fun row -> (
        Array.map ~f:(fun c -> (f c)) row
    )) m

(* Helper for the operations involving a matrix and a scalar *)
let scalar_operation ~f (m:t) (scalar:element_t) : t =
    map m (fun el -> f el scalar)

let ( +++. ) (m:t) (scalar:element_t) : t = scalar_operation ~f:(+.) m scalar
let ( ***. ) = scalar_operation ~f:( *.)
let ( ///. ) = scalar_operation ~f:(/.)

(* Matrix multiplication *)
let ( *** ) (m1:t) (m2:t) : t =
    assert ((width m1) = (height m2));
    assert ((height m1) = (width m2));
    Array.init (height m1) ~f:(fun row_no ->
        let row = m1.(row_no) in
        Array.init (width m2) ~f:(fun column_no ->
            let column = column_as_vector m2 column_no in
            Vector.dot row column
        )
    )

let adjoint_horizontal (m1:t) (m2:t) : t =
    assert (height m1 = height m2);
    Array.init (width m1 + width m2)
        ~f:(fun idx -> Array.append m1.(idx) m2.(idx))

let adjoint_vertical (m1:t) (m2:t) : t =
    assert (width m1 = width m2);
    Array.append m1 m2

(* Adjoints `m2` horizontally AFTER the column specified by the `column_idx`
 * (zero-indexed) *)
let adjoint_horizontal_at_column (m1:t) (column_idx:int) (m2:t) : t =
    assert (height m1 = height m2);
        Array.init
            (width m1 + width m2)
            ~f:(fun idx -> Array.concat [
                m1.(idx) <|> (0, column_idx);
                m2.(idx);
                m1.(idx) <|> (column_idx, 0);
            ])

let pivot (m:t) (Row row) (Column column) : t =
    let row_vector = m.(row) in
    Log.debugf "Row vector = %s" (Vector.to_string row_vector);
    let pivot_element = m.(row).(column) in
    Vector.(
        let normalized_row = row_vector //. pivot_element in
        let substitute row =
            row -- (normalized_row **. row.(column)) in
        Array.init (height m) ~f:(fun row_no ->
            if row_no = row then normalized_row else
                let ret = substitute m.(row_no) in
                assert (ret.(column) = 0.0);
                ret
        )
    );;

