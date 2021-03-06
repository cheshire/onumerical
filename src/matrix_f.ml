open Core.Std

module Make
    (Number : module type of Number_intf) (* Number type parametrization *)
    =
struct
    type t = Number.t array array

    type column_idx_t = Column of int
    type row_idx_t = Row of int

    module Vector = Vector_f.Make(Number)

    let create ~height ~width initial : t =
        Array.make_matrix ~dimx:height ~dimy:width initial

    let create_identity (size:int) : t = 
        let matrix = create ~height:size ~width:size Number.zero in
        for i = 0 to (size - 1) do
            matrix.(i).(i) <- Number.one
        done;
        matrix

    let (+++) (m1:t) (m2:t) : t = Array.mapi m1 ~f:(
        Vector.(fun idx v -> Number.(v ++ m2.(idx))))

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
    let _scalar_operation ~f (m:t) (scalar:Number.t) : t =
        map m (fun el -> f el scalar)

    let ( +++. ) = _scalar_operation ~f:Number.(+/)
    let ( ***. ) = _scalar_operation ~f:Number.( */)
    let ( ///. ) = _scalar_operation ~f:Number.(//)

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
        if (width m1 = width m2) then
            Array.append m1 m2
        else
            let () = Log.debugf "%s\n%s" (to_string m1) (to_string m2) in
            failwith "Matrix horizontal dimensions do not match, aborting"
end

