open Core.Std

module Make
    (Number : module type of Number_intf) (* Number type parametrization *)
    =
struct
    type t = Number.t array

    let create (size:int) (initial:'a) = Array.create ~len:size initial 

    let (++) = Array.map2_exn ~f:Number.(+/)
    let (//) = Array.map2_exn ~f:Number.(//)
    let (--) = Array.map2_exn ~f:Number.(-/)

    let _scalar_op ~f v scalar = Array.map ~f:(fun el -> f el scalar) v

    let (--.) = _scalar_op ~f:Number.(-/)
    let (++.) = _scalar_op ~f:Number.(+/)
    let ( **. ) = _scalar_op ~f:Number.( */ )
    let (//.) = _scalar_op ~f:Number.(//)

    let (~~) (v:t) = Array.map ~f:(fun el -> Number.(~/ el)) v

    let sum v = Array.fold ~init:Number.zero ~f:Number.(+/) v

    let to_string v =
        sprintf "|%s|" (
            String.concat_array
                ~sep:"\t"
                (Array.map ~f:(fun e ->
                    (*
                     (* OCaml displays negative zero as -0.0 by default =( *)
                     let norm = if e = 0.0 then 0.0 else e in

                     (* Padding space to leave the space for the
                      * nice alignment with negative numbers *)
                     let separator = if (e >= 0.0) then " " else "" in
                     sprintf "%s%.3f" separator norm
                     *)

                     sprintf "%s" (Number.to_string_with_padding e ~no_chars:5)
                 ) v)
        )

    let dot v1 v2 =
        let () = assert (Array.length v1 = Array.length v2) in
        sum (Array.map2_exn v1 v2 ~f:Number.( */ ))

    (** Helper function for min/max *)
    let _vcmp_exn ~comparator (v:Number.t array) = 
        match (Array.fold v ~init:None
            ~f:(fun (extremum:Number.t option) (element:Number.t) -> (
                match extremum with
                    | None -> Some element
                    | Some extremum_el -> Some (comparator extremum_el element)
            ))) with
        | Some x -> x
        | None -> failwith "Expected non-empty array"

    let vmin_exn = _vcmp_exn ~comparator:Number.min
    let vmax_exn = _vcmp_exn ~comparator:Number.max

end
