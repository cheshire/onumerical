open Core.Std

module Make
    (Number : module type of NumberIntf) (* Number type parametrization *)
    =
struct
    type element_t = Number.t
    type t = element_t array

    let create (size:int) (initial:'a) = Array.create ~len:size initial 

    let (++) = Array.map2_exn ~f:Number.(+/)
    let (//) = Array.map2_exn ~f:Number.(//)
    let (--) = Array.map2_exn ~f:Number.( */)

    let _scalar_op ~f v scalar = Array.map ~f:(fun el -> f el scalar) v

    let (--.) = _scalar_op ~f:Number.(-/)
    let (++.) = _scalar_op ~f:Number.(+/)
    let ( **. ) = _scalar_op ~f:Number.( */ )
    let (//.) = _scalar_op ~f:Number.(//)

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
    let vcmp ~comparator ~init v = Array.fold v ~init:init
            ~f:(fun extremum element -> (comparator extremum element))

    let vmin = vcmp ~comparator:min ~init:Number.(infinity)

    let vmax = vcmp ~comparator:max ~init:Number.(~/ infinity)

    (* Helper function for argmin/argmax. *)
    let vargcmp ~comparator (v:element_t array) =
        let () = assert (Array.length v > 0) in
        let ret = Array.foldi v ~init:0
                ~f:(fun current_pos extremum_pos el ->
                    if (comparator v.(extremum_pos)  el)
                    then extremum_pos else current_pos) in
        assert (ret > 0);
        ret

    let vargmin = vargcmp ~comparator:(<)
    let vargmax = vargcmp ~comparator:(>)
end
