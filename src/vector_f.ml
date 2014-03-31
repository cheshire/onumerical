open Core.Std

(** Magic number: padding for pretty-printing *)
let number_padding = 5

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

    (** Alignment is a bit tricky. We want numbers in a table aligned
     * vertically, with minus signs (if needed) on the left. *)
    let _number_to_string_with_padding n ~no_chars =
        let abs = Number.abs n in
        let s = Number.to_string abs in
        let len = String.length s in
        let prefix =
            if Number.(n </ zero) then
                "-"
            else
                " " in
        (* 1 for prefix *)
        let postfix_len = max 0 (no_chars - len - 1) in
        let postfix = String.make postfix_len ' ' in
        prefix ^ s ^ postfix

    let to_string v =
        let len = Array.length v in
        "|" ^ (
            String.concat_array
                (Array.mapi v ~f:(fun idx e ->
                    let padding = if idx = len - 1 then 0 else number_padding in
                    _number_to_string_with_padding e ~no_chars:padding))
        ) ^ "|"

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
