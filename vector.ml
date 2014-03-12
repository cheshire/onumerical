open Core.Std

type element_t = float
type t = element_t array

let create (size:int) (initial:'a) = Array.create ~len:size initial 

let (++) = Array.map2_exn ~f:(+.)
let (//) = Array.map2_exn ~f:(/.)
let (--) = Array.map2_exn ~f:(-.)

(* private operations? *)
let scalar_op ~f v scalar = Array.map ~f:(fun el -> f el scalar) v

let (--.) = scalar_op ~f:(-.)
let (++.) = scalar_op ~f:(+.)
let ( **. ) = scalar_op ~f:( *. )
let (//.) = scalar_op ~f:(/.)

let sum v = Array.fold ~init:0.0 ~f:(+.) v

let to_string v =
    sprintf "|%s|" (
        String.concat_array
            ~sep:"\t"
            (Array.map ~f:(fun e ->
                 (* OCaml displays negative zero as -0.0 by default =( *)
                 let norm = if e = 0.0 then 0.0 else e in

                 (* Padding space to leave the space for the
                  * nice alignment with negative numbers *)
                 let separator = if (e >= 0.0) then " " else "" in
                 sprintf "%s%.3f" separator norm
             ) v)
    )

let dot v1 v2 =
    let () = assert (Array.length v1 = Array.length v2) in
    sum (Array.map2_exn v1 v2 ~f:( *. ))

(** Helper function for min/max *)
let vcmp ~comparator ~init v = Array.fold v ~init:init
        ~f:(fun extremum element -> (comparator extremum element))

let vmin = vcmp ~comparator:min ~init:Float.infinity

let vmax = vcmp ~comparator:max ~init:(-.Float.infinity)

(* Helper function for argmin/argmax. *)
let vargcmp ~comparator (v:float array) =
    let () = assert (Array.length v > 0) in
    let ret = Array.foldi v ~init:0
            ~f:(fun current_pos extremum_pos el ->
                if (comparator v.(extremum_pos)  el)
                then extremum_pos else current_pos) in
    assert (ret > 0);
    ret

let vargmin = vargcmp ~comparator:(<)
let vargmax = vargcmp ~comparator:(>)
