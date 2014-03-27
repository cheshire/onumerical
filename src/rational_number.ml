open Core.Std

module S : (module type of Number_intf with type t = Q.t) = struct
    type t = Q.t

    (* SEXP-serialization *)
    let t_of_sexp = Fn.compose Q.of_string  string_of_sexp
    let sexp_of_t = Fn.compose sexp_of_string Q.to_string

    let zero = Q.of_int 0
    let one = Q.of_int 1

    let of_int = Q.of_int

    let (+/) = Q.(+)
    let (-/) = Q.(-)
    let (//) = Q.(/)
    let ( */) = Q.( * )

    let (~/) = Q.neg

    let compare = Q.compare
    let (<=/) = Q.leq
    let (>=/) = Q.geq
    let (=/) = Q.equal
    let (</) = Q.lt
    let (>/) = Q.gt
    let min = Q.min
    let max = Q.max

    let to_string = Q.to_string

    (* TODO: better implementation? *)
    let to_string_with_padding s ~no_chars = Q.to_string s
end

include S
