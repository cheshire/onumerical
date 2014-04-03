open Core.Std

module S : (module type of Number_intf with type t = Q.t) = struct
    type t = Q.t

    (* SEXP-serialization *)
    let t_of_sexp = Fn.compose Q.of_string string_of_sexp
    let sexp_of_t = Fn.compose sexp_of_string Q.to_string

    let zero = Q.zero
    let one = Q.one
    let infinity = Q.inf

    let of_int = Q.of_int

    let (+/) = Q.(+)
    let (-/) = Q.(-)
    let (//) = Q.(/)
    let ( */) = Q.( * )

    let (~/) = Q.neg

    let abs = Q.abs

    let compare = Q.compare
    let (<=/) = Q.leq
    let (>=/) = Q.geq
    let (=/) = Q.equal
    let (</) = Q.lt
    let (>/) = Q.gt
    let min = Q.min
    let max = Q.max

    let to_string = Q.to_string
end

include S
