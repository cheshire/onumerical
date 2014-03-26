open Core.Std

module M = struct
    type t = Q.t
    let zero = Q.of_int 0
    let one = Q.of_int 1
    let infinity = Q.inf

    let of_int = Q.of_int

    let (+/) = Q.(+)
    let (-/) = Q.(-)
    let (//) = Q.(/)
    let ( */) = Q.( * )

    let (~/) = Q.neg

    let compare = Q.compare
    let (<=/) = Q.(leq)
    let (>=/) = Q.(geq)
    let (=/) = Q.(equal)
    let (</) = Q.(lt)
    let (>/) = Q.(gt)
    let min = Q.min
    let max = Q.max

    let to_string = Num.string_of_num

    (* TODO: better implementation? *)
    let to_string_with_padding s ~no_chars = Num.string_of_num s
end

include M
