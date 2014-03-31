open Core.Std

module S : (module type of Number_intf with type t = int) = struct
    type t = int with sexp
    let zero = 0
    let one = 1
    let of_int x = x

    let (+/) = (+)
    let (-/) = (-)
    let ( */) = ( * )
    let (//) = (/)

    let (~/) = Int.neg

    let abs = Int.abs

    let compare = Int.compare
    let (<=/) = Int.(<=)
    let (>=/) = Int.(>=)
    let (=/) = Int.(=)
    let (</) = Int.(<)
    let (>/) = Int.(>)
    let min = Int.min
    let max = Int.max

    let to_string = Int.to_string
end

include S
