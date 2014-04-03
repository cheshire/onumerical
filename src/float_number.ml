open Core.Std

module S : (module type of Number_intf with type t = float) = struct
    type t = float with sexp
    let zero = 0.0
    let one = 1.0
    let of_int = Float.of_int
    let infinity = Float.infinity

    let (+/) = (+.)
    let (-/) = (-.)
    let ( */) = ( *.)
    let (//) = (/.)

    let (~/) = Float.neg

    let abs = Float.abs

    let compare = Float.compare
    let (<=/) = Float.(<=)
    let (>=/) = Float.(>=)
    let (=/) = Float.(=)
    let (</) = Float.(<)
    let (>/) = Float.(>)
    let min = Float.min
    let max = Float.max

    let to_string = Float.to_string
end

include S
