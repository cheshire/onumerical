open Core.Std

type t = float
let zero = 0.0
let one = 1.0
let infinity = Float.infinity
let of_int = Float.of_int

let (+/) = (+.)
let (-/) = (-.)
let ( */) = ( *.)
let (//) = (/.)

let (~/) = Float.neg

let compare = Float.compare
let (<=/) = Float.(<=)
let (>=/) = Float.(>=)
let (=/) = Float.(=)
let (</) = Float.(<)
let (>/) = Float.(>)
let min = Float.min
let max = Float.max

let to_string = Float.to_string

(* TODO: better implementation *)
let to_string_with_padding s ~no_chars = Float.to_string s
