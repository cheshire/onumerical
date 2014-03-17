(** Variable type: anything serializable and comparable *)
open Core.Std

(** Variable datatype: serializable and comparable *)
type t with sexp, compare
