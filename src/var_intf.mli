(** Variable type: anything serializable and comparable *)
open Core.Std

(** Variable datatype: serializable, polymorphic comparison is used. *)
type t with sexp

val to_string : t -> string
