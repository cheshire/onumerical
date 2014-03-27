open Core.Std

(** Matrixes from linear algebra, parametrized by a number type. *)

module Make
    (Number : module type of Number_intf) (* Number type parametrization *)
    :
sig
    (** Matrix type *)
    type t = Number.t array array

    type column_idx_t = Column of int
    type row_idx_t = Row of int

    module Vector : module type of Vector_f.Make(Number)

    (** Create a matrix of the given dimensions *)
    val create : height:int -> width:int -> Number.t -> t

    (** Create an identity matrix of a given size *)
    val create_identity: int -> t

    (** Add two matrices *)
    val (+++) : t -> t -> t

    (** Multiply two matrices *)
    val ( *** ) : t -> t -> t

    (** Operations on a matrix and a scalar *)
    val (+++.) : t -> Number.t -> t
    val (///.) : t -> Number.t -> t
    val ( ***.) : t -> Number.t -> t

    (** Get matrix dimensions *)
    val width : t -> int
    val height : t -> int

    (** Extract the column as vector *)
    (* TODO: what do we do about a vector? what if we don't want to put
     * implementation in the same place?.. *)
    val column_as_vector : t -> int -> Vector.t

    (** Apply a function to each element *)
    val map : t -> (Number.t -> Number.t) -> t

    (** Adjoint a second matrix horizontally on the right *)
    val adjoint_horizontal : t -> t -> t

    (** Adjoint a second matrix vertically below *)
    val adjoint_vertical : t -> t -> t

    (** Adjoint second matrix horizontally after the given column *)
    val adjoint_horizontal_at_column : t -> int -> t -> t

    (** Pretty-print the matrix to string *)
    val to_string : t -> string
end
