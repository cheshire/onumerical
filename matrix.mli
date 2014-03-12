(** Matrixes from linear algebra, parametrized by a number type. *)

module Make
    (Number : module type of Number) (* Number type parametrization *)
    :
sig
    (** Element stored in the matrix *)
    type element_t = Number.t

    (** Matrix type *)
    type t = element_t array array

    type column_idx_t = Column of int
    type row_idx_t = Row of int

    (** Create a matrix of the given dimensions *)
    val create : height:int -> width:int -> element_t -> t

    (** Create an identity matrix of a given size *)
    val create_identity: int -> t

    (** Add two matrices *)
    val (+++) : t -> t -> t

    (** Multiply two matrices *)
    val ( *** ) : t -> t -> t

    (** Operations on a matrix and a scalar *)
    val (+++.) : t -> element_t -> t
    val (///.) : t -> element_t -> t
    val ( ***.) : t -> element_t -> t

    (** Get matrix dimensions *)
    val width : t -> int
    val height : t -> int

    (** Extract the column as vector *)
    val column_as_vector : t -> int -> Vector.t

    (** Apply a function to each element *)
    val map : t -> (element_t -> element_t) -> t

    (** Adjoint a second matrix horizontally on the right *)
    val adjoint_horizontal : t -> t -> t

    (** Adjoint a second matrix vertically below *)
    val adjoint_vertical : t -> t -> t

    (** Adjoint second matrix horizontally after the given column *)
    val adjoint_horizontal_at_column : t -> int -> t -> t

    (** Makes all elements in the given column 0 except for the given row.
      * Makes given row have 1 in the given column. *)
    val pivot : t -> row_idx_t -> column_idx_t -> t

    (** Pretty-print the matrix to string *)
    val to_string : t -> string
end
