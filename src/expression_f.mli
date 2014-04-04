(** See [Expression_intf] for the documentation. *)

open Core.Std

module Make
    (Var : module type of Var_intf) (* Variable type parametrization *)
    (Number : module type of Number_intf) (* Number type parametrization *)

    : (module type of Expression_intf

    with type
        var_t := Var.t
    and type
        number_t := Number.t)

