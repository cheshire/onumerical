open Core.Std

module S:(module type of VarIntf) = struct
    type t = string with sexp, compare
end

include S
