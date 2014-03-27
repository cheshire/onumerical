open Core.Std

module S:(module type of Var_intf with type t = string) = struct
    type t = string with sexp
    let to_string s = s
end

include S
