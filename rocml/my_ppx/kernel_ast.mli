open! Core

module rec Expression : sig
  type t =
    | Identifier of Identifier.t
    | Constant of Constant.t
    | Assign of
        { ident : Identifier.t
        ; assignment : Expression.t
        ; within_scope : Expression.t
        }
    | Apply of
        { f : t
        ; args : Arg.t list
        }
    | If_then_else of
        { condition : Expression.t
        ; then_ : Expression.t
        ; else_ : Expression.t option
        }
    | For of
        { from : Expression.t
        ; to_ : Expression.t
        ; body : Expression.t
        ; direction : Direction.t
        }
    | While of
        { condition : Expression.t
        ; body : Expression.t
        }
    | Arr_update of
        { ident : Identifier.t
        ; value : Identifier.t
        }
    | Sequence of
        { this : Expression.t
        ; next : Expression.t
        }
end

and Arg : sig
  module Label : sig
    type t =
      | Unnamed
      | Labelled of string
      | Optional of string
  end

  type t =
    { label : Label.t
    ; expression : Expression.t
    }
end

and Identifier : sig
  type t = { name : string }
end

and Constant : sig
  type t =
    | Bool of bool
    | Int of int
    | Float of float
end

and Direction : sig
  type t =
    | Up
    | Down
end

module Func_arg : sig
  module Type : sig
    type t =
      | Bool
      | Int
      | Float
      | Array of t
  end

  type t =
    { ident : Identifier.t
    ; type_ : Type.t
    }
end

type t =
  { func_args : Func_arg.t list
  ; body : Expression.t
  }
[@@deriving sexp_of]
