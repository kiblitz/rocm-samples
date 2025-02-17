open! Core
open! Ppxlib

let f ~ctxt s =
  print_s [%message "log" (s : string)];
  Ast_builder.Default.eunit ~loc:(Expansion_context.Extension.extension_point_loc ctxt)
;;

let extension =
  Extension.V3.declare
    "my_ppx"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    f
;;

let () = Driver.register_transformation __MODULE__ ~extensions:[ extension ]
