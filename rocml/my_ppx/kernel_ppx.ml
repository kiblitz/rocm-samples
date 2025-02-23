open! Core
open Ppxlib

let kernel_attribute = "kernel"

module Kernelize : sig
  val run : expression -> Kernel_ast.t
end = struct
  let on_expression expression =
    let raise_on_unexpected_expression kind =
      raise_s [%message "Cannot kernelize expression" (kind : string)]
    in
    match expression.pexp_desc with
    | Pexp_ident { txt = Lident ident; loc = _ } ->
      Kernel_ast.Expression.Identifier { name = ident }
    | Pexp_ident _ -> raise_on_unexpected_expression "Non_ident_var"
    | _ -> raise_on_unexpected_expression "TODO"
  ;;

  let rec on_function ?(func_args = []) expression =
    let raise_on_unexpected_function_param kind =
      raise_s [%message "Cannot kernelize function param" (kind : string)]
    in
    match expression.pexp_desc with
    | Pexp_fun
        ( Nolabel
        , None
        , { ppat_desc =
              Ppat_constraint
                ({ ppat_desc = Ppat_var { txt; _ }; _ }, { ptyp_desc = Ptyp_any; _ })
          ; _
          }
        , body ) ->
      let func_args =
        Kernel_ast.Func_arg.
          { ident = { name = txt }
          ; (* TODO *)
            type_ = Type.Int
          }
        :: func_args
      in
      on_function ~func_args body
    | Pexp_fun (Labelled _, _, _, _) ->
      raise_on_unexpected_function_param "Labelled_param"
    | Pexp_fun (Optional _, _, _, _) ->
      raise_on_unexpected_function_param "Optional_param"
    | Pexp_fun
        (_, _, { ppat_desc = Ppat_constraint ({ ppat_desc = Ppat_var _; _ }, _); _ }, _)
      -> raise_on_unexpected_function_param "Non_gpu_type"
    | Pexp_fun (_, _, { ppat_desc = Ppat_constraint _; _ }, _) ->
      raise_on_unexpected_function_param "Non_var_argument"
    | Pexp_fun (_, _, _, _) -> raise_on_unexpected_function_param "Untyped_argument"
    | (_body : expression_desc) ->
      { Kernel_ast.func_args = List.rev func_args; body = on_expression expression }
  ;;

  let run expression = on_function expression
end

let transform_value_binding value_binding =
  if
    List.exists value_binding.pvb_attributes ~f:(fun attribute ->
      [%equal: string] attribute.attr_name.txt kernel_attribute)
  then (
    let kernel_ast = Kernelize.run value_binding.pvb_expr in
    print_s [%message (kernel_ast : Kernel_ast.t)];
    { value_binding with pvb_expr = value_binding.pvb_expr })
  else value_binding
;;

let transform_structure_item structure_item =
  match structure_item.pstr_desc with
  | Pstr_value (Nonrecursive, value_bindings) ->
    (* Pexp_constraint expression, core_type*)
    let pstr_desc =
      Pstr_value (Nonrecursive, List.map value_bindings ~f:transform_value_binding)
    in
    { structure_item with pstr_desc }
  | _ -> structure_item
;;

let transform_structure structure = List.map structure ~f:transform_structure_item
let () = Driver.register_transformation kernel_attribute ~impl:transform_structure
