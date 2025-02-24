open! Core
open Ppxlib

let kernel_attribute = "kernel"

module Kernelize : sig
  val run : expression -> Kernel_ast.t
end = struct
  let rec on_expression expression =
    let raise_on_unexpected_expression kind =
      raise_s [%message "Cannot kernelize expression" (kind : string)]
    in
    match expression.pexp_desc with
    (* Ident *)
    | Pexp_ident { txt = Lident ident; loc = _ } ->
      Kernel_ast.Expression.Identifier { name = ident }
    | Pexp_ident _ -> raise_on_unexpected_expression "Non_ident_var"
    (* Const *)
    | Pexp_constant (Pconst_integer (int_, None)) ->
      Kernel_ast.Expression.Constant (Int (Int.of_string int_))
    | Pexp_constant (Pconst_float (float_, None)) ->
      Kernel_ast.Expression.Constant (Float (Float.of_string float_))
    | Pexp_constant _ -> raise_on_unexpected_expression "Non_raw_constant_int_or_float"
    | Pexp_construct ({ txt = Lident "true"; _ }, None) ->
      Kernel_ast.Expression.Constant (Bool true)
    | Pexp_construct ({ txt = Lident "false"; _ }, None) ->
      Kernel_ast.Expression.Constant (Bool false)
    (* Assign *)
    | Pexp_let (Nonrecursive, value_bindings, within_scope) ->
      let assignments =
        let%map.List { pvb_pat; pvb_expr; _ } = value_bindings in
        let ident =
          match pvb_pat.ppat_desc with
          | Ppat_var { txt; _ } -> { Kernel_ast.Identifier.name = txt }
          | _ -> raise_on_unexpected_expression "Non_ident_var_assignment"
        in
        let value = on_expression pvb_expr in
        { Kernel_ast.Assignment.ident; value }
      in
      let within_scope = on_expression within_scope in
      Kernel_ast.Expression.Assign { assignments; within_scope }
    | Pexp_let (Recursive, _, _) -> raise_on_unexpected_expression "Recursive_assignment"
    (* Apply *)
    | Pexp_apply (func, args) ->
      let f = on_expression func in
      let args =
        let%map.List arg_type, expression = args in
        let arg = on_expression expression in
        let label =
          match arg_type with
          | Nolabel -> Kernel_ast.Arg.Label.Unnamed
          | Labelled name -> Labelled name
          | Optional name -> Optional name
        in
        { Kernel_ast.Arg.label; expression = arg }
      in
      Kernel_ast.Expression.Apply { f; args }
    (* If-then-else *)
    | Pexp_ifthenelse (condition, then_, else_) ->
      let condition = on_expression condition in
      let then_ = on_expression then_ in
      let else_ = Option.map else_ ~f:on_expression in
      Kernel_ast.Expression.If_then_else { condition; then_; else_ }
    (* For *)
    | Pexp_for ({ ppat_desc = Ppat_var { txt; _ }; _ }, from, to_, direction_flag, body)
      ->
      let ident = { Kernel_ast.Identifier.name = txt } in
      let from = on_expression from in
      let to_ = on_expression to_ in
      let body = on_expression body in
      let direction =
        match direction_flag with
        | Upto -> Kernel_ast.Direction.Up
        | Downto -> Down
      in
      Kernel_ast.Expression.For { ident; from; to_; direction; body }
    | Pexp_for _ -> raise_on_unexpected_expression "Non_var_for_iterator"
    (* While *)
    | Pexp_while (condition, body) ->
      let condition = on_expression condition in
      let body = on_expression body in
      Kernel_ast.Expression.While { condition; body }
    (* Sequence *)
    | Pexp_sequence (this, next) ->
      let this = on_expression this in
      let next = on_expression next in
      Kernel_ast.Expression.Sequence { this; next }
    (* Not part of syntax *)
    | Pexp_function _ -> raise_on_unexpected_expression "Function_pattern_match"
    | Pexp_fun _ -> raise_on_unexpected_expression "Lambda"
    | Pexp_match _ -> raise_on_unexpected_expression "Pattern_match"
    | Pexp_try _ -> raise_on_unexpected_expression "Try"
    | Pexp_tuple _ -> raise_on_unexpected_expression "Tuple"
    | Pexp_construct _ -> raise_on_unexpected_expression "Construct"
    | Pexp_variant _ -> raise_on_unexpected_expression "Variant"
    | Pexp_record _ -> raise_on_unexpected_expression "Record"
    | Pexp_field _ -> raise_on_unexpected_expression "Field"
    | Pexp_setfield _ -> raise_on_unexpected_expression "Set_field"
    | Pexp_array _ -> raise_on_unexpected_expression "Array_define"
    | Pexp_constraint _ -> raise_on_unexpected_expression "Type_constraint"
    | Pexp_coerce _ -> raise_on_unexpected_expression "Type_coerce"
    | Pexp_send _ -> raise_on_unexpected_expression "Method_call"
    | Pexp_new _ -> raise_on_unexpected_expression "New"
    | Pexp_setinstvar _ -> raise_on_unexpected_expression "Set_class_var"
    | Pexp_override _ -> raise_on_unexpected_expression "Override"
    | Pexp_letmodule _ -> raise_on_unexpected_expression "Assign_module"
    | Pexp_letexception _ -> raise_on_unexpected_expression "Assign_exception"
    | Pexp_assert _ -> raise_on_unexpected_expression "Assert"
    | Pexp_lazy _ -> raise_on_unexpected_expression "Lazy"
    | Pexp_poly _ -> raise_on_unexpected_expression "Method_body"
    | Pexp_object _ -> raise_on_unexpected_expression "Object"
    | Pexp_newtype _ -> raise_on_unexpected_expression "New_type"
    | Pexp_pack _ -> raise_on_unexpected_expression "Pack"
    | Pexp_open _ -> raise_on_unexpected_expression "Open"
    | Pexp_letop _ -> raise_on_unexpected_expression "Let_op"
    | Pexp_extension _ -> raise_on_unexpected_expression "Extension"
    | Pexp_unreachable -> raise_on_unexpected_expression "Unreachable"
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
                ( { ppat_desc = Ppat_var { txt; _ }; _ }
                , { ptyp_desc = (* TODO *) Ptyp_any; _ } )
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
