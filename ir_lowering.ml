(* Compiler Construction - Minimal Lambda Language *)

open Typed_ast
open Label_generator

let lower program =
  let next_id = ref 0 in
  let new_id () = let id = !next_id in next_id := id + 1; id in

  (* Map function IDs to functions. *)
  let funcs_by_id = Hashtbl.create 10 in
  Array.iter
    (Array.iter
       (fun func -> Hashtbl.add funcs_by_id func.id (func, new_id ()))
    )
    program;

  (* Pass through each function, lowering all functions and closures. *)
  let closures = ref [] in

  let lower_func func =
    match func.body with
    | None -> ()
    | Some body ->
      let rec lower_expr acc e =
        match e with
        | FuncExpr(_, id) ->
          (* Decide whether the symbol is a function or a builtin. *)
          let { body; name; _ }, closure_id = Hashtbl.find funcs_by_id id in
          (match body with
           | None ->
             Ir.GetBuiltin name :: acc
           | Some _ ->
             Ir.GetClosure closure_id :: acc
          )
        | EnvExpr(_, id) ->
          Ir.GetEnv id :: acc
        | BoundExpr(_, id) ->
          Ir.GetLocal id :: acc
        | ArgExpr(_, id) ->
          Ir.GetArg id :: acc
        | IntExpr(_, i) ->
          Ir.ConstInt i :: acc
        | BoolExpr(_, i) ->
          Ir.ConstBool i :: acc
        | AddExpr(_, lhs, rhs) ->
          Ir.Add :: lower_expr (lower_expr acc lhs) rhs
        | SubExpr(_, lhs, rhs) ->
          Ir.Sub :: lower_expr (lower_expr acc lhs) rhs
        | EqualsExpr(_, lhs, rhs) ->
          Ir.Equals :: lower_expr (lower_expr acc lhs) rhs
        | NotEqualsExpr(_, lhs, rhs) ->
          Ir.NotEquals :: lower_expr (lower_expr acc lhs) rhs
        | AndExpr(_, lhs, rhs) ->
          Ir.And :: lower_expr (lower_expr acc lhs) rhs
        | OrExpr(_, lhs, rhs) ->
          Ir.Or :: lower_expr (lower_expr acc lhs) rhs
        | LambdaExpr(_, num_params, env, body) ->
          (* Create a new closure from the body. *)
          let id = new_id() in
          let ir_body = Ir.Return :: lower_expr [] body in
          let num_captures = Array.length env in
          let closure =
            { Ir.id
            ; Ir.name = None
            ; Ir.num_params
            ; Ir.num_captures
            ; Ir.num_locals = 0
            ; Ir.insts =  Array.of_list (List.rev ir_body)
            }
          in
          closures := closure :: !closures;
          (* Instantiate the closure with the captures. *)
          Ir.Closure(id, num_captures) :: Array.fold_left lower_expr acc env
        | CallExpr(_, callee, args) ->
          Ir.Call :: lower_expr (Array.fold_left lower_expr acc args) callee
      in
      (* The return instruction, same across the function. *)
      let rec lower_body acc stmts =
        let extract_block block = match block with
          | None -> []
          | Some x -> x
        in
        match stmts with
        | ReturnStmt(_, e) :: rest ->
          lower_body (Ir.Return :: lower_expr acc e) rest
        | ExprStmt(_, e) :: rest ->
          lower_body (Ir.Pop :: lower_expr acc e) rest
        | BindStmt(_, id, e) :: rest ->
          lower_body (Ir.SetLocal id :: lower_expr acc e) rest
        | IfStmt(_, e, block) :: rest ->
          let end_label = new_label() in
          let block' = extract_block block in
          let lowered_block = lower_body ( Ir.If end_label :: lower_expr acc e ) block' in
          lower_body ( Ir.EndIf end_label :: lowered_block ) rest
        | IfElseStmt(_, e, if_block, else_block) :: rest ->
          let if_block' = extract_block if_block in
          let else_block' = extract_block else_block in
          let else_label = new_label() in
          let end_label = new_label() in
          let lowered_if_block = lower_body ( Ir.If else_label :: lower_expr acc e ) if_block' in
          let lowered_else_block =
            lower_body ( Ir.Else (end_label, else_label) :: lowered_if_block ) else_block'
          in
          lower_body ( Ir.EndIf end_label :: lowered_else_block ) rest
        | [] ->
          acc
      in
      let _, id = Hashtbl.find funcs_by_id func.id in
      let insts = lower_body [] body in
      let body = Ir.Return :: Ir.ConstInt 0 :: insts in
      let closure =
        { Ir.id
        ; Ir.name = Some(func.name)
        ; Ir.num_params = func.num_params
        ; Ir.num_captures = 0
        ; Ir.num_locals = func.num_locals
        ; Ir.insts = Array.of_list (List.rev body)
        }
      in
      closures := closure :: !closures
  in
  Array.iter (Array.iter lower_func) program;
  Array.of_list (List.rev !closures)
