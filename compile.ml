open Mgoast
open Mips

let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt;
  Printf.sprintf "_label_%i" !cpt

let strings = ref []
let new_string s =
  let l = new_label () in
  strings := (l, s) :: !strings;
  l

let current_fun_name = ref ""

module StrMap = Map.Make(String)

type struct_info = int * int StrMap.t
let struct_env : struct_info StrMap.t ref = ref StrMap.empty
let fun_return_env : typ list StrMap.t ref = ref StrMap.empty

let collect_info decls =
  List.iter (function
    | Struct s ->
        let size, fields_map = 
          List.fold_left (fun (offset, map) (id, _) ->
            (offset + 4, StrMap.add id.id offset map)
          ) 
          (0, StrMap.empty) s.fields
        in
        struct_env := StrMap.add s.sname.id (size, fields_map) !struct_env
    | Fun f ->
        fun_return_env := StrMap.add f.fname.id f.return !fun_return_env
  ) decls

let scan_frame_size _ body =
  let rec scan_stmt min_off instr =
    match instr.idesc with
    | Vars(ids, _, seq) ->
        let new_off = min_off - (4 * List.length ids) in
        scan_seq new_off seq
    | Block b -> scan_seq min_off b
    | For(_, b) -> scan_seq min_off b
    | If(_, b1, b2) -> 
        min (scan_seq min_off b1) (scan_seq min_off b2)
    | _ -> min_off
  and scan_seq off seq = List.fold_left scan_stmt off seq
  in
  let final_off = scan_seq (-4) body in
  -(final_off - 4)

let get_expr_type type_env e = match e.edesc with
  | Int _ -> TInt
  | Bool _ -> TBool
  | String _ -> TString
  | New s -> TStruct s
  | Nil -> TStruct "nil"
  | Var id -> (try StrMap.find id.id type_env with Not_found -> TInt)
  | Binop((Eq|Neq|Lt|Le|Gt|Ge|And|Or), _, _) -> TBool
  | Binop _ -> TInt
  | Unop(Not, _) -> TBool
  | Call(f, _) -> 
      (match StrMap.find_opt f.id !fun_return_env with
       | Some [t] -> t | Some [] -> TInt | Some (t::_) -> t
       | None -> TInt)
  | _ -> TInt
let rec tr_expr env type_env e = match e.edesc with
  | Int n -> li t0 (Int64.to_int n)
  | Bool b -> li t0 (if b then 1 else 0)
  | Nil -> li t0 0
  | String s -> la t0 (new_string s)
  | Var id ->
      let off = try StrMap.find id.id env with Not_found -> failwith ("Var introuvable: " ^ id.id) in
      lw t0 off fp
  | New s_name ->
      let (size, _) = StrMap.find s_name !struct_env in
      li a0 size @@ li v0 9 @@ syscall @@ move t0 v0
  | Dot(exp, field) ->
      tr_expr env type_env exp 
      @@ (match exp.edesc with
          | _ -> 
             let offset = 
               StrMap.fold (fun _ (_, fields) found ->
                 if found <> -1 then found
                 else if StrMap.mem field.id fields then StrMap.find field.id fields
                 else -1
               ) !struct_env (-1)
             in
             if offset = -1 then failwith ("Champ inconnu: " ^ field.id);
             lw t0 offset t0)
             
  | Binop(And, e1, e2) ->
      let l_end = new_label () in
      tr_expr env type_env e1
      @@ beqz t0 l_end
      @@ tr_expr env type_env e2
      @@ label l_end

  | Binop(Or, e1, e2) ->
      let l_end = new_label () in
      tr_expr env type_env e1
      @@ bnez t0 l_end
      @@ tr_expr env type_env e2
      @@ label l_end

  | Binop(bop, e1, e2) ->
      let is_string_eq = 
        match bop with 
        | Eq | Neq -> (get_expr_type type_env e1) = TString 
        | _ -> false 
      in

      if is_string_eq then
        tr_expr env type_env e1 
        @@ push t0 
        @@ tr_expr env type_env e2 
        @@ pop a0
        @@ move a1 t0
        @@ jal "_strcmp"
        @@ move t0 v0
        @@ (if bop = Neq then xori t0 t0 1 else nop)
      else
        let op = match bop with
          | Add -> add | Sub -> sub | Mul -> mul | Div -> div | Rem -> rem
          | Eq -> seq  | Neq -> sne | Lt -> slt  | Le -> sle | Gt -> sgt | Ge -> sge
          | _ -> failwith "Opérateur binaire non géré ou mal placé"
        in
        tr_expr env type_env e2 @@ push t0 @@ tr_expr env type_env e1 @@ pop t1 @@ op t0 t0 t1    
  
  | Unop(uop, e) ->
      tr_expr env type_env e @@ (match uop with Opp -> neg t0 t0 | Not -> xori t0 t0 1)
  | Call(f, args) ->
      let code_args = List.fold_right (fun arg code ->
          tr_expr env type_env arg @@ push t0 @@ code
        ) (List.rev args) nop 
      in
      code_args
      @@ jal f.id
      @@ addi sp sp (4 * List.length args)
      @@ move t0 v0 
  | _ -> failwith "Expression non gérée"

let rec tr_seq env type_env fp_off = function
  | [] -> nop
  | {idesc=Vars(ids, opt_t, init_seq); _} :: s ->
      let (new_env, new_type_env, new_off) = 
        List.fold_left (fun (e, et, o) id ->
           let new_o = o - 4 in
           let ty = match opt_t with Some t -> t | None -> TInt in
           (StrMap.add id.id new_o e, StrMap.add id.id ty et, new_o)
        ) (env, type_env, fp_off) ids
      in
      let final_type_env = match opt_t with
        | Some _ -> new_type_env
        | None -> 
            match init_seq with
            | {idesc=Set(_, rhss); _} :: _ -> 
                 let inferred_types = List.map (get_expr_type type_env) rhss in
                 if List.length ids = List.length inferred_types then
                   List.fold_left2 (fun acc id t -> StrMap.add id.id t acc) new_type_env ids inferred_types
                 else new_type_env
            | _ -> new_type_env
      in
      let (init_code, rest_of_init) = match init_seq with
        | {idesc=Set(lhss, rhss); _} :: rest ->
             let assign_code = 
               if List.length rhss = 1 && List.length lhss > 1 then
                 let rhs = List.hd rhss in
                 tr_expr env type_env rhs @@ push t0
                 @@ (
                   let rec assign_tuple i vars = match vars with
                     | [] -> nop
                     | lhs :: tail ->
                        lw t0 0(sp) @@ lw t0 (i * 4)(t0) 
                         @@ (match lhs.edesc with
                             | Var id -> let off = StrMap.find id.id new_env in sw t0 off fp
                             | Dot(obj, field) ->
                                 push t0 @@ tr_expr env type_env obj @@ pop t1 
                                 @@ let offset = StrMap.fold (fun _ (_, fs) f -> if StrMap.mem field.id fs then StrMap.find field.id fs else f) !struct_env (-1) in sw t1 offset t0
                             | _ -> failwith "Lvalue invalide")
                         @@ assign_tuple (i + 1) tail
                    in assign_tuple 0 lhss
                 ) @@ addi sp sp 4 
               else
                 List.fold_left2 (fun acc lhs rhs ->
                   acc @@ tr_expr env type_env rhs 
                   @@ push t0
                   @@ (match lhs.edesc with
                       | Var id when id.id = "_" -> pop t0
                       | Var id -> pop t0 @@ let off = StrMap.find id.id new_env in sw t0 off fp
                       | Dot(obj, field) ->
                           tr_expr env type_env obj @@ pop t1 
                           @@ let offset = StrMap.fold (fun _ (_, fs) f -> if StrMap.mem field.id fs then StrMap.find field.id fs else f) !struct_env (-1) in
                               sw t1 offset t0
                       | _ -> failwith "Lvalue invalide")
                 ) nop lhss rhss
             in
             (assign_code, rest)
        | _ -> (nop, init_seq)
      in
      init_code 
      @@ tr_seq new_env final_type_env new_off rest_of_init
      @@ tr_seq new_env final_type_env new_off s 
  | i :: s -> 
      tr_instr env type_env fp_off i 
      @@ tr_seq env type_env fp_off s

and tr_instr env type_env fp_off i = match i.idesc with
  | Vars _ -> failwith "Erreur interne: Vars devrait être géré par tr_seq"
  | Block b -> tr_seq env type_env fp_off b 
  | Expr e -> 
      (match e.edesc with
       | Print args ->
           List.fold_left (fun acc arg ->
             let ret_types = match arg.edesc with
                | Call(f, _) -> (try StrMap.find f.id !fun_return_env with Not_found -> [TInt])
                | _ -> [get_expr_type type_env arg]
             in
             match ret_types with
             | [] -> acc 
             | [ty] -> 
                 acc @@ tr_expr env type_env arg 
                 @@ move a0 t0 
                 @@ (match ty with
                     | TBool -> jal "_print_bool"
                     | TString -> li v0 4 @@ syscall
                     | TStruct _ -> jal "_print_ptr"
                     | _ -> li v0 1 @@ syscall)
             | types -> 
                 acc @@ tr_expr env type_env arg 
                 @@ move t1 t0 
                 @@ (
                    let rec print_tuple i = function
                      | [] -> nop
                      | ty::q ->
                          lw a0 (i*4) t1 
                          @@ (match ty with
                              | TBool -> jal "_print_bool"
                              | TString -> li v0 4 @@ syscall
                              | TStruct _ -> jal "_print_ptr"
                              | _ -> li v0 1 @@ syscall)
                          @@ print_tuple (i+1) q
                    in print_tuple 0 types
                 )
           ) nop args
       | _ -> tr_expr env type_env e)
  | Set(lhss, rhss) ->
      if List.length rhss = 1 && List.length lhss > 1 then
        let rhs = List.hd rhss in
        tr_expr env type_env rhs @@ push t0      
        @@ (
          let rec assign_tuple i vars = match vars with
            | [] -> nop
            | lhs :: rest ->
                lw t0 0(sp) @@ lw t0 (i * 4)(t0) 
                @@ (match lhs.edesc with
                    | Var id -> let off = StrMap.find id.id env in sw t0 off fp
                    | Dot(obj, field) ->
                        push t0 @@ tr_expr env type_env obj @@ pop t1 
                        @@ let offset = StrMap.fold (fun _ (_, fs) f -> if StrMap.mem field.id fs then StrMap.find field.id fs else f) !struct_env (-1) in sw t1 offset t0
                    | _ -> failwith "Lvalue invalide")
                @@ assign_tuple (i + 1) rest
          in assign_tuple 0 lhss
        ) @@ addi sp sp 4 
      else
        List.fold_left2 (fun acc lhs rhs ->
           acc @@ tr_expr env type_env rhs @@ push t0
          @@ (match lhs.edesc with
              | Var id when id.id = "_" -> pop t0
              | Var id -> pop t0 @@ let off = StrMap.find id.id env in sw t0 off fp
              | Dot(obj, field) ->
                  tr_expr env type_env obj @@ pop t1 
                  @@ let offset = StrMap.fold (fun _ (_, fs) f -> if StrMap.mem field.id fs then StrMap.find field.id fs else f) !struct_env (-1) in
                      sw t1 offset t0
              | _ -> failwith "Lvalue invalide")
        ) nop lhss rhss
  | If(c, s1, s2) ->
      let then_l = new_label() and end_l = new_label() in
      tr_expr env type_env c @@ bnez t0 then_l @@ tr_seq env type_env fp_off s2 @@ b end_l
      @@ label then_l @@ tr_seq env type_env fp_off s1 @@ label end_l
  | For(c, s) ->
      let test_l = new_label() and code_l = new_label() in
      b test_l @@ label code_l @@ tr_seq env type_env fp_off s @@ label test_l
      @@ tr_expr env type_env c @@ bnez t0 code_l
  | Return exprs ->
      let ret_label = !current_fun_name ^ "_ret" in
      let n = List.length exprs in
      if n = 0 then b ret_label
      else if n = 1 then 
        tr_expr env type_env (List.hd exprs) @@ move v0 t0 @@ b ret_label
      else
        let push_code = List.fold_right (fun e acc -> tr_expr env type_env e @@ push t0 @@ acc) (List.rev exprs) nop in
        let alloc_code = li a0 (n * 4) @@ li v0 9 @@ syscall @@ move t1 v0 in
        let rec fill i =
          if i >= n then nop
          else pop t0 @@ sw t0 (i * 4)(t1) @@ fill (i + 1)
        in
        push_code @@ alloc_code @@ fill 0 @@ move v0 t1 @@ b ret_label
  | Inc e ->
      tr_expr env type_env e @@ addi t0 t0 1 @@ (match e.edesc with
          | Var id -> let off = StrMap.find id.id env in sw t0 off fp
          | Dot(obj, field) ->
              push t0 @@ tr_expr env type_env obj @@ pop t1 
              @@ let offset = StrMap.fold (fun _ (_, fs) f -> if StrMap.mem field.id fs then StrMap.find field.id fs else f) !struct_env (-1) in
                 sw t1 offset t0
          | _ -> failwith "Lvalue invalide")
  | Dec e ->
      tr_expr env type_env e @@ addi t0 t0 (-1) @@ (match e.edesc with
          | Var id -> let off = StrMap.find id.id env in sw t0 off fp
          | Dot(obj, field) ->
              push t0 @@ tr_expr env type_env obj @@ pop t1 
              @@ let offset = StrMap.fold (fun _ (_, fs) f -> if StrMap.mem field.id fs then StrMap.find field.id fs else f) !struct_env (-1) in
                 sw t1 offset t0
          | _ -> failwith "Lvalue invalide")

let tr_fun df =
  current_fun_name := df.fname.id;
  let frame_size = scan_frame_size df.params df.body in
  let (env, env_typ, _) = List.fold_left (fun (e, et, o) (id, t) -> 
      (StrMap.add id.id o e, StrMap.add id.id t et, o + 4)
    ) (StrMap.empty, StrMap.empty, 4) df.params 
  in
  label df.fname.id
  @@ sw fp (-4)(sp) @@ addi fp sp (-4) @@ sw ra (-4)(fp) @@ addi sp sp (-frame_size)
  @@ tr_seq env env_typ (-4) df.body
  @@ label (df.fname.id ^ "_ret")
  @@ addi sp sp frame_size @@ lw ra (-4)(fp) @@ lw fp 0(fp) @@ jr ra

let builtins =
  label "_print_bool"
  @@ beqz a0 "_pb_false"
  @@ la a0 "_str_true"
  @@ li v0 4 @@ syscall @@ jr ra
  @@ label "_pb_false"
  @@ la a0 "_str_false"
  @@ li v0 4 @@ syscall @@ jr ra
  
  @@ label "_print_ptr"
  @@ beqz a0 "_pp_nil"
  @@ li v0 1 @@ syscall @@ jr ra
  @@ label "_pp_nil"
  @@ la a0 "_str_nil"
  @@ li v0 4 @@ syscall @@ jr ra

  @@ label "_strcmp"
  @@ move t2 a0
  @@ move t3 a1
  @@ label "_strcmp_loop"
  @@ lbu t4 0 t2
  @@ lbu t5 0 t3
  @@ bne t4 t5 "_strcmp_diff"
  @@ beqz t4 "_strcmp_eq"
  @@ addi t2 t2 1
  @@ addi t3 t3 1
  @@ b "_strcmp_loop"
  @@ label "_strcmp_eq"
  @@ li v0 1
  @@ jr ra
  @@ label "_strcmp_diff"
  @@ li v0 0
  @@ jr ra

let tr_prog decls =
  struct_env := StrMap.empty;
  fun_return_env := StrMap.empty;
  strings := [];
  collect_info decls;
  
  let l_true = new_string "true" in
  let l_false = new_string "false" in
  let l_nil = new_string "<nil>" in
  
  let text = 
    List.fold_left (fun acc -> function
      | Fun f -> acc @@ tr_fun f
      | _ -> acc
    ) (label "__start" @@ jal "main" @@ li v0 10 @@ syscall) decls 
    @@ builtins
  in
  
  let data = List.fold_left (fun acc (l, s) -> 
      let label_name = if l = l_true then "_str_true" 
                       else if l = l_false then "_str_false"
                       else if l = l_nil then "_str_nil"
                       else l 
      in
      acc @@ label label_name @@ asciiz s
    ) nop !strings 
  in
  
  { text; data }
