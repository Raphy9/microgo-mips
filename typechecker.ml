open Mgoast
open Lexing

exception Error of location * string
let error loc s = raise (Error (loc, s))

let warning loc msg =
  let (start_p, end_p) = loc in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\nWarning: %s\n"
    start_p.pos_fname start_p.pos_lnum 
    (start_p.pos_cnum - start_p.pos_bol) 
    (end_p.pos_cnum - start_p.pos_bol) 
    msg

let type_error loc ty_actual ty_expected =
  error loc (Printf.sprintf "expected %s, got %s"
                (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)

type tenv = (typ * bool ref) Env.t
type fenv = (typ list * typ list) Env.t
type senv = (ident * typ) list Env.t

let dummy = "_"

let add_env_track l tenv =
  List.fold_left (fun (env, refs) (x, t) -> 
    if x.id = dummy then (env, refs)
    else 
      let used = ref false in
      (Env.add x.id (t, used) env, (x, used) :: refs)
  ) (tenv, []) l
let add_env_params l tenv =
  List.fold_left (fun env (x, t) -> 
    if x.id = dummy then env 
    else Env.add x.id (t, ref true) env 
  ) tenv l

let get_type_no_mark id tenv =
  try fst (Env.find id.id tenv)
  with Not_found -> error id.loc ("Unknown variable: " ^ id.id)
  
let check_typ loc t senv =
  match t with
  | TInt | TBool | TString -> ()
  | TStruct s -> 
      if not (Env.mem s senv) then 
        error loc ("Unknown structure type: " ^ s)

let check_fields fields senv =
  let rec loop seen = function
    | [] -> ()
    | (id, t) :: rest ->
        if List.mem id.id seen then error id.loc ("Duplicate field: " ^ id.id);
        check_typ id.loc t senv;
        loop (id.id :: seen) rest
  in loop [] fields

let rec check_expr e expected_typ tenv fenv senv =
  match e.edesc, expected_typ with
  | Nil, TStruct _ -> ()
  | Nil, _ -> error e.eloc "nil can only be assigned to a pointer type"
  | _, _ ->
      let inferred_typ = type_expr e tenv fenv senv in
      if inferred_typ <> expected_typ then 
        type_error e.eloc inferred_typ expected_typ

and type_expr e tenv fenv senv =
  match e.edesc with
  | Int _ -> TInt
  | Bool _ -> TBool
  | String _ -> TString
  | Nil -> error e.eloc "cannot infer type of nil without context"
  
  | Var id -> 
      (try 
         let (t, used) = Env.find id.id tenv in
         used := true; 
         t
       with Not_found -> error id.loc ("Unknown variable: " ^ id.id))

  | New s -> 
      if not (Env.mem s senv) then error e.eloc ("Unknown struct: " ^ s);
      TStruct s

  | Dot (exp, field_id) -> 
      let t_exp = type_expr exp tenv fenv senv in
      (match t_exp with
       | TStruct sname ->
           let fields = try Env.find sname senv with Not_found -> error e.eloc "Struct error" in
           let rec find_field = function
             | [] -> error field_id.loc ("Unknown field " ^ field_id.id ^ " in struct " ^ sname)
             | (f, t) :: _ when f.id = field_id.id -> t
             | _ :: rest -> find_field rest
           in find_field fields
       | _ -> error e.eloc "Dot operator requires a pointer to struct")

  | Binop (op, e1, e2) ->
      begin match op with
      | Eq | Neq ->
          if e1.edesc = Nil && e2.edesc = Nil then error e.eloc "cannot compare nil with nil"
          else if e1.edesc = Nil then (
             let t2 = type_expr e2 tenv fenv senv in
             check_expr e1 t2 tenv fenv senv; 
             TBool
          )
          else if e2.edesc = Nil then (
             let t1 = type_expr e1 tenv fenv senv in
             check_expr e2 t1 tenv fenv senv; 
             TBool
          )
          else (
             let t1 = type_expr e1 tenv fenv senv in
             let t2 = type_expr e2 tenv fenv senv in
             if t1 <> t2 then type_error e.eloc t1 t2;
             TBool
          )
      | _ ->
          let t1 = type_expr e1 tenv fenv senv in
          let t2 = type_expr e2 tenv fenv senv in
          match op with
          | Lt | Le | Gt | Ge ->
              if t1 <> TInt then type_error e1.eloc t1 TInt;
              if t2 <> TInt then type_error e2.eloc t2 TInt;
              TBool
          | Add | Sub | Mul | Div | Rem ->
              if t1 <> TInt then type_error e1.eloc t1 TInt;
              if t2 <> TInt then type_error e2.eloc t2 TInt;
              TInt
          | And | Or ->
              if t1 <> TBool then type_error e1.eloc t1 TBool;
              if t2 <> TBool then type_error e2.eloc t2 TBool;
              TBool
          | _ -> failwith "Impossible logic path"
      end

  | Unop (op, e) ->
      let t = type_expr e tenv fenv senv in
      (match op with
       | Opp -> if t <> TInt then type_error e.eloc t TInt; TInt
       | Not -> if t <> TBool then type_error e.eloc t TBool; TBool)

  | Call (f, args) ->
      let (param_types, ret_types) = 
        try Env.find f.id fenv 
        with Not_found -> error f.loc ("Unknown function: " ^ f.id) 
      in
      let args_ok = 
        if List.length args = 1 && List.length param_types > 1 then
           match args with
           | [{edesc = Call(g, g_args); eloc}] ->
               let (g_params, g_rets) = try Env.find g.id fenv with Not_found -> error g.loc "Unknown function" in
               if List.length g_args <> List.length g_params then error eloc "Wrong arg count for inner call";
               List.iter2 (fun a t -> check_expr a t tenv fenv senv) g_args g_params;
               if g_rets <> param_types then error eloc "Function return types do not match parameters";
               true
           | _ -> false
        else false
      in

      if not args_ok then (
        if List.length args <> List.length param_types then
          error e.eloc (Printf.sprintf "Function %s expects %d arguments, got %d" f.id (List.length param_types) (List.length args));
        List.iter2 (fun arg p_typ -> check_expr arg p_typ tenv fenv senv) args param_types
      );

      begin match ret_types with
      | [t] -> t
      | _ -> error e.eloc "Function call in expression context must return exactly one value"
      end

  | Print _ -> error e.eloc "fmt.Print returns no value, cannot be used as expression"

let rec check_instr i ret_expected tenv fenv senv =
  match i.idesc with
  | Expr e -> 
      (match e.edesc with
      | Print args -> 
           (match args with
            | [{edesc = Call(f, f_args); eloc}] ->
                 let (p_types, _) = try Env.find f.id fenv with Not_found -> error f.loc ("Unknown function: " ^ f.id) in
                 if List.length f_args <> List.length p_types then error eloc "Wrong arg count";
                 List.iter2 (fun a t -> check_expr a t tenv fenv senv) f_args p_types
            | _ -> 
                 List.iter (fun arg -> ignore (type_expr arg tenv fenv senv)) args)
      | Call (f, args) ->
           let (param_types, _) = try Env.find f.id fenv with Not_found -> error f.loc ("Unknown function: " ^ f.id) in
           if List.length args <> List.length param_types then error e.eloc "Wrong arg count";
           List.iter2 (fun arg p_typ -> check_expr arg p_typ tenv fenv senv) args param_types
      | _ -> 
           ignore (type_expr e tenv fenv senv))

  | Set (lhss, rhss) ->
      let check_lvalue e = 
        match e.edesc with 
        | Var id -> 
             if id.id <> dummy && not (Env.mem id.id tenv) then error id.loc ("Unknown variable: " ^ id.id)
        | Dot (e, _) -> ignore (type_expr e tenv fenv senv)
        | _ -> error e.eloc "Invalid lvalue" 
      in
      List.iter check_lvalue lhss;

      if List.length lhss = List.length rhss then
        List.iter2 (fun lhs rhs ->
          match lhs.edesc with
          | Var id when id.id = dummy -> 
              ignore (type_expr rhs tenv fenv senv)
          | Var id ->
              let t_lhs = get_type_no_mark id tenv in
              check_expr rhs t_lhs tenv fenv senv
          | _ ->
              let t_lhs = type_expr lhs tenv fenv senv in
              check_expr rhs t_lhs tenv fenv senv
        ) lhss rhss
      else if List.length rhss = 1 then
        let rhs = List.hd rhss in
        match rhs.edesc with
        | Call (f, args) ->
            let (param_types, ret_types) = try Env.find f.id fenv with Not_found -> error f.loc "Unknown function" in
            if List.length args <> List.length param_types then error rhs.eloc "Wrong arg count";
            List.iter2 (fun arg p_typ -> check_expr arg p_typ tenv fenv senv) args param_types;
            if List.length lhss <> List.length ret_types then error i.iloc "Mismatch between variables and return values";
            List.iter2 (fun lhs t_ret ->
               match lhs.edesc with
               | Var id when id.id = dummy -> ()
               | Var id -> 
                   let t_lhs = get_type_no_mark id tenv in
                   if t_lhs <> t_ret then type_error lhs.eloc t_lhs t_ret
               | _ -> 
                   let t_lhs = type_expr lhs tenv fenv senv in
                   if t_lhs <> t_ret then type_error lhs.eloc t_lhs t_ret
            ) lhss ret_types
        | _ -> error i.iloc "Assignment count mismatch"
      else
        error i.iloc "Assignment count mismatch"

  | Inc e | Dec e ->
      check_expr e TInt tenv fenv senv;
      (match e.edesc with Var _ | Dot _ -> () | _ -> error e.eloc "Lvalue required for ++/--")

  | If (cond, b1, b2) ->
      check_expr cond TBool tenv fenv senv;
      check_seq b1 ret_expected tenv fenv senv;
      check_seq b2 ret_expected tenv fenv senv

  | For (cond, b) ->
      check_expr cond TBool tenv fenv senv;
      check_seq b ret_expected tenv fenv senv
  
  | Block b -> 
      check_seq b ret_expected tenv fenv senv

  | Vars _ -> failwith "Internal error: Vars should be handled in check_seq"

  | Return exprs ->
      if List.length exprs <> List.length ret_expected then
        error i.iloc "Wrong number of return values";
      List.iter2 (fun e t_ret -> check_expr e t_ret tenv fenv senv) exprs ret_expected

and check_seq s ret tenv fenv senv =
  match s with
  | [] -> ()
  | { idesc = Vars(ids, Some t, sub_seq); _ } :: rest ->
       check_typ (List.hd ids).loc t senv;
       let vars = List.map (fun id -> (id, t)) ids in
       let (new_tenv, new_refs) = add_env_track vars tenv in
       
       check_seq sub_seq ret new_tenv fenv senv;
       check_seq rest ret new_tenv fenv senv;
       
       List.iter (fun (id, used) -> 
         if not !used && id.id <> dummy then warning id.loc ("Unused variable: " ^ id.id)
       ) new_refs

  | { idesc = Vars(ids, None, sub_seq); iloc } :: rest ->
       (match sub_seq with
        | { idesc = Set(_, rhss); _ } :: rest_sub ->
            let types = 
               if List.length rhss = 1 && List.length ids > 1 then
                 match (List.hd rhss).edesc with
                 | Call(f, _) -> 
                     let (_, ret_ts) = try Env.find f.id fenv with Not_found -> error f.loc "Unknown function" in ret_ts
                 | _ -> error iloc "Inference error"
               else
                  List.map (fun e -> type_expr e tenv fenv senv) rhss
            in
            if List.length ids <> List.length types then error iloc "Inference mismatch";
            let vars = List.map2 (fun id t -> (id, t)) ids types in
            let (new_tenv, new_refs) = add_env_track vars tenv in
            
            check_instr (List.hd sub_seq) ret new_tenv fenv senv;
            check_seq rest_sub ret new_tenv fenv senv;
            check_seq rest ret new_tenv fenv senv;
            
            List.iter (fun (id, used) -> 
               if not !used && id.id <> dummy then warning id.loc ("Unused variable: " ^ id.id)
            ) new_refs

        | _ -> error iloc "Short declaration must be followed by assignment")
  
  | i :: rest ->
       check_instr i ret tenv fenv senv;
       check_seq rest ret tenv fenv senv

let rec check_returns instrs =
  match instrs with
  | [] -> false
  | i :: rest ->
      let returns_now = match i.idesc with
        | Return _ -> true
        | If (_, b1, b2) -> 
            (check_returns b1) && (check_returns b2)
        | Block b -> check_returns b
        | For _ -> false 
        | Vars (_, _, init_seq) -> check_returns init_seq
        | _ -> false
      in
      if returns_now then true else check_returns rest

let prog (_, decls) =
  let senv = 
    List.fold_left (fun acc d -> 
      match d with 
      | Struct s -> 
          if Env.mem s.sname.id acc then error s.sname.loc ("Redefinition of struct " ^ s.sname.id);
          Env.add s.sname.id s.fields acc
      | _ -> acc
    ) Env.empty decls
  in

  let fenv =
    List.fold_left (fun acc d ->
      match d with
      | Fun f ->
          if Env.mem f.fname.id acc then error f.fname.loc ("Redefinition of function " ^ f.fname.id);
          let param_types = List.map snd f.params in
          let _ = List.fold_left (fun seen (id, _) -> 
             if List.mem id.id seen then error id.loc "Duplicate parameter" else id.id :: seen
          ) [] f.params in
          Env.add f.fname.id (param_types, f.return) acc
      | _ -> acc
    ) Env.empty decls
  in

  Env.iter (fun _ fields -> check_fields fields senv) senv;

  List.iter (function
    | Fun f ->
        List.iter (fun (_, t) -> check_typ f.fname.loc t senv) f.params;
        List.iter (fun t -> check_typ f.fname.loc t senv) f.return;
        let tenv = add_env_params f.params Env.empty in
        check_seq f.body f.return tenv fenv senv;

        if f.return <> [] then begin
          if not (check_returns f.body) then
            error f.fname.loc "Control flow may reach end of non-void function"
        end
    | _ -> ()
  ) decls;

  begin match Env.find_opt "main" fenv with
  | Some ([], []) -> ()
  | Some _ -> failwith "main function must have no arguments and no return values"
  | None -> error (Lexing.dummy_pos, Lexing.dummy_pos) "missing main function"
  end;
  
  decls