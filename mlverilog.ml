open Lex_and_parse
open Four_state

let printf      = Printf.printf
let lmap        = List.map
let lconcat     = List.concat
let lsort       = List.sort
let lfilter     = List.filter
let lfilter_map = List.filter_map
let hd          = List.hd
let sconcat     = String.concat
let hadd        = Hashtbl.add
let hfind       = Hashtbl.find
let hreplace    = Hashtbl.replace

exception NoEval of string
exception NoWay
exception Not_declared of string
exception NonConstExpr
exception NotImplemented of string
exception Yield
exception Done
exception StackUnderflow
exception IllegalScalarIndex
exception ReversedRange

type trig_edge = PosEdge | NegEdge | AnyEdge

type event_trigger = Trigger of int * trig_edge * expr

type process_env = { kinds  : (string, decl_kind ) Hashtbl.t ;
                     ranges : (string, ioreg_decl) Hashtbl.t ;
                     values : (string, veri_value) Hashtbl.t ;
                     hooks  : (string, event_trigger list) Hashtbl.t   ;
             mutable unblk  : int list ;
             mutable time   : int }

let hvalue env s   = try hfind env.values s with Not_found -> raise (Not_declared s)
let hreplace env s v = try hreplace env.values s v with Not_found -> raise (Not_declared s)

let rec eval_expr env = function
  | E_Unbased  _    as u -> fst_from_literal u
  | E_Based_H (_,_) as u -> fst_from_literal u
  | E_Based_D (_,_) as u -> fst_from_literal u
  | E_Based_O (_,_) as u -> fst_from_literal u
  | E_Based_B (_,_) as u -> fst_from_literal u
  | E_String s       -> V_String s
  | E_Variable s     -> ( try hfind env.values s with
                          Not_found -> raise (Not_declared s) )
  | E_Plus   (a,b) -> fst_plus   (eval_expr env a) (eval_expr env b)
  | E_Minus  (a,b) -> fst_minus  (eval_expr env a) (eval_expr env b)
  | E_Mul    (a,b) -> fst_mul    (eval_expr env a) (eval_expr env b)
  | E_BinAnd (a,b) -> fst_binand (eval_expr env a) (eval_expr env b)
  | E_BinOr  (a,b) -> fst_binor  (eval_expr env a) (eval_expr env b)
  | E_EqEq   (a,b) -> fst_eqeq   (eval_expr env a) (eval_expr env b)
  | E_BinXor (a,b) -> fst_binxor (eval_expr env a) (eval_expr env b)
  | E_Unary (Uop_Inv, a) -> fst_uninv (eval_expr env a)
  | E_Unary (Uop_Or,  a) -> fst_unor  (eval_expr env a)
  | E_Unary (Uop_Xor, a) -> fst_unxor (eval_expr env a)
  | E_Unary (Uop_And, a) -> fst_unand (eval_expr env a)
  | _ -> raise (NoEval "Unsupported expr :(")

(* overrides a single variable, used for edge detection *)
let rec eval_expr_with env var v = function
  | E_Variable s     -> ( if s = var then v else
                          try hfind env.values s with
                          Not_found -> raise (Not_declared s) )
  | E_Plus   (a,b) -> fst_plus   (eval_expr_with env var v a) (eval_expr_with env var v b)
  | E_Minus  (a,b) -> fst_minus  (eval_expr_with env var v a) (eval_expr_with env var v b)
  | E_Mul    (a,b) -> fst_mul    (eval_expr_with env var v a) (eval_expr_with env var v b)
  | E_BinAnd (a,b) -> fst_binand (eval_expr_with env var v a) (eval_expr_with env var v b)
  | E_BinOr  (a,b) -> fst_binor  (eval_expr_with env var v a) (eval_expr_with env var v b)
  | E_EqEq   (a,b) -> fst_eqeq   (eval_expr_with env var v a) (eval_expr_with env var v b)
  | E_BinXor (a,b) -> fst_binxor (eval_expr_with env var v a) (eval_expr_with env var v b)
  | E_Unary (Uop_Inv, a) -> fst_uninv (eval_expr_with env var v a)
  | E_Unary (Uop_Or,  a) -> fst_unor  (eval_expr_with env var v a)
  | E_Unary (Uop_Xor, a) -> fst_unxor (eval_expr_with env var v a)
  | E_Unary (Uop_And, a) -> fst_unand (eval_expr_with env var v a)
  | other -> eval_expr env other

let eval_builtin s lst = match s with
  | "display" -> print_endline (sconcat "" (List.map fst_display lst))
  | "finish"  -> print_endline "Finish called"
  | e -> raise (NoEval ("Unsupported builtin: " ^ e))

let print_symtable env =
    let printer s f = printf "%s -> %s\n" s (fst_display f)
    in Hashtbl.iter printer env.values

let rec eval_stmt env = function
   | S_Builtin (s, lst) -> eval_builtin s (lmap (fun x -> eval_expr env x) lst)
   | S_Seq_Block lst    -> List.iter (fun x -> eval_stmt env x) lst
   | S_Blk_Assign (E_Variable v, ex) -> () (* FIXME *)
   | _ -> raise (NoEval "Unsupported stmt :(")

let eval_constexpr = function (*FIXME: can easily allow more*)
   | E_Unbased  _    as u -> fst_from_literal u
   | E_Based_H (_,_) as u -> fst_from_literal u
   | E_Based_D (_,_) as u -> fst_from_literal u
   | E_Based_O (_,_) as u -> fst_from_literal u
   | E_Based_B (_,_) as u -> fst_from_literal u
   | _ -> raise NonConstExpr

let rec eval_constexpr_int = function
   | E_Unbased i -> int_of_string i
   | E_Based_H _ as i -> fst_val (fst_from_literal i)
   | E_Based_D _ as i -> fst_val (fst_from_literal i)
   | E_Based_O _ as i -> fst_val (fst_from_literal i)
   | E_Based_B _ as i -> fst_val (fst_from_literal i)
   | E_Plus  (e1, e2) -> (eval_constexpr_int e1) + (eval_constexpr_int e2)
   | E_Minus (e1, e2) -> (eval_constexpr_int e1) - (eval_constexpr_int e2)
   | E_Mul   (e1, e2) -> (eval_constexpr_int e1) * (eval_constexpr_int e2)
   | _ -> raise NonConstExpr

let sizeof env s = match hfind env.ranges s with
                   | Single -> 1
                   | Range (e1, e2) -> let msb = eval_constexpr_int e1 in
                                       let lsb = eval_constexpr_int e2 in
                                       if(msb >= lsb) then msb - lsb + 1
                                       else                lsb - msb + 1

let norm_idx env s i = match hfind env.ranges s with
                   | Single -> raise IllegalScalarIndex
                   | Range (e1,e2) -> let msb = eval_constexpr_int e1 in
                                      let lsb = eval_constexpr_int e2 in
                                      if(msb >= lsb) then i - lsb
                                      else                lsb - i

let all_zs = function
    | Range (e1, e2) -> let msb = match eval_constexpr e1 with
                            | V_FourState (_,_,msb) -> msb
                            | V_String _ -> raise NonConstExpr in
                        let lsb = match eval_constexpr e2 with
                            | V_FourState (_,_,lsb) -> lsb
                            | V_String _ -> raise NonConstExpr in
                        let w   = msb - lsb + 1  in V_FourState (w,0,0)
    | Single -> V_FourState (1,0,-1)

let mk_symtable = { kinds  = Hashtbl.create 100;
                    ranges = Hashtbl.create 100;
                    values = Hashtbl.create 100;
                    hooks  = Hashtbl.create 100;
                    unblk  = [];
                    time   = 0 }

let populate_symtable env (Module (_,_,ents)) =
    let decls = lfilter_map (function Decl (k,r,lst) -> Some (k,r,lst) | _ -> None) ents in
    let add_multiple (k,r,lst) = List.iter (fun s -> hadd env.kinds  s k;
                                                     hadd env.ranges s r;
                                                     hadd env.values s (all_zs r);) lst in
    List.iter (fun d -> add_multiple d) decls

let rec expr_deps = function
    | E_Plus  (e1, e2) -> (expr_deps e1) @ (expr_deps e2)
    | E_Minus (e1, e2) -> (expr_deps e1) @ (expr_deps e2)
    | E_Mul   (e1, e2) -> (expr_deps e1) @ (expr_deps e2)
    | E_Variable s     -> [s]
    | E_String  _      -> []
    | E_Unbased _      -> []
    | E_Builtin _      -> []
    | E_Based_H _      -> []
    | E_Based_D _      -> []
    | E_Based_O _      -> []
    | E_Based_B _      -> []
    | E_Range (e1,e2,e3) -> (expr_deps e1) @ (expr_deps e2) @ (expr_deps e3)
    | E_Index   (e1, e2) -> (expr_deps e1) @ (expr_deps e2)
    | E_Concat  (el)     -> List.concat (lmap expr_deps el)
    | E_Unary   (_, e)   -> (expr_deps e)
    | E_BinAnd  (e1, e2) -> (expr_deps e1) @ (expr_deps e2)
    | E_BinOr   (e1, e2) -> (expr_deps e1) @ (expr_deps e2)
    | E_BinXor  (e1, e2) -> (expr_deps e1) @ (expr_deps e2)
    | E_EqEq    (e1, e2) -> (expr_deps e1) @ (expr_deps e2)

let event_deps el = let f = function | Posedge e -> expr_deps e
                                     | Negedge e -> expr_deps e
                                     | Level   e -> expr_deps e
                    in List.concat (lmap f el)

type instr = I_Read of string
           | I_Write of string
           | I_Literal of veri_value
           | I_Delay of int
           | I_Event of event list
           | I_Clear of string list
           | I_Plus
           | I_Minus
           | I_Mul
           | I_BinAnd
           | I_BinOr
           | I_BinXor
           | I_EqEq
           | I_UnInv
           | I_UnOr
           | I_UnAnd
           | I_UnXor
           | I_Index of string
           | I_Range of string * int * int
           | I_Concat of int
           | I_Builtin of string * int
           | I_Restart
           | I_Halt
           | I_Finish
           | I_Time
           | I_Resize of string

type process_status = Stts_Ready        |
                      Stts_Finish       |
                      Stts_Halt         |
                      Stts_Delay of int |
                      Stts_Event of event list

type process_state = { mutable status : process_status;
                       mutable pc     : int;
                       mutable dstack : veri_value list;
                               instrs : instr array ;
                               pid    : int }

let init_pstate p ii = {  status = Stts_Ready;
                          pc = 0;
                          dstack = [];
                          instrs = Array.of_list ii;
                          pid = p }

let push_dstk v pstate = pstate.dstack <- v::pstate.dstack
let pop_dstk pstate = try (let v = hd pstate.dstack in
                           pstate.dstack <- List.tl pstate.dstack; v )
                      with Failure _ -> raise StackUnderflow
let pop_dstk_n n pstate =
    let rec iter acc n = if n = 0 then acc
                         else iter (pop_dstk pstate::acc) (n-1) in iter [] n
let pc_incr pstate = pstate.pc <- pstate.pc+1

let rec compile_expr expr =
   let uop     a inst = (compile_expr a) @ [inst] in
   let binop a b inst = (compile_expr a) @
                        (compile_expr b) @ [inst] in match expr with
  | E_Unbased  _    as u -> [I_Literal (fst_from_literal u)]
  | E_Based_H (_,_) as u -> [I_Literal (fst_from_literal u)]
  | E_Based_D (_,_) as u -> [I_Literal (fst_from_literal u)]
  | E_Based_O (_,_) as u -> [I_Literal (fst_from_literal u)]
  | E_Based_B (_,_) as u -> [I_Literal (fst_from_literal u)]
  | E_String s           -> [I_Literal (V_String s) ]
  | E_Variable s         -> [I_Read s]
  | E_Plus   (a,b)       -> binop a b I_Plus
  | E_Minus  (a,b)       -> binop a b I_Minus
  | E_Mul    (a,b)       -> binop a b I_Mul
  | E_BinAnd (a,b)       -> binop a b I_BinAnd
  | E_BinOr  (a,b)       -> binop a b I_BinOr
  | E_BinXor (a,b)       -> binop a b I_BinXor
  | E_EqEq   (a,b)       -> binop a b I_EqEq
  | E_Concat lst         -> (lconcat (lmap compile_expr lst)) @ [I_Concat (List.length lst)]
  | E_Unary (Uop_Inv, a) -> uop a I_UnInv
  | E_Unary (Uop_Or,  a) -> uop a I_UnOr
  | E_Unary (Uop_Xor, a) -> uop a I_UnXor
  | E_Unary (Uop_And, a) -> uop a I_UnAnd
  | E_Index (E_Variable a, e) -> (compile_expr e) @ [I_Index a ]
  | E_Index _            -> raise (NotImplemented "Index must be variable based")
  | E_Range (E_Variable s, e1, e2) ->
                         let msb = eval_constexpr_int e1 in
                         let lsb = eval_constexpr_int e2 in [I_Range (s,msb,lsb) ]
  | E_Range _            -> raise (NotImplemented "Range must be variable based")
  | E_Builtin "time"     -> [I_Time]
  | E_Builtin _          -> raise (NotImplemented "Only $time is allowed in expressions")


let rec compile_stmt = function
   | S_Builtin ("finish", _) -> [I_Finish]
   | S_Builtin ("time",   _) -> [I_Time]
   | S_Builtin (s, lst) -> (List.concat (lmap compile_expr lst)) @ [I_Builtin (s,List.length lst)]
   | S_Seq_Block lst    ->  List.concat (lmap compile_stmt lst)
   | S_Blk_Assign (E_Variable v, ex) -> (compile_expr ex) @ [I_Resize v; I_Write v]
   | S_Delay (d, stmt) -> [I_Delay d] @ (compile_stmt stmt)
   | S_EvControl (el, stmt) -> [I_Event el; I_Clear (event_deps el)] @ (compile_stmt stmt)
   | _ -> raise (NoEval "Unsupported stmt for compilation :(")

let check_hook env var old_val (Trigger(_, edg, exp))
    = let new_val = eval_expr      env             exp in
      let old_val = eval_expr_with env var old_val exp in
      printf "%s -> %s\n" (fst_display old_val) (fst_display new_val) ;
      match edg with
      | AnyEdge -> new_val != old_val
      | PosEdge -> fst_posedge old_val new_val
      | NegEdge -> fst_negedge old_val new_val

let add_hook env s hooks = match Hashtbl.find_opt env.hooks s with
                | Some lst -> Hashtbl.replace env.hooks s (hooks @ lst)
                | None     -> Hashtbl.add env.hooks s hooks

let make_triggers pid el = let trg =
    function Posedge e -> Trigger (pid, PosEdge, e)
           | Negedge e -> Trigger (pid, NegEdge, e)
           | Level   e -> Trigger (pid, AnyEdge, e) in lmap trg el

let unblock_theads env pid s old_val =
    let f_s = function Trigger (p,_,_) -> string_of_int p in
    let f_i = function Trigger (p,_,_) -> p in
        match Hashtbl.find_opt env.hooks s with
        | None     -> ()
        | Some lst -> let unblk_lst = lfilter (fun h -> check_hook env s old_val h) lst in
                      printf "PID %d: unblocking %s\n" pid (sconcat ", " (lmap f_s unblk_lst));
                      env.unblk <- (lmap f_i unblk_lst) @ env.unblk

let clear_hook env pid lst =
    let not_l = function Trigger (p,_,_) -> p != pid in
    let iter s = ( match Hashtbl.find_opt env.hooks s with
                    | Some lst -> Hashtbl.replace env.hooks s (lfilter not_l lst)
                    | None -> () ) in
    List.iter iter lst

let run_instr env ps =
    let inst     = ps.instrs.(ps.pc)  in
    let binop fn = ( let a = pop_dstk ps in
                     let b = pop_dstk ps in
                     push_dstk (fn a b) ps; pc_incr ps ) in
    let uop   fn = ( let a = pop_dstk ps in
                     push_dstk (fn a) ps; pc_incr ps )   in

    match inst with
    | I_Read  s       -> push_dstk (hvalue env s) ps; pc_incr ps;
    | I_Write s       -> let tos  = pop_dstk ps in
                         let prev = hvalue env s in
                         printf "PID %d: writing %d to %s\n" ps.pid (fst_val tos) s ;
                         hreplace env s tos;
                         unblock_theads env ps.pid s prev; (* this does posedge/negedge/expr checks *)
                         pc_incr ps
    | I_Resize s      -> let tos = pop_dstk ps     in
                         let sz  = sizeof env s in
                         push_dstk (fst_resize tos sz) ps;
                         pc_incr ps;
    | I_Delay d       -> pc_incr ps;
                         ps.status <- Stts_Delay (d + env.time);
                         raise Yield
    | I_Event el      -> let add_hooks_here = event_deps el     in
                         let triggers = make_triggers ps.pid el in
                         printf "PID %d: adding hooks to: %s\n" ps.pid (String.concat "," add_hooks_here);
                         List.iter (fun s -> add_hook env s triggers) add_hooks_here ;
                         pc_incr ps;
                         ps.status <- Stts_Event el;
                         raise Yield
    | I_Clear el_s    -> clear_hook env ps.pid el_s; pc_incr ps
    | I_Concat n      -> let lst = ref [] in
                         for i = 1 to n do
                            lst := (pop_dstk ps) :: !lst
                         done;
                         push_dstk (fst_concat_lst !lst) ps;
                         pc_incr ps
    | I_Literal v     -> push_dstk v ps; pc_incr ps
    | I_EqEq          -> binop fst_eqeq
    | I_Plus          -> binop fst_plus
    | I_Minus         -> binop fst_minus
    | I_Mul           -> binop fst_mul
    | I_BinAnd        -> binop fst_binand
    | I_BinOr         -> binop fst_binor
    | I_BinXor        -> binop fst_binxor
    | I_UnInv         -> uop   fst_uninv
    | I_UnOr          -> uop   fst_unor
    | I_UnAnd         -> uop   fst_unand
    | I_UnXor         -> uop   fst_unxor
    | I_Index s       -> ( match hfind env.ranges s with
                            | Single -> raise IllegalScalarIndex
                            | Range (e1,e2) -> 
                                let msb = eval_constexpr_int e1 in
                                let lsb = eval_constexpr_int e2 in
                                let v   = hfind env.values s    in
                                let i   = pop_dstk ps           in
                                let ret = fst_idx v msb lsb i   in
                                push_dstk ret ps;
                                pc_incr       ps )
    | I_Range (s,m,l) -> ( match hfind env.ranges s with
                            | Single -> raise IllegalScalarIndex 
                            | Range (e1,e2) ->   
                                let msb = eval_constexpr_int e1 in
                                let lsb = eval_constexpr_int e2 in
                                let v   = hfind env.values s in
                                let ret = fst_rng v msb lsb m l in
                                if msb >= lsb && m < l then raise ReversedRange;
                                if msb <  lsb && m > l then raise ReversedRange;
                                push_dstk ret ps;
                                pc_incr       ps )
    | I_Builtin (s,l) -> let lst = pop_dstk_n l ps in eval_builtin s lst ; pc_incr ps
    | I_Restart       -> ps.pc <- 0 ; ps.dstack <- []
    | I_Halt          -> ps.status <- Stts_Halt; raise Yield
    | I_Finish        -> ps.status <- Stts_Finish; raise Yield
    | I_Time          -> push_dstk (V_FourState (Sys.int_size, -1, env.time)) ps ; pc_incr ps

let compile_process = function
    | Always  s -> (compile_stmt s) @ [I_Restart]
    | Initial s -> (compile_stmt s) @ [I_Halt]
    | Assign  (E_Variable v, expr) -> let deps = expr_deps expr in
                                      let el   = lmap (fun s -> Level (E_Variable s)) deps in
                                      (compile_expr expr) @ 
                                      [ I_Resize v; 
                                        I_Write  v; 
                                        I_Event el;
                                        I_Clear (event_deps el); 
                                        I_Restart ]
    | Assign _  -> raise (NotImplemented "Involved lvalues are not supported")
    | _         -> raise NoWay

let make_process_tab = function Module (_, _, ents) ->

       let f_ents = lfilter (function
                                    | Always _ | Initial _ | Assign _ -> true
                                    | _ -> false ) ents in
       let pids = List.init (List.length f_ents) (fun f -> f+1) in
       let prcs = lmap  compile_process f_ents                  in
       List.map2 (fun n bc -> init_pstate n bc) pids prcs

let run_process env ps = try while true do run_instr env ps done with Yield -> ()

let run_tab p = lmap run_process p

let run_eligible env ps_tab =
    let f = function {status = Stts_Ready; _} -> true | _ -> false in
    List.iter (fun ps -> run_process env ps) (lfilter f ps_tab)

let rec have_eligible = function
   | {status = Stts_Ready; _} :: rst -> true
   | _ :: rst -> have_eligible rst
   | [] -> false

let rec finish_seen = function
   | {status = Stts_Finish; _} :: rst -> true
   | _ :: rst -> finish_seen rst
   | [] -> false

let next_time n ps_tab = 
    let f = function {status = Stts_Delay n; _} -> Some n | _ -> None in
    match (lsort compare (lfilter_map f ps_tab)) with
    | x::_ -> x
    | []   -> n    

let wake_time n ps_tab = 
    let f = function {status = Stts_Delay d; _} as p -> if d = n then p.status <- Stts_Ready  
                    | _ -> () in
    List.iter f ps_tab

let wake_unblock ps_tab lst = (*FIXME: Lame, dude *)
    List.iter (fun pid -> (List.nth ps_tab (pid-1)).status <- Stts_Ready ) lst

let simulate env ps_tab =
    while true do
        if (have_eligible ps_tab) then (
            run_eligible env ps_tab; (* updates unblock list via hooks *)
            wake_unblock ps_tab env.unblk;
            env.unblk <- [];
            if (finish_seen ps_tab) then (
                printf "Finish seen at %d\n" env.time ; 
                raise Done
            )
        ) else (
            let nn = next_time env.time ps_tab in 
            env.time <- nn;
            printf "Advancing time to %d\n" env.time ;
            wake_time nn ps_tab
        )
    done

let main path = let m      = fst ( parse_module ( tokenize path ) ) in
                let env    = mk_symtable in
                let ps_tab = make_process_tab m in
                populate_symtable env m;
                simulate env ps_tab ;;

main Sys.argv.(1) ;;