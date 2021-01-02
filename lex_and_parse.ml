let sprintf  = Printf.sprintf
let clcase   = Char.lowercase_ascii
let smake    = String.make
let lrev     = List.rev
let hd       = List.hd
let slcase   = String.lowercase_ascii


let fst = function (a, _) -> a
let snd = function (_, b) -> b
let sm_val = function (_, _, v) -> v

type token = Tk_LParen  | Tk_RParen   | Tk_LBrace  | Tk_RBrace  | Tk_LBracket | Tk_RBracket
           | Tk_Semi    | Tk_Colon    | Tk_Comma   | Tk_Hash    | Tk_Dot      | Tk_At
           | Tk_Op_Plus | Tk_Op_Minus | Tk_Op_Mul  | Tk_Op_Div  | Tk_Op_Equal | Tk_EqEq
           | Tk_BinAnd  | Tk_BinOr    | Tk_BinXor  | Tk_BinInv
           | Tk_BaseHex of string | Tk_BaseDec of string | Tk_BaseOct of string | Tk_BaseBin of string
           | Tk_Ident   of string | Tk_Literal of string | Tk_String of string | Tk_Builtin of string
           | Tk_Kw_module | Tk_Kw_endmodule | Tk_Kw_reg   | Tk_Kw_wire | Tk_Kw_posedge | Tk_Kw_negedge
           | Tk_Kw_assign | Tk_Kw_output    | Tk_Kw_input | Tk_Kw_inout | Tk_Kw_if | Tk_Kw_else
           | Tk_Kw_begin  | Tk_Kw_end | Tk_Kw_fork | Tk_Kw_join | Tk_Kw_always | Tk_Kw_initial

exception UnexpectedChar of string
exception UnexpectedEOF  of string
exception UnexpectedWTF  of string
exception NoWay
exception Done
exception NoParse of string
exception UnexpectedToken of token * string

(* --------------------------------------------------------------------------------------------- *)
(*                                           Tokenize                                            *)
(* --------------------------------------------------------------------------------------------- *)

let kw_maybe = function "module"    -> Tk_Kw_module
                       |"endmodule" -> Tk_Kw_endmodule
                       |"reg"       -> Tk_Kw_reg
                       |"wire"      -> Tk_Kw_wire
                       |"input"     -> Tk_Kw_input
                       |"output"    -> Tk_Kw_output
                       |"inout"     -> Tk_Kw_inout
                       |"assign"    -> Tk_Kw_assign
                       |"always"    -> Tk_Kw_always
                       |"initial"   -> Tk_Kw_initial
                       |"begin"     -> Tk_Kw_begin
                       |"end"       -> Tk_Kw_end
                       |"fork"      -> Tk_Kw_fork
                       |"join"      -> Tk_Kw_join
                       |"posedge"   -> Tk_Kw_posedge
                       |"negedge"   -> Tk_Kw_negedge
                       |"if"        -> Tk_Kw_if
                       |"else"      -> Tk_Kw_else
                       | s          -> Tk_Ident s

let unread file = let f_len = in_channel_length file in
                  let f_pos = pos_in file in
                  if f_pos < f_len then seek_in file (pos_in file - 1) else ()

let take_two file = let one = input_char file in
                    try let two = input_char file in (one, two)
                    with End_of_file -> (one, ' ')

let take_while cond file = let str = ref "" in
                           try while true do match input_char file with
                            | a when cond a -> str := !str ^ (smake 1 a)
                            | _ -> unread file ; raise End_of_file
                           done; !str
                           with End_of_file -> !str

let read_string_lit file =
    let str = ref "" in
    try while true do match input_char file with
    | '\n' -> raise (UnexpectedWTF ("ERROR: encountered a newline in string literal: " ^ !str))
    | '"'  -> raise Done
    | a    -> str := !str ^ (smake 1 a)
    done; !str
    with End_of_file -> raise (UnexpectedEOF "Unexpected EOF while looking for closing quote")
       | Done        -> !str

let take_until cond file =
    let str = ref "" in
    try while true do match input_char file with
     |  a when cond a -> raise Done
     |  a             -> str := !str ^ (smake 1 a)
    done; !str
    with End_of_file -> raise (UnexpectedEOF "Unexpected EOF while taking escaped string or comment")
       | Done -> !str

let eat_space file = try while true do match input_char file with
                     | ' ' -> ()
                     | _   -> unread file ; raise Done
                     done ; () with Done -> () | End_of_file -> ()

let hex_set = function '0'..'9' | 'a'..'f' | 'A'..'F' | 'z' | 'Z' | 'x' | 'X' -> true | _ -> false
let dec_set = function '0'..'9' -> true | _ -> false
let oct_set = function '0'..'7' | 'z' | 'Z' | 'x' | 'X' -> true | _ -> false
let bin_set = function '0'..'1' | 'z' | 'Z' | 'x' | 'X' -> true | _ -> false

let read_based_lit cond file =
    let str = ref "" in
    eat_space file ;
    try while true do match input_char file with
       | a when cond a -> str := !str ^ (smake 1 (clcase a))
       | '_' -> ()
       |  _  -> unread file; raise Done
    done; !str
    with Done -> !str
       | End_of_file -> !str

let tokenize fname =
    let in_f     = open_in fname in
    let line_num = ref 1 in
    let tk_list  = ref [] in
    let is_alpha = function 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false in
    let is_alphanum = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false in
    try while true do
    match take_two in_f with
        | '\'', 'h' -> let l = read_based_lit hex_set in_f
                       in tk_list := (Tk_BaseHex l,   !line_num) :: !tk_list
        | '\'', 'd' -> let l = read_based_lit dec_set in_f
                       in tk_list := (Tk_BaseDec l,   !line_num) :: !tk_list
        | '\'', 'o' -> let l = read_based_lit oct_set in_f
                       in tk_list := (Tk_BaseOct l,   !line_num) :: !tk_list
        | '\'', 'b' -> let l = read_based_lit bin_set in_f
                       in tk_list := (Tk_BaseBin l,   !line_num) :: !tk_list
        | '/' , '/' -> let _ = take_until (function '\n' -> true | _ -> false) in_f
                       in line_num := !line_num + 1
        | '=' , '=' -> tk_list := (Tk_EqEq,   !line_num) :: !tk_list
        | '\n', _ -> unread in_f ; line_num := !line_num + 1
        | ' ' , _ -> unread in_f
        | '(' , _ -> unread in_f ; tk_list := (Tk_LParen,   !line_num) :: !tk_list
        | ')' , _ -> unread in_f ; tk_list := (Tk_RParen,   !line_num) :: !tk_list
        | '[' , _ -> unread in_f ; tk_list := (Tk_LBracket, !line_num) :: !tk_list
        | ']' , _ -> unread in_f ; tk_list := (Tk_RBracket, !line_num) :: !tk_list
        | '{' , _ -> unread in_f ; tk_list := (Tk_LBrace,   !line_num) :: !tk_list
        | '}' , _ -> unread in_f ; tk_list := (Tk_RBrace,   !line_num) :: !tk_list
        | ':' , _ -> unread in_f ; tk_list := (Tk_Colon,    !line_num) :: !tk_list
        | ';' , _ -> unread in_f ; tk_list := (Tk_Semi,     !line_num) :: !tk_list
        | '.' , _ -> unread in_f ; tk_list := (Tk_Dot,      !line_num) :: !tk_list
        | '#' , _ -> unread in_f ; tk_list := (Tk_Hash,     !line_num) :: !tk_list
        | ',' , _ -> unread in_f ; tk_list := (Tk_Comma,    !line_num) :: !tk_list
        | '=' , _ -> unread in_f ; tk_list := (Tk_Op_Equal, !line_num) :: !tk_list
        | '*' , _ -> unread in_f ; tk_list := (Tk_Op_Mul,   !line_num) :: !tk_list
        | '+' , _ -> unread in_f ; tk_list := (Tk_Op_Plus,  !line_num) :: !tk_list
        | '-' , _ -> unread in_f ; tk_list := (Tk_Op_Minus, !line_num) :: !tk_list
        | '&' , _ -> unread in_f ; tk_list := (Tk_BinAnd,   !line_num) :: !tk_list
        | '|' , _ -> unread in_f ; tk_list := (Tk_BinOr,    !line_num) :: !tk_list
        | '^' , _ -> unread in_f ; tk_list := (Tk_BinXor,   !line_num) :: !tk_list
        | '~' , _ -> unread in_f ; tk_list := (Tk_BinInv,   !line_num) :: !tk_list
        | '@' , _ -> unread in_f ; tk_list := (Tk_At,       !line_num) :: !tk_list
        | '$' , _ -> unread in_f ; let s = take_while is_alphanum in_f
                                   in tk_list := (Tk_Builtin s, !line_num) :: !tk_list
        | '\\', _ -> unread in_f ; unread in_f ;
                                let s = take_until (function ' ' -> true | _ -> false) in_f
                                in tk_list := (Tk_Ident s, !line_num) :: !tk_list
        |  a  , _ when dec_set a -> unread in_f ; unread in_f ;
                                let s = read_based_lit dec_set in_f
                                in tk_list := (Tk_Literal s, !line_num) :: !tk_list
        | '"' , _ -> unread in_f ;
                                let s = read_string_lit in_f
                                in tk_list := (Tk_String s, !line_num) :: !tk_list
        |  a  , _ when is_alpha a -> unread in_f; unread in_f ;
                                let s = take_while is_alphanum in_f
                                in  tk_list := (kw_maybe s, !line_num) :: !tk_list
        |  e  , _ -> raise (UnexpectedChar (sprintf "ERROR: Unexpected char %c at line %d\n" e !line_num ))
    done ; !tk_list
    with End_of_file -> close_in in_f ; lrev !tk_list

type unary_op = Uop_And | Uop_Or | Uop_Xor | Uop_Inv

let is_unary  = function Tk_BinAnd | Tk_BinOr | Tk_BinXor | Tk_BinInv -> true | _ -> false
let unary_map = function Tk_BinAnd -> Uop_And
                       | Tk_BinOr  -> Uop_Or
                       | Tk_BinInv -> Uop_Inv
                       | Tk_BinXor -> Uop_Xor
                       | _ -> raise NoWay

type expr = E_Plus  of expr * expr
          | E_Minus of expr * expr
          | E_Mul of expr * expr
          | E_Variable of string
          | E_String  of string
          | E_Unbased of string
          | E_Builtin of string
          | E_Based_H of int * string
          | E_Based_D of int * string
          | E_Based_O of int * string
          | E_Based_B of int * string
          | E_Range   of expr * expr * expr
          | E_Index   of expr * expr
          | E_Concat  of expr list
          | E_Unary   of unary_op * expr
          | E_EqEq    of expr * expr
          | E_BinAnd  of expr * expr
          | E_BinOr   of expr * expr
          | E_BinXor  of expr * expr

type portmap = Portmap of string * expr

type ioreg_decl = Range of expr * expr | Single

type event = Posedge of expr | Negedge of expr | Level of expr

type stmt = S_Blk_Assign of expr * expr
          | S_Nblk_Assign of expr * expr
          | S_Delay of int * stmt
          | S_EvControl of event list * stmt
          | S_Seq_Block of stmt list
          | S_Par_Block of stmt list
          | S_If of expr*stmt
          | S_If_Else of expr*stmt*stmt
          | S_Builtin of string*expr list

type decl_kind  = Wire | Reg | Input | Output | Inout

type module_ent = Decl   of decl_kind * ioreg_decl * string list
                | Inst of string * string * portmap list * portmap list
                | Assign of expr * expr
                | Always of stmt
                | Initial of stmt

type module_def = Module of string * string list * module_ent list


let parse_error str = function
    | (q, n)::rst -> sprintf "Expected %s, at line %d" str n
    | []          -> sprintf "Expected %s, got empty list" str

let expect (tk:token) = function
    | (t,_)::rst when t == tk -> (), rst
    | e -> raise (NoParse (parse_error "TBD" e)) (*FIXME*)

let ident = function
    | (Tk_Ident s,_)::rst -> s, rst
    | e -> raise (NoParse (parse_error "identifier" e))

let rec parse_delimited acc parser delim tkns =
    let item, rst = parser tkns in match rst with
    | (d,_)::rst when d == delim -> parse_delimited (item::acc) parser delim rst
    | _ -> lrev (item::acc),rst

(* --------------------------------------------------------------------------------------------- *)
(*                                         Parse Expr                                            *)
(* --------------------------------------------------------------------------------------------- *)

let rec parse_primary =
    let prepare s = slcase (List.fold_left (fun a b -> a ^ b) "" (String.split_on_char '_' s)) in
    function
    | (Tk_Ident s,_)::rst -> E_Variable s, rst
    | (Tk_String s,_)::rst -> E_String s, rst
    | (Tk_Literal s,_)::(Tk_BaseHex l,_)::rst -> E_Based_H (int_of_string s, prepare l), rst
    | (Tk_Literal s,_)::(Tk_BaseDec l,_)::rst -> E_Based_D (int_of_string s, prepare l), rst
    | (Tk_Literal s,_)::(Tk_BaseOct l,_)::rst -> E_Based_O (int_of_string s, prepare l), rst
    | (Tk_Literal s,_)::(Tk_BaseBin l,_)::rst -> E_Based_B (int_of_string s, prepare l), rst
    | (Tk_Literal s,_)::rst -> E_Unbased s, rst
    | (Tk_Builtin s,_)::rst -> E_Builtin s, rst
    | (Tk_LParen,_)::rst -> let e, rst = parse_expr rst       in
                            let _, rst = expect Tk_RParen rst in e, rst
    | e -> raise (NoParse (parse_error "literal or identifier" e))

and parse_index tkns = let v_or_l, rst = parse_primary tkns in match rst with
    | (Tk_LBracket,_)::rst -> let e1, rst = parse_expr rst in
                        (match rst with
                            | (Tk_Colon,_)::rst -> let e2, rst = parse_expr rst in
                                                   let _,  rst = expect Tk_RBracket rst in
                                                   E_Range (v_or_l,e1,e2), rst
                            | (Tk_RBracket,_)::rst  -> E_Index (v_or_l,e1), rst
                            | e -> raise (NoParse (parse_error "colon or rbracket" e))
                        )
    | _ -> v_or_l, rst

and parse_unary = function
    | (u,_)::rst when is_unary u -> let idx,rst = parse_index rst in
                                    E_Unary (unary_map u, idx), rst
    | rst -> parse_index rst

and parse_concat = function
    |(Tk_LBrace,_)::rst -> let e_lst, rst = parse_delimited [] parse_expr Tk_Comma rst in
                           let _, rst = expect Tk_RBrace rst in E_Concat e_lst, rst
    | tkns -> parse_unary tkns

and parse_factor tkns = let f1, rst1 = parse_concat tkns in match rst1 with
         | (Tk_Op_Mul,_)::rst1 -> let f2, rst = parse_factor rst1 in E_Mul (f1, f2), rst
         | _ -> f1, rst1

and parse_sum tkns =
        let rec parse_sum_tick acc = (function
             | (Tk_Op_Plus,_)::rst  -> let t, rst = parse_factor rst in
                                      parse_sum_tick ((true, t)::acc) rst
             | (Tk_Op_Minus,_)::rst -> let t, rst = parse_factor rst in
                                      parse_sum_tick ((false, t)::acc) rst
             | rst -> acc, rst) in
        let rec collect t = (function (true,  x)::xs -> E_Plus  (collect t xs, x)
                                    | (false, x)::xs -> E_Minus (collect t xs, x)
                                    | [] -> t ) in
        let t,  rst = parse_factor tkns in
        let lt, rst = parse_sum_tick [] rst in collect t lt, rst

and parse_eq tkns = let t1, rst1 = parse_sum tkns in match rst1 with
         | (Tk_EqEq,_)::rst1 -> let t2, rst = parse_eq rst1 in E_EqEq (t1, t2), rst
         | _ -> t1, rst1

and parse_and tkns = let t1, rst1 = parse_eq tkns in match rst1 with
         | (Tk_BinAnd,_)::rst1 -> let t2, rst = parse_and rst1 in E_BinAnd (t1, t2), rst
         | _ -> t1, rst1

and parse_xor tkns = let t1, rst1 = parse_and tkns in match rst1 with
         | (Tk_BinXor,_)::rst1 -> let t2, rst = parse_xor rst1 in E_BinXor (t1, t2), rst
         | _ -> t1, rst1

and parse_or  tkns = let t1, rst1 = parse_xor tkns in match rst1 with
         | (Tk_BinOr,_)::rst1 -> let t2, rst = parse_or rst1 in E_BinOr (t1, t2), rst
         | _ -> t1, rst1

and parse_expr tkns = parse_or tkns

(* --------------------------------------------------------------------------------------------- *)
(*                                         Parse Module                                          *)
(* --------------------------------------------------------------------------------------------- *)

let parse_conn tkns = let _,rst = expect Tk_Dot tkns   in
                      let p,rst = ident rst            in
                      let _,rst = expect Tk_LParen rst in
                      let e,rst = parse_expr rst       in
                      let _,rst = expect Tk_RParen rst in Portmap (p,e), rst

let rec parse_conn_list acc tkn =
    let c,rst = parse_conn tkn in match rst with
       | (Tk_Comma,_)::rst -> parse_conn_list (c::acc) rst
       | (Tk_RParen,_)::rst -> lrev (c::acc), rst
       | e -> raise (NoParse (parse_error "comma or rparen" e))

let rec parse_module_inst = function
  | (Tk_Ident mt,_)::(Tk_Ident mn,_)::(Tk_LParen,_)::rst
   -> let cl,rst = parse_conn_list [] rst in
      let _, rst = expect Tk_Semi rst in (Inst (mt, mn, [], cl), rst)
  | (Tk_Ident mt,_)::(Tk_Hash,_)::rst
   -> let _, rst = expect Tk_LParen rst in
      let pl,rst = parse_conn_list [] rst in
      let mn,rst = ident rst in
      let _, rst = expect Tk_LParen rst in
      let cl,rst = parse_conn_list [] rst in
      let _, rst = expect Tk_Semi rst in (Inst (mt, mn, pl, cl), rst)
  | e -> raise (NoParse (parse_error "module instance" e))

let rec parse_port_lst acc = function
  | (Tk_Ident s,_)::(Tk_Comma,_)::rst  -> parse_port_lst (s::acc) rst
  | (Tk_Ident s,_)::(Tk_RParen,_)::(Tk_Semi,_)::rst -> (lrev (s::acc),rst)
  | e -> raise (NoParse (parse_error "port ident" e))

let rec parse_ident_list acc = function
       | (Tk_Ident i,_)::(Tk_Semi,_) ::rst -> lrev (i::acc), rst
       | (Tk_Ident i,_)::(Tk_Comma,_)::rst -> parse_ident_list (i::acc) rst
       | e -> raise (NoParse (parse_error "identifier" e))

let parse_ioreg_decl tkns = match tkns with
    | ((Tk_LBracket,_)::rst) as i -> let _,  rst = expect Tk_LBracket i    in
                                     let e1, rst = parse_expr rst          in
                                     let _,  rst = expect Tk_Colon rst     in
                                     let e2, rst = parse_expr rst          in
                                     let _,  rst = expect Tk_RBracket rst  in
                                     let il, rst = parse_ident_list [] rst in Range (e1, e2), il, rst
    | ((Tk_Ident _,_)::rst) as i  -> let il, rst = parse_ident_list [] i   in Single, il, rst
    | e -> raise (NoParse (parse_error "wire/reg/input/output decl" e))

let parse_assignment tkns = let lvalue, rst = parse_primary tkns         in (* FIXME: lvalue syntax *)
                            let _,      rst = expect Tk_Op_Equal rst     in
                            let expr,   rst = parse_expr rst             in
                            let _,      rst = expect Tk_Semi rst         in Assign (lvalue, expr), rst

let parse_blk_assignment tkns = let lvalue, rst = parse_primary tkns     in (* FIXME: lvalue syntax *)
                            let _,      rst = expect Tk_Op_Equal rst     in
                            let expr,   rst = parse_expr rst             in
                            let _,      rst = expect Tk_Semi rst         in S_Blk_Assign (lvalue, expr), rst

let rec parse_stmt_list fin acc tkns = match tkns with
                            | (kw,_)::rst when kw = fin -> lrev acc, rst
                            | tkns -> let stmt, rst = parse_statement tkns in parse_stmt_list fin (stmt::acc) rst



and parse_statement = function
    | (Tk_Hash,_)::(Tk_Literal i,_)::rst -> let enclosed, rst = parse_statement rst in
                                            S_Delay (int_of_string i, enclosed), rst
    | (Tk_At,_)::rst        -> let evs,      rst = parse_event_ctrl rst in
                               let enclosed, rst = parse_statement rst  in
                               S_EvControl (evs, enclosed), rst
    | (Tk_Kw_begin,_)::rst  -> let be_blk, rst = parse_stmt_list Tk_Kw_end  [] rst in S_Seq_Block be_blk, rst
    | (Tk_Kw_fork,_)::rst   -> let fj_blk, rst = parse_stmt_list Tk_Kw_join [] rst in S_Par_Block fj_blk, rst
    | (Tk_Builtin s,_)::rst -> let _,     rst = expect Tk_LParen rst in
                               let exprs, rst = if (fst (hd rst)) = Tk_RParen
                                    then [], rst
                                    else parse_delimited [] parse_expr Tk_Comma rst in
                               let _,     rst = expect Tk_RParen rst in
                               let _,     rst = expect Tk_Semi   rst in S_Builtin (s, exprs), rst
    | ((Tk_Kw_if,_)::rst) as i -> parse_conditional i
    | tkns                     -> let asgn, rst = parse_blk_assignment tkns in asgn, rst

and parse_conditional tkns = let _, rst = expect Tk_Kw_if  tkns in
                             let _, rst = expect Tk_LParen rst  in
                             let c, rst = parse_expr rst        in
                             let _, rst = expect Tk_RParen rst  in
                             let s, rst = parse_statement  rst  in match rst with
                                | (Tk_Kw_else,_)::rst -> let se, rst = parse_statement rst in
                                                         S_If_Else (c,s,se), rst
                                | rst -> S_If (c,s), rst

and parse_event = function
    | (Tk_Kw_posedge,_)::rst -> let e, rst = parse_expr rst in Posedge e, rst
    | (Tk_Kw_negedge,_)::rst -> let e, rst = parse_expr rst in Negedge e, rst
    | rst                    -> let e, rst = parse_expr rst in Level e, rst

and parse_event_ctrl = function
    | (Tk_Ident i,_)::rst -> [Level (E_Variable i)], rst
    | (Tk_LParen,_)::rst  -> let evs, rst = parse_delimited [] parse_event Tk_Comma rst in
                             let _,rst    = expect Tk_RParen rst in evs, rst
    | rst -> [], rst

let rec parse_mod_ent_lst acc = function
  | (Tk_Kw_wire,_)   :: rst -> let ioreg, lst, rst = parse_ioreg_decl rst
                               in parse_mod_ent_lst (Decl (Wire  , ioreg, lst)::acc) rst
  | (Tk_Kw_reg,_)    :: rst -> let ioreg, lst, rst = parse_ioreg_decl rst
                               in parse_mod_ent_lst (Decl (Reg   , ioreg, lst)::acc) rst
  | (Tk_Kw_input,_)  :: rst -> let ioreg, lst, rst = parse_ioreg_decl rst
                               in parse_mod_ent_lst (Decl (Input , ioreg, lst)::acc) rst
  | (Tk_Kw_output,_) :: rst -> let ioreg, lst, rst = parse_ioreg_decl rst
                               in parse_mod_ent_lst (Decl (Output, ioreg, lst)::acc) rst
  | (Tk_Kw_inout,_)  :: rst -> let ioreg, lst, rst = parse_ioreg_decl rst
                               in parse_mod_ent_lst (Decl (Inout , ioreg, lst)::acc) rst
  | (Tk_Kw_always,_) :: rst -> let stmt, rst = parse_statement  rst in parse_mod_ent_lst (Always  stmt::acc) rst
  | (Tk_Kw_initial,_):: rst -> let stmt, rst = parse_statement  rst in parse_mod_ent_lst (Initial stmt::acc) rst
  | (Tk_Kw_assign,_) :: rst -> let assgn,rst = parse_assignment rst in parse_mod_ent_lst (assgn::acc) rst
  | (Tk_Ident _,_) as i :: rst -> let m,rst = parse_module_inst (i::rst) in parse_mod_ent_lst (m::acc) rst
  | (Tk_Kw_endmodule,_) :: rst -> lrev acc, rst
  | e -> raise (NoParse (parse_error "module entity or endmodule" e))

let parse_module = function
  | (Tk_Kw_module,_) :: (Tk_Ident mname,_) :: (Tk_Semi,_) :: rst
    -> let ents, rst = parse_mod_ent_lst [] rst in
       Module (mname, [], ents) , rst

  | (Tk_Kw_module,_) :: (Tk_Ident mname,_) :: (Tk_LParen,_) :: (Tk_RParen,_) :: (Tk_Semi,_) :: rst
    -> let ents, rst = parse_mod_ent_lst [] rst in
       Module (mname, [], ents) , rst

  | (Tk_Kw_module,_) :: (Tk_Ident mname,_) :: (Tk_LParen,_) :: rst
    -> let pl, rst   = parse_port_lst [] rst in
       let ents, rst = parse_mod_ent_lst [] rst in
       Module (mname, pl, ents) , rst

  | e -> raise (NoParse (parse_error "module module <module_name>" e))

