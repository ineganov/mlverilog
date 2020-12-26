let sprintf  = Printf.sprintf
let clcase   = Char.lowercase_ascii
let code     = Char.code
let smake    = String.make
let lrev     = List.rev
let lmap     = List.map
let hd       = List.hd
let slcase   = String.lowercase_ascii
let sconcat  = String.concat
let hfind    = Hashtbl.find
let hreplace = Hashtbl.replace

let fst = function (a, _) -> a
let snd = function (_, b) -> b
let sm_val = function (_, _, v) -> v

type token = Tk_LParen  | Tk_RParen   | Tk_LBrace  | Tk_RBrace  | Tk_LBracket | Tk_RBracket
           | Tk_Semi    | Tk_Colon    | Tk_Comma   | Tk_Hash    | Tk_Dot      | Tk_At
           | Tk_Op_Plus | Tk_Op_Minus | Tk_Op_Mul  | Tk_Op_Div  | Tk_Op_Equal
           | Tk_BinAnd  | Tk_BinOr    | Tk_BinXor  | Tk_BinInv
           | Tk_BaseHex of string | Tk_BaseDec of string | Tk_BaseOct of string | Tk_BaseBin of string
           | Tk_Ident   of string | Tk_Literal of string | Tk_String of string | Tk_Builtin of string
           | Tk_Kw_module | Tk_Kw_endmodule | Tk_Kw_reg   | Tk_Kw_wire | Tk_Kw_posedge | Tk_Kw_negedge
           | Tk_Kw_assign | Tk_Kw_output    | Tk_Kw_input | Tk_Kw_inout | Tk_Kw_if | Tk_Kw_else
           | Tk_Kw_begin  | Tk_Kw_end | Tk_Kw_fork | Tk_Kw_join | Tk_Kw_always | Tk_Kw_initial

exception UnexpectedChar of string
exception UnexpectedEOF  of string
exception UnexpectedWTF  of string
exception Done
exception NoParse of string
exception UnexpectedToken of token * string
exception NoEval of string
exception NoWay
exception Not_declared of string
exception NonConstExpr
exception UnexpectedArguments
exception OutOfRange
exception NotImplemented of string
exception Yield
exception StackUnderflow

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

type expr = E_Plus of expr * expr
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

and parse_sum tkns = let t1, rst1 = parse_factor tkns in match rst1 with
         | (Tk_Op_Plus,_)::rst1 -> let t2, rst = parse_sum rst1 in E_Plus (t1, t2), rst
         | _ -> t1, rst1

and parse_and tkns = let t1, rst1 = parse_sum tkns in match rst1 with
         | (Tk_BinAnd,_)::rst1 -> let t2, rst = parse_and rst1 in E_BinAnd (t1, t2), rst
         | _ -> t1, rst1

and parse_xor tkns = let t1, rst1 = parse_and tkns in match rst1 with
         | (Tk_BinXor,_)::rst1 -> let t2, rst = parse_xor rst1 in E_BinXor (t1, t2), rst
         | _ -> t1, rst1

and parse_or  tkns = let t1, rst1 = parse_xor tkns in match rst1 with
         | (Tk_BinOr,_)::rst1 -> let t2, rst = parse_or rst1 in E_BinOr (t1, t2), rst
         | _ -> t1, rst1

and parse_expr tkns = parse_or tkns



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



type veri_value =  V_FourState of int*int*int
                 | V_String of string

let fst_display = function
   | V_String s -> s
   | V_FourState (w,r,v) ->
    let str = ref "" in
    for i = 0 to w - 1 do
        match (1 land (r lsr i), 1 land (v lsr i)) with
           | 0,0 -> str := "x" ^ !str
           | 0,1 -> str := "z" ^ !str
           | 1,0 -> str := "0" ^ !str
           | 1,1 -> str := "1" ^ !str
           | _ -> raise NoWay
    done ; !str

let fst_from_literal = function
   | E_Unbased l     -> V_FourState (32,0xFFFFFFFF, int_of_string l)
   | E_Based_H (w,l) -> let r    = ref 0       in
                        let v    = ref 0       in
                        let mask = 1 lsl w - 1 in
                        for i = 0 to String.length l - 1 do
                            r := !r lsl 4;
                            v := !v lsl 4;
                            match l.[i] with
                               | '0'..'9' as c -> r := !r lor 0xf; v := !v lor ((code c) - (code '0'))
                               | 'a'..'f' as c -> r := !r lor 0xf; v := !v lor (10 + (code c) - (code 'a'))
                               | 'z' -> v := !v lor 0xf
                               | 'x' -> v := !v lor 0x0
                               | _ -> raise (NoParse "illegal character in hex literal")
                        done; V_FourState (w, !r land mask, !v land mask)
   | E_Based_D (w,l) -> let mask = 1 lsl w - 1 in V_FourState (w, mask, (int_of_string l) land mask )
   | E_Based_O (w,l) -> let r = ref 0          in
                        let v = ref 0          in
                        let mask = 1 lsl w - 1 in
                        for i = 0 to String.length l - 1 do
                            r := !r lsl 3;
                            v := !v lsl 3;
                            match l.[i] with
                               | '0'..'7' as c -> r := !r lor 0x7; v := !v lor ((code c) - (code '0'))
                               | 'z' -> v := !v lor 0x7
                               | 'x' -> v := !v lor 0x0
                               | _ -> raise (NoParse "illegal character in oct literal")
                        done; V_FourState (w, !r land mask, !v land mask)
   | E_Based_B (w,l) -> let r = ref 0 in
                        let v = ref 0 in
                        for i = 0 to String.length l - 1 do
                           r := !r lsl 1;
                           v := !v lsl 1;
                           match l.[i] with
                           | '0' -> r := !r lor 1; v := !v lor 0
                           | '1' -> r := !r lor 1; v := !v lor 1
                           | 'z' -> r := !r lor 0; v := !v lor 1
                           | 'x' -> r := !r lor 0; v := !v lor 0
                           | e -> raise (NoParse "illegal character in bin literal")
                        done; V_FourState (w, !r, !v)
   | _ -> raise NoWay


let fst_plus a b = match a, b with
                     | V_FourState (w1,r1,v1), V_FourState (w2,r2,v2) ->
                       let w = 1 + if w1 > w2 then w1 else w2 in
                       let r = 1 lsl w - 1 in (* this is wrong: take xs into account *)
                       if w <= Sys.int_size then V_FourState (w, r, (v1 + v2) land r)
                       else raise OutOfRange
                     | _,_ -> raise UnexpectedArguments

let fst_mul a b = match a, b with
                     | V_FourState (w1,r1,v1), V_FourState (w2,r2,v2) ->
                       let w = w1 + w2 in
                       let r = 1 lsl w - 1 in (* this is wrong: take xs into account *)
                       if w <= Sys.int_size then V_FourState (w, r, (v1 * v2) land r)
                       else raise OutOfRange
                     | _,_ -> raise UnexpectedArguments

let fst_binand a b = match a, b with
                     | V_FourState (w1,r1,v1), V_FourState (w2,r2,v2) ->
                        let w = if w1 > w2 then w1 else w2 in
                        let r = r1 land r2                 in
                        V_FourState (w,r, (v1 land v2) land r  )
                     | _,_ -> raise UnexpectedArguments

let fst_binor a b = match a, b with
                     | V_FourState (w1,r1,v1), V_FourState (w2,r2,v2) ->
                        let w = if w1 > w2 then w1 else w2 in
                        let r = r1 land r2                 in
                        V_FourState (w,r, (v1 lor v2) land r  )
                     | _,_ -> raise UnexpectedArguments

let fst_binxor a b = match a, b with
                     | V_FourState (w1,r1,v1), V_FourState (w2,r2,v2) ->
                        let w = if w1 > w2 then w1 else w2 in
                        let r = r1 land r2                 in
                        V_FourState (w,r, (v1 lxor v2) land r  )
                     | _,_ -> raise UnexpectedArguments

let fst_uninv = function V_FourState (w,r,v) -> V_FourState(w,r, (lnot v) land r )
                        | _ -> raise UnexpectedArguments

let fst_unor = function V_FourState (w,r,v) -> let r_r = ref 1 in
                                               let r_v = ref 0 in
                                               for i = 0 to w - 1 do
                                                 r_r := !r_r land (r lsr i) ;
                                                 r_v := !r_v lor (1 land (v lsr i)) ;
                                               done;
                                               V_FourState (1, !r_r, !r_v)
                      | _ -> raise UnexpectedArguments

let fst_unxor = function V_FourState (w,r,v) -> let r_r = ref 1 in
                                                let r_v = ref 0 in
                                                for i = 0 to w - 1 do
                                                  r_r := !r_r land (r lsr i) ;
                                                  r_v := !r_v lxor (1 land (v lsr i)) ;
                                                done;
                                                V_FourState (1, !r_r, !r_v)
                      | _ -> raise UnexpectedArguments

let fst_unand = function V_FourState (w,r,v) -> let r_r = ref 1 in
                                                let r_v = ref 1 in
                                                for i = 0 to w - 1 do
                                                  r_r := !r_r land (r lsr i) ;
                                                  r_v := !r_v land (1 land (v lsr i)) ;
                                                done;
                                                V_FourState (1, !r_r, !r_v)
                      | _ -> raise UnexpectedArguments

type process_env = { kinds  : (string, decl_kind ) Hashtbl.t ;
                     ranges : (string, ioreg_decl) Hashtbl.t ;
                     values : (string, veri_value) Hashtbl.t ;  }

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
  | E_Mul    (a,b) -> fst_mul    (eval_expr env a) (eval_expr env b)
  | E_BinAnd (a,b) -> fst_binand (eval_expr env a) (eval_expr env b)
  | E_BinOr  (a,b) -> fst_binor  (eval_expr env a) (eval_expr env b)
  | E_BinXor (a,b) -> fst_binxor (eval_expr env a) (eval_expr env b)
  | E_Unary (Uop_Inv, a) -> fst_uninv (eval_expr env a)
  | E_Unary (Uop_Or,  a) -> fst_unor  (eval_expr env a)
  | E_Unary (Uop_Xor, a) -> fst_unxor (eval_expr env a)
  | E_Unary (Uop_And, a) -> fst_unand (eval_expr env a)
  | _ -> raise (NoEval "Unsupported expr :(")

let eval_builtin s lst = match s with
  | "display" -> print_endline ("DISPLAY: " ^ (sconcat ", " (List.map fst_display lst)))
  | "finish"  -> print_endline "FINISH: called"
  | e -> raise (NoEval ("Unsupported builtin: " ^ e))

let print_symtable env =
    let printer s f = Printf.printf "%s -> %s\n" s (fst_display f)
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

let all_zs = function
    | Range (e1, e2) -> let msb = match eval_constexpr e1 with
                            | V_FourState (_,_,msb) -> msb
                            | V_String _ -> raise NonConstExpr in
                        let lsb = match eval_constexpr e2 with
                            | V_FourState (_,_,lsb) -> lsb
                            | V_String _ -> raise NonConstExpr in
                        let w   = msb - lsb + 1  in V_FourState (w,0,0)
    | Single -> V_FourState (1,0,-1)

let populate_symtable ents =
    let env   = {kinds  = Hashtbl.create 100;
                 ranges = Hashtbl.create 100;
                 values = Hashtbl.create 100;} in
    let decls = List.filter_map (function Decl (k,r,lst) -> Some (k,r,lst) | _ -> None) ents in
    let add_multiple (k,r,lst) = List.iter (fun s -> Hashtbl.add env.kinds  s k;
                                                     Hashtbl.add env.ranges s r;
                                                     Hashtbl.add env.values s (all_zs r);) lst in
    List.iter (fun d -> add_multiple d) decls ; env


type instr = I_Read of string
           | I_Write of string
           | I_Literal of veri_value
           | I_Delay of int
           | I_Event of event list
           | I_Plus
           | I_Mul
           | I_BinAnd
           | I_BinOr
           | I_BinXor
           | I_UnInv
           | I_UnOr
           | I_UnAnd
           | I_UnXor
           | I_Index of string
           | I_Builtin of string * int
           | I_Restart
           | I_Halt
           | I_Finish
           | I_Time

type process_status = Stts_Ready        |
                      Stts_Finish       |
                      Stts_Halt         |
                      Stts_Delay of int |
                      Stts_Event of event list

type process_state = { mutable status : process_status;
                       mutable pc     : int;
                       mutable dstack : veri_value list;
                               instrs : instr array ;
                               pid    : int ;
                               env    : process_env ;
                               time   : int ref  }

let init_pstate p ii e t = {  status = Stts_Ready;
                              pc = 0;
                              dstack = [];
                              instrs = Array.of_list ii;
                              pid = p;
                              env = e;
                              time = t }

let push_dstk v pstate = pstate.dstack <- v::pstate.dstack
let pop_dstk pstate = try (let v = List.hd pstate.dstack in
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
  | E_Plus (a,b)         -> binop a b I_Plus
  | E_Mul (a,b)          -> binop a b I_Mul
  | E_BinAnd (a,b)       -> binop a b I_BinAnd
  | E_BinOr (a,b)        -> binop a b I_BinOr
  | E_BinXor (a,b)       -> binop a b I_BinXor
  | E_Unary (Uop_Inv, a) -> uop a I_UnInv
  | E_Unary (Uop_Or, a)  -> uop a I_UnOr
  | E_Unary (Uop_Xor, a) -> uop a I_UnXor
  | E_Unary (Uop_And, a) -> uop a I_UnAnd
  | E_Index (E_Variable a, e) -> [ I_Literal (eval_constexpr e);
                                   I_Index a ]
  | E_Index _            -> raise (NotImplemented "Index must be variable based")
  | E_Range _            -> raise (NotImplemented "Ranges not implemented")
  | E_Concat _           -> raise (NotImplemented "Concats not implemented")
  | E_Builtin "time"     -> [I_Time]
  | E_Builtin _          -> raise (NotImplemented "Only $time is allowed in expressions")

 (* | _ -> raise (NoEval "Unsupported expr for compilation :(") *)

let rec compile_stmt = function
   | S_Builtin ("finish", _) -> [I_Finish]
   | S_Builtin ("time",   _) -> [I_Time]
   | S_Builtin (s, lst) -> (List.concat (lmap compile_expr lst)) @ [I_Builtin (s,List.length lst)]
   | S_Seq_Block lst    ->  List.concat (lmap compile_stmt lst)
   | S_Blk_Assign (E_Variable v, ex) -> (compile_expr ex) @ [I_Write v]
   | S_Delay (d, stmt) -> [I_Delay d] @ (compile_stmt stmt)
   | S_EvControl (el, stmt) -> [I_Event el] @ (compile_stmt stmt)
   | _ -> raise (NoEval "Unsupported stmt for compilation :(")


let run_instr ps =
    let inst     = ps.instrs.(ps.pc)  in
    let binop fn = ( let a = pop_dstk ps in
                     let b = pop_dstk ps in
                     push_dstk (fn a b) ps; pc_incr ps ) in
    let uop   fn = ( let a = pop_dstk ps in
                     push_dstk (fn a) ps; pc_incr ps )   in

    match inst with
    | I_Read  s       -> push_dstk (hvalue ps.env s) ps; pc_incr ps;
    | I_Write s       -> let tos = pop_dstk ps in
                         hreplace ps.env s tos; pc_incr ps
    | I_Delay d       -> pc_incr ps;
                         ps.status <- Stts_Delay (d + !(ps.time));
                         raise Yield
    | I_Event el      -> pc_incr ps;
                         ps.status <- Stts_Event el;
                         raise Yield
    | I_Literal v     -> push_dstk v ps; pc_incr ps
    | I_Plus          -> binop fst_plus
    | I_Mul           -> binop fst_mul
    | I_BinAnd        -> binop fst_binand
    | I_BinOr         -> binop fst_binor
    | I_BinXor        -> binop fst_binxor
    | I_UnInv         -> uop   fst_uninv
    | I_UnOr          -> uop   fst_unor
    | I_UnAnd         -> uop   fst_unand
    | I_UnXor         -> uop   fst_unxor
    | I_Index s       -> raise (NotImplemented "I_Index")
    | I_Builtin (s,l) -> let lst = pop_dstk_n l ps in eval_builtin s lst ; pc_incr ps
    | I_Restart       -> ps.pc <- 0 ; ps.dstack <- []
    | I_Halt          -> ps.status <- Stts_Halt; raise Yield
    | I_Finish        -> ps.status <- Stts_Finish; raise Yield
    | I_Time          -> push_dstk (V_FourState (Sys.int_size, -1, !(ps.time))) ps

let compile_process = function
    | Always  s -> (compile_stmt s) @ [I_Restart]
    | Initial s -> (compile_stmt s) @ [I_Halt]
    | Assign  _ -> raise (NotImplemented "Assign statement compilation")
    | _         -> raise NoWay

let make_process_tab sim_time = function Module (_, _, ents) ->
       let d_ents = populate_symtable ents in
       let f_ents = List.filter (function
                                    | Always _ | Initial _ | Assign _ -> true
                                    | _ -> false ) ents in
       let pids = List.init (List.length f_ents) (fun f -> f+1) in
       let prcs = List.map  compile_process f_ents              in
       List.map2 (fun n bc -> init_pstate n bc d_ents sim_time) pids prcs

let run_process ps = try while true do run_instr ps done with Yield -> ()

let run_tab p = lmap run_process p


let rec pick_eligible = function
   | {status = Stts_Ready; _} as ans :: _ -> Some ans
   | _ :: rst -> pick_eligible rst
   | [] -> None

