let sprintf = Printf.sprintf
let lcase   = Char.lowercase_ascii
let smake   = String.make
let lrev    = List.rev

let fst = function (a, _) -> a
let snd = function (_, b) -> b

type token = Tk_LParen  | Tk_RParen   | Tk_LBrace  | Tk_RBrace  | Tk_LBracket | Tk_RBracket
           | Tk_Semi    | Tk_Colon    | Tk_Comma   | Tk_Hash    | Tk_Dot
           | Tk_Op_Plus | Tk_Op_Minus | Tk_Op_Mul  | Tk_Op_Div
           | Tk_BaseHex of string | Tk_BaseDec of string | Tk_BaseOct of string | Tk_BaseBin of string
           | Tk_Ident   of string | Tk_Literal of string | Tk_String of string
           | Tk_Kw_module | Tk_Kw_endmodule | Tk_Kw_reg   | Tk_Kw_wire
           | Tk_Kw_assign | Tk_Kw_output    | Tk_Kw_input | Tk_Kw_inout 
           | Tk_Kw_begin  | Tk_Kw_end | Tk_Kw_always

exception UnexpectedChar of string
exception UnexpectedEOF  of string
exception UnexpectedWTF  of string
exception Done
exception NoParse of string
exception UnexpectedToken of token * string

let kw_maybe = function "module"    -> Tk_Kw_module
                       |"endmodule" -> Tk_Kw_endmodule
                       |"reg"       -> Tk_Kw_reg
                       |"wire"      -> Tk_Kw_wire
                       |"input"     -> Tk_Kw_input
                       |"output"    -> Tk_Kw_output
                       |"inout"     -> Tk_Kw_inout
                       |"assign"    -> Tk_Kw_assign
                       |"always"    -> Tk_Kw_always
                       |"begin"     -> Tk_Kw_begin
                       |"end"       -> Tk_Kw_end
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
       | a when cond a -> str := !str ^ (smake 1 (lcase a))
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
        | '*' , _ -> unread in_f ; tk_list := (Tk_Op_Mul,   !line_num) :: !tk_list
        | '+' , _ -> unread in_f ; tk_list := (Tk_Op_Plus,  !line_num) :: !tk_list
        |  a  , _ when dec_set a -> unread in_f ; unread in_f ;
                                   let s = read_based_lit dec_set in_f
                                   in tk_list := (Tk_Literal s, !line_num) :: !tk_list
        | '"' , _ -> unread in_f ; let s = read_string_lit in_f 
                                   in tk_list := (Tk_String s, !line_num) :: !tk_list
        |  a  , _ when is_alpha a -> unread in_f; unread in_f ;
                                let s = take_while is_alphanum in_f
                                in  tk_list := (kw_maybe s, !line_num) :: !tk_list
        |  e  , _ -> raise (UnexpectedChar (sprintf "ERROR: Unexpected char %c at line %d\n" e !line_num ))
    done ; !tk_list
    with End_of_file -> close_in in_f ; lrev !tk_list

type expr = E_Plus of expr * expr
          | E_Mul of expr * expr
          | E_Variable of string
          | E_Literal of string
          | E_Range of expr * expr * expr
          | E_Index of expr * expr
          | E_Nul (*FIXME: delme; used only in single-bit IO Ranges*)

type portmap = Portmap of string * string (* should be of string*exp really *)

type ioreg_decl = IOReg of expr * expr * string list

type module_ent = Wire   of ioreg_decl
                | Reg    of ioreg_decl
                | Input  of ioreg_decl
                | Output of ioreg_decl
                | Inout  of ioreg_decl
                | Inst of string * string * portmap list * portmap list

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

let parse_var_or_lit = function
    | (Tk_Ident s,_)::rst -> E_Variable s, rst
    | (Tk_Literal s,_)::rst -> E_Literal s, rst
    | e -> raise (NoParse (parse_error "literal or identifier" e))

let rec parse_idx_or_range expr tkns = let _,  rst = expect Tk_LBracket tkns in
                                   let e1, rst = parse_term rst in match rst with 
                            | (Tk_Colon,_)::rst -> let e2, rst = parse_term rst in 
                                                   let _,  rst = expect Tk_RBracket rst in
                                                   E_Range (expr,e1,e2), rst
                            | (Tk_RBracket,_)::rst  -> E_Index (expr,e1), rst
                            | e -> raise (NoParse (parse_error "colon or rbracket" e))

and parse_indexable tkns = let v_or_l, rst = parse_var_or_lit tkns in
                           match rst with
                         | ((Tk_LBracket,_)::rst) as r -> parse_idx_or_range v_or_l r
                         | _ -> v_or_l, rst

and parse_factor tkns = let f1, rst1 = parse_indexable tkns in match rst1 with
         | (Tk_Op_Mul,_)::rst1 -> let f2, rst = parse_factor rst1 in E_Mul (f1, f2), rst
         | _ -> f1, rst1

and parse_term tkns = let t1, rst1 = parse_factor tkns in match rst1 with
         | (Tk_Op_Plus,_)::rst1 -> let t2, rst = parse_term rst1 in E_Plus (t1, t2), rst
         | _ -> t1, rst1



let parse_conn = function
    | (Tk_Dot,_)::(Tk_Ident p,_)::(Tk_LParen,_)::(Tk_Ident c,_)::(Tk_RParen,_)::rst
    -> (Portmap (p,c), rst)
    | e -> raise (NoParse (parse_error "port connection" e))

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
                                     let e1, rst = parse_term rst          in
                                     let _,  rst = expect Tk_Colon rst     in
                                     let e2, rst = parse_term rst          in
                                     let _,  rst = expect Tk_RBracket rst  in
                                     let il, rst = parse_ident_list [] rst in IOReg (e1, e2, il), rst
    | ((Tk_Ident _,_)::rst) as i  -> let il, rst = parse_ident_list [] i   in IOReg (E_Nul , E_Nul, il), rst
    | e -> raise (NoParse (parse_error "wire/reg/input/output decl" e))


let rec parse_mod_ent_lst acc = function
  | (Tk_Kw_wire,_)   :: rst -> let ioreg,rst = parse_ioreg_decl rst in parse_mod_ent_lst (Wire   ioreg::acc) rst
  | (Tk_Kw_reg,_)    :: rst -> let ioreg,rst = parse_ioreg_decl rst in parse_mod_ent_lst (Reg    ioreg::acc) rst
  | (Tk_Kw_input,_)  :: rst -> let ioreg,rst = parse_ioreg_decl rst in parse_mod_ent_lst (Input  ioreg::acc) rst
  | (Tk_Kw_output,_) :: rst -> let ioreg,rst = parse_ioreg_decl rst in parse_mod_ent_lst (Output ioreg::acc) rst
  | (Tk_Kw_inout,_)  :: rst -> let ioreg,rst = parse_ioreg_decl rst in parse_mod_ent_lst (Inout  ioreg::acc) rst
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