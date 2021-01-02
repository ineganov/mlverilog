type token = Tk_LParen  | Tk_RParen   | Tk_LBrace  | Tk_RBrace  | Tk_LBracket | Tk_RBracket
           | Tk_Semi    | Tk_Colon    | Tk_Comma   | Tk_Hash    | Tk_Dot      | Tk_At
           | Tk_Op_Plus | Tk_Op_Minus | Tk_Op_Mul  | Tk_Op_Div  | Tk_Op_Equal | Tk_EqEq
           | Tk_BinAnd  | Tk_BinOr    | Tk_BinXor  | Tk_BinInv
           | Tk_BaseHex of string | Tk_BaseDec of string | Tk_BaseOct of string | Tk_BaseBin of string
           | Tk_Ident   of string | Tk_Literal of string | Tk_String of string | Tk_Builtin of string
           | Tk_Kw_module | Tk_Kw_endmodule | Tk_Kw_reg   | Tk_Kw_wire | Tk_Kw_posedge | Tk_Kw_negedge
           | Tk_Kw_assign | Tk_Kw_output    | Tk_Kw_input | Tk_Kw_inout | Tk_Kw_if | Tk_Kw_else
           | Tk_Kw_begin  | Tk_Kw_end | Tk_Kw_fork | Tk_Kw_join | Tk_Kw_always | Tk_Kw_initial

type unary_op = Uop_And | Uop_Or | Uop_Xor | Uop_Inv

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



val fst          : 'a * 'b -> 'a
val snd          : 'a * 'b -> 'b
val tokenize     : string -> (token * int) list
val parse_expr   : (token * int) list -> expr * (token * int) list
val parse_module : (token * int) list -> module_def * (token * int) list