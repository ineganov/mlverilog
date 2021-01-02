open Lex_and_parse

let code = Char.code


type veri_value =  V_FourState of int*int*int
                 | V_String of string

exception NoParse of string
exception UnexpectedArguments
exception OutOfRange
exception NoWay

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

let fst_display_dec = function
   | V_String s -> s
   | V_FourState (w,r,v) -> string_of_int v

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

let fst_val = function V_FourState (_,_,v) -> v
                     | V_String _ -> (-2)

let fst_resize v sz = match v with
    | V_String v -> V_String v
    | V_FourState (w,r,v) -> let mask = 1 lsl sz - 1 in
        if w <= sz then V_FourState(sz, r, v)
                   else V_FourState(sz, r land mask, v land mask )

(*FIXME: '==' equality should consider Xs, as opposed to '===' *)
let fst_eqeq a b = match a, b with
                     | V_FourState (w1,r1,v1), V_FourState (w2,r2,v2) ->
                        if a = b then V_FourState (1, 1, 1) else V_FourState(1, 1, 0)
                     | V_String s1, V_String s2 ->
                        if s1 = s2 then V_FourState (1, 1, 1) else V_FourState(1, 1, 0)
                     | _,_ -> raise UnexpectedArguments

let fst_plus a b = match a, b with
                     | V_FourState (w1,r1,v1), V_FourState (w2,r2,v2) ->
                       let w = 1 + if w1 > w2 then w1 else w2 in
                       let r = 1 lsl w - 1 in (* this is wrong: take xs into account *)
                       if w <= Sys.int_size then V_FourState (w, r, (v1 + v2) land r)
                       else raise OutOfRange
                     | _,_ -> raise UnexpectedArguments

let fst_minus a b = match a, b with
                     | V_FourState (w1,r1,v1), V_FourState (w2,r2,v2) ->
                       let w = 1 + if w1 > w2 then w1 else w2 in
                       let r = 1 lsl w - 1 in (* this is wrong: take xs into account *)
                       if w <= Sys.int_size then V_FourState (w, r, (v1 - v2) land r)
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
