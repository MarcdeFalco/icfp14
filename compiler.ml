open Ast
open Gcc

let lbl = ref 0
let iflbl = ref 0
let flblcount = ref 0

let currentPos = ref (0,0)

let rec compile env (loc, e) =
    let branches = ref [] in
    let rec get_idx s loc =
        match loc with
        | [] -> raise Not_found
        | (ss,_)::q -> if ss = s then 0 else 1 + get_idx s q
    in
    let rec lookup bl env s n =
        match env with
        | [] -> failwith ("Invalid lookup: " ^ s)
        | loc::q ->
            if List.mem_assoc s loc
            then let m = get_idx s loc in
                (if bl then LD (n,m) else ST (n,m))
            else lookup bl q s (n+1)
    in
    let rec eval_expr bt env e =
        match e with
        | SetFileInfo(e,a,b) -> currentPos := (a,b); [ FILEINFO(a,b) ] @ eval_expr bt env e
        | Nop -> []
        | Const n -> [LDC n]
        | Atom a -> eval_expr false env a @ [ATOM]
        | Cons (a,b) -> eval_expr false env a @ eval_expr false env b @ [ CONS ]
        | Add (a,b) -> eval_expr false env a @ eval_expr false env b @ [ ADD ]
        | Sub (a,b) -> eval_expr false env a @ eval_expr false env b @ [ SUB ]
        | Mul (a,b) -> eval_expr false env a @ eval_expr false env b @ [ MUL ]
        | Div (a,b) -> eval_expr false env a @ eval_expr false env b @ [ DIV ]
        | Var s -> [ lookup true env s 0 ]
        | Assign(s,e) -> eval_expr false env e @ [ lookup false env s 0 ]
        | Tuple (e,i,l) -> 
                let rec aux n p = 
                    match n, p with
                    | 0, _ -> [CAR]
                    | 1, 2 -> [CDR]
                    | _ -> CDR :: aux (n-1) (p-1)
                in eval_expr false env e @ aux i l
        | Head a -> eval_expr false env a @ [CAR]
        | Tail a -> eval_expr false env a @ [CDR]
        | Call (f,el) ->
                let n = List.length el in
                List.concat (List.map (eval_expr false env) el)
                @ [ lookup true env f 0; if bt then TAP (f,n) else AP (f,n) ]
        | Equals(a,b) -> eval_expr false env a @ eval_expr false env b @ [ CEQ ]
        | Greater(a,b) -> eval_expr false env a @ eval_expr false env b @ [ CGT ]
        | GreaterEquals(a,b) -> eval_expr false env a @ eval_expr false env b @ [ CGTE ]
        | If(c,t,f) ->
            if bt
            then begin
                let tlabel = "then"^string_of_int !iflbl in
                let elabel = "else"^string_of_int !iflbl in
                incr iflbl;
                eval_expr false env c 
                @ [TSELs(tlabel,elabel); Label tlabel]
                @ eval_expr true env t
                @ [RTN; Label elabel]
                @ eval_expr true env f
                @ [RTN]
            end else begin
                let tlabel = "then"^string_of_int !iflbl in
                let elabel = "else"^string_of_int !iflbl in
                incr iflbl;
                branches := (tlabel, t) :: (elabel,f) :: !branches;
                eval_expr false env c @ [ SELs(tlabel, elabel) ]
            end
        | Chain(a,b) -> eval_expr false env a @ eval_expr bt env b
        | Print a -> eval_expr false env a @ [ DBUG ]
    in
    let rec expand loc e =
        match e with
        | Var s -> 
            if List.mem_assoc s loc 
            then let DVar ep = List.assoc s loc in expand loc ep
            else Var s
        | SetFileInfo(e,a,b) -> SetFileInfo(expand loc e, a, b)
        | Cons(a,b) -> Cons(expand loc a, expand loc b)
        | Add(a,b) -> Add(expand loc a, expand loc b)
        | Sub(a,b) -> Sub(expand loc a, expand loc b)
        | Mul(a,b) -> Mul(expand loc a, expand loc b)
        | Div(a,b) -> Div(expand loc a, expand loc b)
        | Tuple(e,i,l) -> Tuple(expand loc e, i, l)
        | Head a -> Head (expand loc a)
        | Tail a -> Tail (expand loc a)
        | Call(f,el) -> Call(f, List.map (expand loc) el)
        | Equals(a,b) -> Equals(expand loc a, expand loc b)
        | Greater(a,b) -> Greater(expand loc a, expand loc b)
        | GreaterEquals(a,b) -> GreaterEquals(expand loc a, expand loc b)
        | If(a,b,c) -> If(expand loc a, expand loc b, expand loc c)
        | Chain(a,b) -> Chain(expand loc a, expand loc b)
        | Print a -> Print (expand loc a)
        | Atom a -> Atom (expand loc a)
        | _ -> e
    in

    let labels = ref [] in
        
    let rec push_locals loc others = match loc with
    | (_, DVar e)::q -> eval_expr false ([]::env) (expand others e) @ push_locals q others
    | (s, DFun (al,b))::q -> 
            let lbl = s ^ string_of_int !flblcount in
            incr flblcount;
            labels := ((al,b), lbl) :: !labels;
            LDFs lbl :: push_locals q others
    | [] -> []
    in
    let compile_branches env =
        let bcode = ref [] in
        while !branches <> [] do
            let (label, e) = List.hd !branches in
            branches := List.tl !branches;
            bcode := !bcode @ [ Label label ] @ eval_expr false env e @ [ JOIN ];
        done;
        !bcode
    in
    let rec compile_fun env l = 
        match l with
        [] -> []
        | (_, DVar _)::q -> compile_fun env q
        | (s, DFun(al, b))::q ->
                let nenv = (List.map (fun s -> (s, DDummy)) al) :: env in
                let funcode = Label (List.assoc (al,b) !labels) :: compile nenv b in
                funcode @ compile_fun env q
    in
    if loc = []
    then begin
        let exprcode = eval_expr true env e in 
        exprcode @ [ RTN ] @ compile_branches env
    end
    else
    begin
        (* Local definitions -> recursive encoding *)
        let label = "body"^string_of_int !lbl in
        incr lbl;
        let exprcode = 
            (DUM (List.length loc)
                :: (push_locals loc loc)) @
                [ LDFs label; TRAP (List.length loc);
                  Label label ]
                @ (eval_expr true (loc :: env) e)
                @ [ RTN ]
        in exprcode
            @ compile_branches (loc :: env)
            @ compile_fun (loc :: env) loc
    end

let compile_file fn =
    Printexc.record_backtrace true;

    let s = ref "" in

    let load fn =
        let f = open_in fn in
        let sz = in_channel_length f in
        let sloaded = String.make sz ' ' in
        really_input f sloaded 0 sz;
        sloaded
    in

    let fjoin = open_in "lambdaman.join" in
    begin try
        while true do
            let l = input_line fjoin in
            if l.[0] <> ';'
            then s := !s ^ load l
        done;
        failwith "Out of reach"
    with End_of_file -> () end;

    try
        let l = Parser.main Lexer.token (Lexing.from_string !s) in
        let code = compile [[("world", DDummy); ("unk", DDummy)]] l in
        let acode = absolute code in
        acode, !s
    with Ast.SyntaxError(loc,startpos,endpos) ->
        let n = String.length !s in
        let a = min (n-1) startpos.pos_cnum in
        let b = min (n-1) (max a endpos.pos_cnum) in
        let context = 20 in
        let a0 = max 0 (a - context) in
        let b0 = min (n-1) (b + context) in
        let sub a b = String.sub !s a (b-a) in
        Printf.printf "Syntax error while parsing a %s rule:\n%s***%s***%s\n"
            loc (sub a0 a) (sub a b) (sub b b0);
        failwith "Syntax error"
    | e ->
        let a, b  = !currentPos in
        let n = String.length !s in
        let context = 20 in
        let a0 = max 0 (a - context) in
        let b0 = min (n-1) (b + context) in
        let sub a b = String.sub !s a (b-a) in
        Printf.printf "Error while compiling:\n%s***%s***%s\n"
            (sub a0 a) (sub a b) (sub b b0);
        raise e
