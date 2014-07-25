open Ast
open Asm

let lbl = ref 0
let iflbl = ref 0

let rec compile env (loc, e) =
    let branches = ref [] in
    let rec get_idx s loc =
        match loc with
        | [] -> raise Not_found
        | (ss,_)::q -> if ss = s then 0 else 1 + get_idx s q
    in
    let rec lookup env s n =
        match env with
        | [] -> raise Not_found
        | loc::q ->
            if List.mem_assoc s loc
            then let m = get_idx s loc in
                LD (n,m)
            else lookup q s (n+1)
    in
    let rec eval_expr env e =
        match e with
        | Const n -> [LDC n]
        | Cons (a,b) -> eval_expr env a @ eval_expr env b @ [ CONS ]
        | Add (a,b) -> eval_expr env a @ eval_expr env b @ [ ADD ]
        | Sub (a,b) -> eval_expr env a @ eval_expr env b @ [ SUB ]
        | Mul (a,b) -> eval_expr env a @ eval_expr env b @ [ MUL ]
        | Div (a,b) -> eval_expr env a @ eval_expr env b @ [ DIV ]
        | Var s -> [ lookup env s 0 ]
        | Call (f,el) ->
                List.concat (List.map (eval_expr env) el)
                @ [ lookup env f 0; AP (List.length el) ]
        | Equals(a,b) -> eval_expr env a @ eval_expr env b @ [ CEQ ]
        | If(c,t,f) ->
            let tlabel = "then"^string_of_int !iflbl in
            let elabel = "else"^string_of_int !iflbl in
            incr iflbl;
            branches := (tlabel, t) :: (elabel,f) :: !branches;
            eval_expr env c @ [ SEL(tlabel, elabel) ]
    in
    let rec push_locals loc = match loc with
    | (_, DVar e)::q -> eval_expr env e @ push_locals q
    | (s, DFun _)::q -> LDF s :: push_locals q
    | [] -> []
    in
    let compile_branches env =
        let bcode = ref [] in
        while !branches <> [] do
            let (label, e) = List.hd !branches in
            branches := List.tl !branches;
            bcode := !bcode @ [ Label label ] @ eval_expr env e @ [ JOIN ];
        done;
        !bcode
    in
    let rec compile_fun env l = 
        match l with
        [] -> []
        | (_, DVar _)::q -> compile_fun env q
        | (s, DFun(al, b))::q ->
                let nenv = (List.map (fun s -> (s, DDummy)) al) :: env in
                (Label s :: compile nenv b) @ compile_fun env q
    in
    if loc = []
    then begin
        let exprcode = eval_expr env e in 
        exprcode @ [ RTN ] @ compile_branches env
    end
    else begin
        (* Local definitions -> recursive encoding *)
        let label = "body"^string_of_int !lbl in
        incr lbl;
        let exprcode = 
            (DUM (List.length loc)
                :: (push_locals loc)) @
                [ LDF label; RAP (List.length loc); RTN;
                  Label label ] 
                @ (eval_expr (loc :: env) e)
                @ [ RTN ]
        in exprcode
            @ compile_branches (loc :: env)
            @ compile_fun (loc :: env) loc
    end

let print_stripped code =
    let rec aux code acc pos = 
        match code with 
        | [] -> acc
        | Label s::q -> aux q ((s,pos)::acc) pos
        | _::q -> aux q acc (pos+1)
    in
    let labels = aux code [] 0 in
    print_string
    (String.concat ""
        (List.map 
            (fun i -> match i with
                Label _ -> ""
                | LDF s -> "LDF " ^ string_of_int (List.assoc s labels) ^ "\n"
                | SEL (a,b) -> "SEL " ^ string_of_int (List.assoc a labels) ^ " " ^ string_of_int (List.assoc b labels) ^ "\n"
                | _ -> pp_instr i ^ "\n")
            code))

let _ =
    Printexc.record_backtrace true;
    let f = open_in Sys.argv.(1) in
    let sz = in_channel_length f in
    let s = String.make sz ' ' in
    really_input f s 0 sz;
    try
        let l = Parser.main Lexer.token (Lexing.from_string s) in
        let code = compile [] l in
        print_string (pp_code code);
        print_string "\n\nStripped:\n";
        print_stripped code
    with Ast.SyntaxError(loc,startpos,endpos) ->
        Printf.printf "Syntax error while parsing a %s rule:\n%s\n"
            loc (String.sub s startpos.pos_cnum
            (endpos.pos_cnum-startpos.pos_cnum))
