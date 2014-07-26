exception Error

type token = 
  | VAR
  | UMINUS
  | TO
  | TL
  | TIMES
  | THEN
  | SQRT
  | SEMICOLON
  | RPAREN
  | RETURN
  | REC
  | RBRACKET
  | RACCO
  | PRINT
  | POW
  | PLUS
  | PIPE
  | MINUS
  | LPAREN
  | LET
  | LESSEQUALS
  | LESS
  | LEFTARROW
  | LBRACKET
  | LACCO
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | HD
  | GREATEREQUALS
  | GREATER
  | FUN
  | FOR
  | EQUALS
  | EOF
  | ELSE
  | DOUBLEEQUALS
  | DOT
  | DONE
  | DO
  | DIV
  | COMMA
  | COLON
  | ARROW

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState134
  | MenhirState133
  | MenhirState130
  | MenhirState126
  | MenhirState125
  | MenhirState121
  | MenhirState118
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState112
  | MenhirState111
  | MenhirState110
  | MenhirState109
  | MenhirState108
  | MenhirState107
  | MenhirState106
  | MenhirState105
  | MenhirState104
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState84
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState74
  | MenhirState73
  | MenhirState70
  | MenhirState69
  | MenhirState67
  | MenhirState63
  | MenhirState60
  | MenhirState57
  | MenhirState56
  | MenhirState54
  | MenhirState53
  | MenhirState49
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState11
  | MenhirState9
  | MenhirState7
  | MenhirState6
  | MenhirState4
  | MenhirState2
  | MenhirState0

  
open Ast

let rec chainize l =
    match l with
    | [] -> failwith "Invalid"
    | [x] -> x
    | t::q -> Chain(t, chainize(q))

let rec consize l =
    match l with
    | [] -> failwith "Invalid"
    | [x] -> x
    | t::q -> Cons(t, consize(q))

let split_var_tuple vl e =
    let n = List.length vl in
    let rec aux l i =
        match l with
        [] -> []
        | t::q -> (t, DVar (Tuple(e,i,n)))::aux q (i+1)
    in aux vl 0

let _eRR =
  Error

let rec _menhir_reduce26 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s), _, a), _), _, l) = _menhir_stack in
    let _v : (Ast.expr) =                                                                             ( consize(a::l) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce20 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), b) = _menhir_stack in
    let _v : (Ast.expr) =                                          ( Tuple(a,b,2) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce19 : _menhir_env -> ((('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * (int)) * (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s, a), _), b), c) = _menhir_stack in
    let _v : (Ast.expr) =                                                       ( Tuple(a,b,c) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce22 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, a), _) = _menhir_stack in
    let _v : (Ast.expr) =                       ( Tail a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce21 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, a), _) = _menhir_stack in
    let _v : (Ast.expr) =                       ( Head a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce6 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string list)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, vl), _, e) = _menhir_stack in
    let _v : ((string * Ast.decl) list) =                                                                       ( split_var_tuple vl e ) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_error117 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string list)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)

and _menhir_error71 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error61 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce24 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
    let _v : (Ast.expr) =                                   ( e ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_error51 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce25 : _menhir_env -> ('ttv_tail * _menhir_state * (string)) * _menhir_state * (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, f), _, xs0) = _menhir_stack in
    let _v : (Ast.expr) = let args =
      let xs = xs0 in
          ( xs )
    in
                                                                  ( Call(f,args) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run105 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run109 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run113 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | HD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack)
    | TL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run101 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, x), _), _, xs) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | HD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack)
    | TL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_reduce42 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ xs ->
    let ((_menhir_stack, _menhir_s, x), _) = _menhir_stack in
    let _v : (Ast.expr list) =     ( x :: xs ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s l ->
    let _v : (Ast.expr) =                                                    ( chainize(l) ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error51 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error51 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error61 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | IN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (((_menhir_stack, _menhir_s), _, v), _, e) = _menhir_stack in
              let _v : (string * Ast.decl) =                                          ( (v, DVar e) ) in
              _menhir_goto_base_decl _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error61 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error71 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error71 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error117 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | IN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | FUN ->
                  _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState118
              | LET ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState118
              | ID _ | IF | INT _ | LPAREN | PRINT ->
                  _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState118
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
          | ID _ | IF | INT _ | LPAREN | PRINT ->
              _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error117 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, d), _, r) = _menhir_stack in
        let _v : (Ast.t) =                              ( (d,r) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState125 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RACCO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), f), _, xs0), _, b) = _menhir_stack in
                let _v : (string * Ast.decl) = let args =
                  let xs = xs0 in
                      ( xs )
                in
                                                                                                      ( (f, DFun(args,b)) ) in
                _menhir_goto_base_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState0 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, t) = _menhir_stack in
                let _v : (Ast.t) =                    ( t ) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = _v in
                Obj.magic _1
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | IF ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INT _v ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LPAREN ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | PRINT ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | IF ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PRINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | RPAREN ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | DIV | DOT | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | PRINT | SEMICOLON | TIMES ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 | MenhirState74 | MenhirState57 | MenhirState43 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState15 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, x) = _menhir_stack in
              let _v : (Ast.expr list) =     ( [ x ] ) in
              _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | COMMA | DIV | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN | TIMES ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState28
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
          | COMMA | DIV | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN | TIMES ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState45 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState47 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState53 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | SEMICOLON ->
              _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | RPAREN ->
              _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState126 | MenhirState4 | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | SEMICOLON ->
              _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | EOF | IN | RACCO | RPAREN ->
              _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState73 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState74
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState74
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState74
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | SEMICOLON ->
              _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | RPAREN ->
              _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState79 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState81 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
              | IF ->
                  _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState82
              | INT _v ->
                  _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
              | LPAREN ->
                  _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState82
              | PRINT ->
                  _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | DOUBLEEQUALS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | GREATER ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | GREATEREQUALS ->
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | LESS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | LESSEQUALS ->
              _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | MINUS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | PLUS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87
          | ID _ | IF | IN | INT _ | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | SEMICOLON | TIMES ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | SEMICOLON ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState102
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState102
          | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | SEMICOLON | TIMES ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | SEMICOLON ->
              _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | MINUS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | PLUS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | MINUS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | PLUS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState110
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState110
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState110
          | MINUS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState110
          | PLUS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState110
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState110
          | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | MINUS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | PLUS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | MINUS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | PLUS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | DOUBLEEQUALS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | GREATER ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | GREATEREQUALS ->
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | LESS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | LESSEQUALS ->
              _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | MINUS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | PLUS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | ID _ | IF | IN | INT _ | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState134 | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | DOT ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | DOUBLEEQUALS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | GREATER ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | GREATEREQUALS ->
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | LBRACKET ->
              _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | LESS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | LESSEQUALS ->
              _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | MINUS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | PLUS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | SEMICOLON ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState133 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
              | IF ->
                  _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState134
              | INT _v ->
                  _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
              | LPAREN ->
                  _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState134
              | PRINT ->
                  _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState134
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
          | TIMES ->
              _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | ID _ | IF | IN | INT _ | LPAREN | PRINT ->
              _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | _ ->
        _menhir_fail ()

and _menhir_goto_base_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130
    else
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | FUN ->
          _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState130
      | LET ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState130
      | ID _ | IF | INT _ | LPAREN | PRINT ->
          _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState130
      | _ ->
          (assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)

and _menhir_reduce9 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _startpos__1_, _endpos__1_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : ((string * Ast.decl) list) =             ( raise (SyntaxError ("decl", _startpos, _endpos)) ) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState126 | MenhirState70 | MenhirState4 | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos__1_ _endpos__1_ ->
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.expr) =             ( raise (SyntaxError ("expr", _startpos, _endpos)) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s c ->
    let _v : (Ast.expr) =               ( Const c ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce28 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, v) = _menhir_stack in
    let _v : (Ast.expr) =              ( Var v ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | IF ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INT _v ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LPAREN ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PRINT ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce29 : _menhir_env -> ((((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((((_menhir_stack, _menhir_s), _, cond), _), _, bthen), _), _, belse) = _menhir_stack in
    let _v : (Ast.expr) =                                                          ( If(cond,bthen,belse) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce11 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                               ( Mul(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce10 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                              ( Add(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce12 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                             ( Div(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce13 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                               ( Sub(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce18 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                    ( GreaterEquals(b,a) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce16 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                              ( Greater(b,a) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce17 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                       ( GreaterEquals(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce15 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                 ( Greater(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce14 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                      ( Equals(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce23 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, a) = _menhir_stack in
    let _v : (Ast.expr) =                      ( Print a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _ | IF | INT _ | LPAREN | PRINT ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | FUN | LET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _startpos__1_, _endpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (string * Ast.decl) =             ( raise (SyntaxError ("base_decl", _startpos, _endpos)) ) in
        _menhir_goto_base_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce41 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : (Ast.expr list) =     ( [ x ] ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.decl) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ID _v ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
          | IF ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | INT _v ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
          | LPAREN ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | PRINT ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, t), _, q) = _menhir_stack in
        let _v : ((string * Ast.decl) list) =                              ( t::q ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, vl), _, e), _, q) = _menhir_stack in
        let _v : ((string * Ast.decl) list) =                                                                                   ( split_var_tuple vl e @ q ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IF ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | PRINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | RPAREN ->
            _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | COMMA | DIV | DOT | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN | TIMES ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LACCO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FUN ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | LET ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | ID _ | IF | INT _ | LPAREN | PRINT ->
                _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState125
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
    | EQUALS | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp _menhir_env._menhir_endp

and _menhir_reduce8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.decl) list) =       ( [] ) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState2 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | IF ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | INT _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | PRINT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run119 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState121 in
                let _v : (string list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.t) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUN ->
        _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _ | IF | INT _ | LPAREN | PRINT ->
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



