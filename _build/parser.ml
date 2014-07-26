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
  | ATOM
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
  | MenhirState150
  | MenhirState149
  | MenhirState146
  | MenhirState142
  | MenhirState141
  | MenhirState137
  | MenhirState134
  | MenhirState131
  | MenhirState130
  | MenhirState129
  | MenhirState128
  | MenhirState127
  | MenhirState126
  | MenhirState125
  | MenhirState124
  | MenhirState123
  | MenhirState122
  | MenhirState121
  | MenhirState120
  | MenhirState119
  | MenhirState118
  | MenhirState117
  | MenhirState116
  | MenhirState115
  | MenhirState114
  | MenhirState113
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState99
  | MenhirState98
  | MenhirState96
  | MenhirState93
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState83
  | MenhirState82
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState75
  | MenhirState71
  | MenhirState68
  | MenhirState67
  | MenhirState64
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState48
  | MenhirState47
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
  | MenhirState17
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState12
  | MenhirState10
  | MenhirState8
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

let rec _menhir_reduce29 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr list) -> 'ttv_return =
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

and _menhir_error133 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string list)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)

and _menhir_error80 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error69 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce27 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
    let _v : (Ast.expr) =                                   ( e ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_error58 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce28 : _menhir_env -> ('ttv_tail * _menhir_state * (string)) * _menhir_state * (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, f), _, xs0) = _menhir_stack in
    let _v : (Ast.expr) = let args =
      let xs = xs0 in
          ( xs )
    in
                                                                  ( Call(f,args) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run102 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run113 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113

and _menhir_run117 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117

and _menhir_run119 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119

and _menhir_run121 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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

and _menhir_run123 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123

and _menhir_run125 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_run127 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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

and _menhir_run115 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState93 | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, x), _), _, xs) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
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
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_reduce26 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s), _, a) = _menhir_stack in
    let _v : (Ast.expr) =                                   ( Atom a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

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

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

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

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_reduce45 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ xs ->
    let ((_menhir_stack, _menhir_s, x), _) = _menhir_stack in
    let _v : (Ast.expr list) =     ( x :: xs ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s l ->
    let _v : (Ast.expr) =                                                    ( chainize(l) ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error58 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error58 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error69 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_error69 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error80 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error80 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error133 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | IN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | FUN ->
                  _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState134
              | LET ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState134
              | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
                  _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState134
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
          | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
              _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error133 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, d), _, r) = _menhir_stack in
        let _v : (Ast.t) =                              ( (d,r) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState141 ->
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
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | ID _v ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | INT _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LPAREN ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | PRINT ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LEFTARROW ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | ID _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | IF ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | INT _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | LPAREN ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | PRINT ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | RETURN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | ID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | IF ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | INT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | RPAREN ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | ATOM | DIV | DOT | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | PRINT | RETURN | SEMICOLON | TIMES ->
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | ID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | IF ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | INT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState15
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState15 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
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
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState31
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
          | COMMA | DIV | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN | TIMES ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState33
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState33
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState37
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState39
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | MenhirState93 | MenhirState83 | MenhirState64 | MenhirState48 | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState47 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
              | ID _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
              | IF ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48
              | INT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState48
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, x) = _menhir_stack in
              let _v : (Ast.expr list) =     ( [ x ] ) in
              _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState52 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
              | ID _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
              | IF ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53
              | INT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState54 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
              | ID _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
              | IF ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState55
              | INT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState55
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState60 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
              | ID _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
              | IF ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState64
              | INT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | SEMICOLON ->
              _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | RPAREN ->
              _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState142 | MenhirState4 | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | SEMICOLON ->
              _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | EOF | IN | RACCO | RPAREN ->
              _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState68
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState82 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
              | ID _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
              | IF ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83
              | INT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | SEMICOLON ->
              _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState82
          | RPAREN ->
              _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState88 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | ID _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
              | IF ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | INT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState90 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState91
              | ID _v ->
                  _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
              | IF ->
                  _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState91
              | INT _v ->
                  _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
              | LPAREN ->
                  _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState91
              | PRINT ->
                  _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
              | RETURN ->
                  _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState91
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState90
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState99 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState99
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | DOUBLEEQUALS ->
              _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | GREATER ->
              _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | GREATEREQUALS ->
              _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | LESS ->
              _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | LESSEQUALS ->
              _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState103
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103
          | ATOM | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON | TIMES ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState116
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState116
          | ATOM | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON | TIMES ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState124
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState124
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState124
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState124
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState124
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState124
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState128
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState128
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState128
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState128
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState128
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState128
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | DOUBLEEQUALS ->
              _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | GREATER ->
              _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | GREATEREQUALS ->
              _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | LESS ->
              _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | LESSEQUALS ->
              _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | DOUBLEEQUALS ->
              _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | GREATER ->
              _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | GREATEREQUALS ->
              _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | LESS ->
              _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | LESSEQUALS ->
              _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState130
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | DOUBLEEQUALS ->
              _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | GREATER ->
              _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | GREATEREQUALS ->
              _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | LESS ->
              _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | LESSEQUALS ->
              _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState150 | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | DOT ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | DOUBLEEQUALS ->
              _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | GREATER ->
              _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | GREATEREQUALS ->
              _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | LBRACKET ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | LESS ->
              _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | LESSEQUALS ->
              _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | MINUS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | PLUS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | SEMICOLON ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState149 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState150
              | ID _v ->
                  _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
              | IF ->
                  _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState150
              | INT _v ->
                  _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
              | LPAREN ->
                  _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState150
              | PRINT ->
                  _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState150
              | RETURN ->
                  _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState150
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
          | TIMES ->
              _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState149
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN ->
              _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
    | _ ->
        _menhir_fail ()

and _menhir_goto_base_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146
    else
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | FUN ->
          _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState146
      | LET ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState146
      | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
          _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState146
      | _ ->
          (assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)

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
    | MenhirState142 | MenhirState79 | MenhirState4 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos__1_ _endpos__1_ ->
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.expr) =             ( raise (SyntaxError ("expr", _startpos, _endpos)) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s c ->
    let _v : (Ast.expr) =               ( Const c ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce31 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, v) = _menhir_stack in
    let _v : (Ast.expr) =              ( Var v ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState71 ->
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
            | ATOM ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | ID _v ->
                _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | IF ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INT _v ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | LPAREN ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | PRINT ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | RETURN ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce25 : _menhir_env -> ('ttv_tail * _menhir_state * (string)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, v), _, a) = _menhir_stack in
    let _v : (Ast.expr) =                                 ( Assign(v,a) ) in
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

and _menhir_reduce32 : _menhir_env -> ((((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((((_menhir_stack, _menhir_s), _, cond), _), _, bthen), _), _, belse) = _menhir_stack in
    let _v : (Ast.expr) =                                                          ( If(cond,bthen,belse) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce24 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, a) = _menhir_stack in
    let _v : (Ast.expr) =                      ( Print a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce23 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, a) = _menhir_stack in
    let _v : (Ast.expr) =                       ( Chain(Print a, a) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
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

and _menhir_reduce44 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : (Ast.expr list) =     ( [ x ] ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.decl) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ATOM ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | ID _v ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
          | IF ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | INT _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
          | LPAREN ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | PRINT ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | RETURN ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, t), _, q) = _menhir_stack in
        let _v : ((string * Ast.decl) list) =                              ( t::q ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState134 ->
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
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | RETURN ->
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
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LEFTARROW ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | ID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | IF ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | INT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | ID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | IF ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | INT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | RPAREN ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | COMMA | DIV | DOT | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN | TIMES ->
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | ID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | IF ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | INT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

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
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | LET ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
                _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
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

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_startp _menhir_env._menhir_endp
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
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp _menhir_env._menhir_endp
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
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ATOM ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | ID _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | INT _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | RETURN ->
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

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState137 in
                let _v : (string list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
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
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



