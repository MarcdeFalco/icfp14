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
  | MenhirState142
  | MenhirState141
  | MenhirState138
  | MenhirState134
  | MenhirState133
  | MenhirState129
  | MenhirState126
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
  | MenhirState112
  | MenhirState111
  | MenhirState110
  | MenhirState109
  | MenhirState108
  | MenhirState107
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState93
  | MenhirState92
  | MenhirState88
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState78
  | MenhirState77
  | MenhirState74
  | MenhirState73
  | MenhirState71
  | MenhirState67
  | MenhirState64
  | MenhirState61
  | MenhirState60
  | MenhirState58
  | MenhirState57
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState47
  | MenhirState46
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
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState13
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

let rec _menhir_reduce27 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr list) -> 'ttv_return =
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

and _menhir_error125 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string list)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)

and _menhir_error75 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error65 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce25 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
    let _v : (Ast.expr) =                                   ( e ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_error55 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce26 : _menhir_env -> ('ttv_tail * _menhir_state * (string)) * _menhir_state * (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, f), _, xs0) = _menhir_stack in
    let _v : (Ast.expr) = let args =
      let xs = xs0 in
          ( xs )
    in
                                                                  ( Call(f,args) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run111 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run113 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState113
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113

and _menhir_run115 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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

and _menhir_run117 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState117
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
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState119
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState119
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
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_run104 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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

and _menhir_run109 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState88 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, x), _), _, xs) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_reduce24 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s), _, a) = _menhir_stack in
    let _v : (Ast.expr) =                                   ( Atom a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
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
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
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
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
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
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
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

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
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
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
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
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
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

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
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

and _menhir_reduce43 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ xs ->
    let ((_menhir_stack, _menhir_s, x), _) = _menhir_stack in
    let _v : (Ast.expr list) =     ( x :: xs ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s l ->
    let _v : (Ast.expr) =                                                    ( chainize(l) ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error55 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error55 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error65 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_error65 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error75 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error75 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error125 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | IN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | FUN ->
                  _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState126
              | LET ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState126
              | ATOM | ID _ | IF | INT _ | LPAREN | PRINT ->
                  _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState126
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
          | ATOM | ID _ | IF | INT _ | LPAREN | PRINT ->
              _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error125 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, d), _, r) = _menhir_stack in
        let _v : (Ast.t) =                              ( (d,r) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState133 ->
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
            _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
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

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ID _v ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | INT _v ->
        _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LPAREN ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | PRINT ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState74
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run81 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | ID _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IF ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INT _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | PRINT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | ID _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | IF ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PRINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | RPAREN ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | ATOM | DIV | DOT | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | PRINT | SEMICOLON | TIMES ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ID _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | IF ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PRINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
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
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState14 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
          | COMMA | DIV | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN | TIMES ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
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
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28
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
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
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
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32
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
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34
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
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36
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
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38
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
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40
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
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState88 | MenhirState78 | MenhirState61 | MenhirState47 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState46 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, x) = _menhir_stack in
              let _v : (Ast.expr list) =     ( [ x ] ) in
              _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState49 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState51 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState57 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61
              | ID _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
              | IF ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61
              | INT _v ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
              | LPAREN ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61
              | PRINT ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | SEMICOLON ->
              _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | RPAREN ->
              _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState134 | MenhirState4 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | SEMICOLON ->
              _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60
          | EOF | IN | RACCO | RPAREN ->
              _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState77 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState78
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
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | SEMICOLON ->
              _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState77
          | RPAREN ->
              _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState83 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState84
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
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState85 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState86
              | ID _v ->
                  _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
              | IF ->
                  _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState86
              | INT _v ->
                  _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
              | LPAREN ->
                  _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState86
              | PRINT ->
                  _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState86
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | DOT ->
              _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | DOUBLEEQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | GREATER ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | GREATEREQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | LBRACKET ->
              _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | LESS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | LESSEQUALS ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | MINUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | PLUS ->
              _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState93 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | TIMES ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState93
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | DOUBLEEQUALS ->
              _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | GREATER ->
              _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | GREATEREQUALS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | LESS ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | LESSEQUALS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | MINUS ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | PLUS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState95
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState97
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState97
          | ATOM | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | SEMICOLON | TIMES ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | SEMICOLON ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
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
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState110
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState110
          | ATOM | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | SEMICOLON | TIMES ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState112
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | SEMICOLON ->
              _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | MINUS ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | PLUS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState114
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
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
          | DIV ->
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState116
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState116
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState116
          | MINUS ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState116
          | PLUS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState116
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState116
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | MINUS ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | PLUS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState118
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | MINUS ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | PLUS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | MINUS ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | PLUS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState122
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | DOUBLEEQUALS ->
              _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | GREATER ->
              _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | GREATEREQUALS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | LESS ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | LESSEQUALS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | MINUS ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | PLUS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | SEMICOLON ->
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState142 | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | DOT ->
              _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | DOUBLEEQUALS ->
              _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | GREATER ->
              _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | GREATEREQUALS ->
              _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | LBRACKET ->
              _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | LESS ->
              _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | LESSEQUALS ->
              _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | MINUS ->
              _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | PLUS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | SEMICOLON ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState141 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState142
              | ID _v ->
                  _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
              | IF ->
                  _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState142
              | INT _v ->
                  _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
              | LPAREN ->
                  _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState142
              | PRINT ->
                  _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState142
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
          | TIMES ->
              _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT ->
              _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | _ ->
        _menhir_fail ()

and _menhir_goto_base_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138
    else
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | FUN ->
          _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState138
      | LET ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState138
      | ATOM | ID _ | IF | INT _ | LPAREN | PRINT ->
          _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState138
      | _ ->
          (assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)

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
    | MenhirState134 | MenhirState74 | MenhirState4 | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos__1_ _endpos__1_ ->
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.expr) =             ( raise (SyntaxError ("expr", _startpos, _endpos)) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s c ->
    let _v : (Ast.expr) =               ( Const c ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce29 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, v) = _menhir_stack in
    let _v : (Ast.expr) =              ( Var v ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState67 ->
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
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | ID _v ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | IF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | INT _v ->
                _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | LPAREN ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | PRINT ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce30 : _menhir_env -> ((((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
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
    _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM | ID _ | IF | INT _ | LPAREN | PRINT ->
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

and _menhir_reduce42 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : (Ast.expr list) =     ( [ x ] ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.decl) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ATOM ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState134
          | ID _v ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
          | IF ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState134
          | INT _v ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
          | LPAREN ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState134
          | PRINT ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState134
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, t), _, q) = _menhir_stack in
        let _v : ((string * Ast.decl) list) =                              ( t::q ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState126 ->
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6
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
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7
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
    _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
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
        | ATOM ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
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
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | COMMA | DIV | DOT | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN | TIMES ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | ID _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | IF ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | INT _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | PRINT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

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
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | LET ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | ATOM | ID _ | IF | INT _ | LPAREN | PRINT ->
                _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
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

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
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
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
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
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp _menhir_env._menhir_endp
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
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ATOM ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4
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

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState129 in
                let _v : (string list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
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
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ATOM | ID _ | IF | INT _ | LPAREN | PRINT ->
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



