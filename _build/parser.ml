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
  | MenhirState146
  | MenhirState145
  | MenhirState142
  | MenhirState138
  | MenhirState137
  | MenhirState133
  | MenhirState130
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
  | MenhirState112
  | MenhirState111
  | MenhirState110
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState96
  | MenhirState95
  | MenhirState91
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState81
  | MenhirState80
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState73
  | MenhirState69
  | MenhirState66
  | MenhirState65
  | MenhirState62
  | MenhirState61
  | MenhirState59
  | MenhirState58
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

let rec _menhir_reduce28 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr list) -> 'ttv_return =
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

and _menhir_error129 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string list)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)

and _menhir_error78 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error67 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce26 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
    let _v : (Ast.expr) =                                   ( e ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_error56 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce27 : _menhir_env -> ('ttv_tail * _menhir_state * (string)) * _menhir_state * (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, f), _, xs0) = _menhir_stack in
    let _v : (Ast.expr) = let args =
      let xs = xs0 in
          ( xs )
    in
                                                                  ( Call(f,args) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run110 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run116 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState116
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run118 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_run101 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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

and _menhir_run120 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120

and _menhir_run122 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState122
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122

and _menhir_run124 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState124
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_run107 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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

and _menhir_run112 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState91 | MenhirState12 ->
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
    | MenhirState62 ->
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
    | MenhirState81 ->
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

and _menhir_reduce25 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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

and _menhir_reduce44 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ xs ->
    let ((_menhir_stack, _menhir_s, x), _) = _menhir_stack in
    let _v : (Ast.expr list) =     ( x :: xs ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s l ->
    let _v : (Ast.expr) =                                                    ( chainize(l) ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error56 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error56 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error67 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_error67 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error78 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error78 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error129 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | IN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | FUN ->
                  _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState130
              | LET ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState130
              | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
                  _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState130
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
          | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
              _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error129 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, d), _, r) = _menhir_stack in
        let _v : (Ast.t) =                              ( (d,r) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState137 ->
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
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
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

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | ID _v ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | IF ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | INT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LPAREN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | PRINT ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | RETURN ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | ID _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | INT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | ID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | IF ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RPAREN ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | ATOM | DIV | DOT | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | PRINT | RETURN | SEMICOLON | TIMES ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | ID _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | IF ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | INT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
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
              _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) _menhir_s
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
    | MenhirState91 | MenhirState81 | MenhirState62 | MenhirState48 | MenhirState12 ->
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
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState50 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
              | ID _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
              | IF ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
              | INT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState51 ->
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
          | ELSE ->
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
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState58 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
              | ID _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
              | IF ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62
              | INT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | SEMICOLON ->
              _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58
          | RPAREN ->
              _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState138 | MenhirState4 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | SEMICOLON ->
              _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | EOF | IN | RACCO | RPAREN ->
              _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState66
          | COMMA | ELSE | EOF | IN | RACCO | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState80 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
              | ID _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
              | IF ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81
              | INT _v ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | SEMICOLON ->
              _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState80
          | RPAREN ->
              _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState86 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
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
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState86
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
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
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState88 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | ID _v ->
                  _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
              | IF ->
                  _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | INT _v ->
                  _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
              | LPAREN ->
                  _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | PRINT ->
                  _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | RETURN ->
                  _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState89
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
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
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState88
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | DOT ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | DOUBLEEQUALS ->
              _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | GREATER ->
              _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | GREATEREQUALS ->
              _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | LBRACKET ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | LESS ->
              _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | LESSEQUALS ->
              _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | MINUS ->
              _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | PLUS ->
              _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState96 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | TIMES ->
              _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | DOUBLEEQUALS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | GREATER ->
              _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | GREATEREQUALS ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | LESS ->
              _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | LESSEQUALS ->
              _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | MINUS ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | PLUS ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | ATOM | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON | TIMES ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState113
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState113
          | ATOM | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON | TIMES ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState115
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState117
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState117
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState117
          | MINUS ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState117
          | PLUS ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState117
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState117
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState119
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState119
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState119
          | MINUS ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState119
          | PLUS ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState119
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState119
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | MINUS ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | PLUS ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | MINUS ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | PLUS ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState123
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState125
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState125
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState125
          | MINUS ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState125
          | PLUS ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState125
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState125
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | DOUBLEEQUALS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | GREATER ->
              _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | GREATEREQUALS ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | LESS ->
              _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | LESSEQUALS ->
              _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | MINUS ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | PLUS ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState126
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | DOUBLEEQUALS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | GREATER ->
              _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | GREATEREQUALS ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | LESS ->
              _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | LESSEQUALS ->
              _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | MINUS ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | PLUS ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState146 | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | DOT ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | DOUBLEEQUALS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | GREATER ->
              _menhir_run122 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | GREATEREQUALS ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | LBRACKET ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | LESS ->
              _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | LESSEQUALS ->
              _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | MINUS ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | PLUS ->
              _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | SEMICOLON ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState145 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState146
              | ID _v ->
                  _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
              | IF ->
                  _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState146
              | INT _v ->
                  _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
              | LPAREN ->
                  _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState146
              | PRINT ->
                  _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState146
              | RETURN ->
                  _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState146
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
          | TIMES ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState145
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN ->
              _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | _ ->
        _menhir_fail ()

and _menhir_goto_base_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142
    else
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | FUN ->
          _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState142
      | LET ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState142
      | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
          _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState142
      | _ ->
          (assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)

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
    | MenhirState138 | MenhirState77 | MenhirState4 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce32 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos__1_ _endpos__1_ ->
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.expr) =             ( raise (SyntaxError ("expr", _startpos, _endpos)) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce29 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s c ->
    let _v : (Ast.expr) =               ( Const c ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce30 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, v) = _menhir_stack in
    let _v : (Ast.expr) =              ( Var v ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState69 ->
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
                _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | ID _v ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | IF ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INT _v ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | LPAREN ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | PRINT ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | RETURN ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce31 : _menhir_env -> ((((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
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
    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

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

and _menhir_reduce43 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : (Ast.expr list) =     ( [ x ] ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.decl) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ATOM ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState138
          | ID _v ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
          | IF ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState138
          | INT _v ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
          | LPAREN ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState138
          | PRINT ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState138
          | RETURN ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState138
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, t), _, q) = _menhir_stack in
        let _v : ((string * Ast.decl) list) =                              ( t::q ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState130 ->
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
    _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

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
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | COMMA | DIV | DOT | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RPAREN | SEMICOLON | THEN | TIMES ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

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
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | LET ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
                _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
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

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
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
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState50 ->
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
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState133 in
                let _v : (string list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
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
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ATOM | ID _ | IF | INT _ | LPAREN | PRINT | RETURN ->
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



