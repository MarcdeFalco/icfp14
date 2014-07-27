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
  | MenhirState161
  | MenhirState160
  | MenhirState157
  | MenhirState153
  | MenhirState152
  | MenhirState148
  | MenhirState145
  | MenhirState142
  | MenhirState141
  | MenhirState140
  | MenhirState139
  | MenhirState138
  | MenhirState137
  | MenhirState136
  | MenhirState135
  | MenhirState134
  | MenhirState133
  | MenhirState132
  | MenhirState131
  | MenhirState130
  | MenhirState129
  | MenhirState128
  | MenhirState127
  | MenhirState126
  | MenhirState125
  | MenhirState124
  | MenhirState113
  | MenhirState112
  | MenhirState111
  | MenhirState109
  | MenhirState108
  | MenhirState106
  | MenhirState103
  | MenhirState101
  | MenhirState100
  | MenhirState99
  | MenhirState98
  | MenhirState97
  | MenhirState93
  | MenhirState90
  | MenhirState89
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState81
  | MenhirState77
  | MenhirState74
  | MenhirState73
  | MenhirState70
  | MenhirState69
  | MenhirState64
  | MenhirState63
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState46
  | MenhirState45
  | MenhirState44
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
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState16
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

let rec listize l =
    match l with
    | [] -> Const 0
    | t::q -> Cons(t, listize(q))

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

let rec _menhir_reduce25 : _menhir_env -> ((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr list) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos__5_ ->
    let ((((_menhir_stack, _menhir_s, _startpos__1_), _, a, _startpos_a_, _endpos_a_), _), _, l) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__5_ in
    let _v : (Ast.expr) =                                                                             ( consize(a::l) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce14 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * (int) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos__4_ ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _, _startpos__2_), b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos__4_ in
    let _v : (Ast.expr) =                                          ( Tuple(a,b,2) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce13 : _menhir_env -> ((('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * (int) * Lexing.position * Lexing.position) * (int) * Lexing.position * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos__6_ ->
    let ((((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _, _startpos__2_), b, _startpos_b_, _endpos_b_), c, _startpos_c_, _endpos_c_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos__6_ in
    let _v : (Ast.expr) =                                                       ( Tuple(a,b,c) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce16 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos__3_ ->
    let ((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos__3_ in
    let _v : (Ast.expr) =                       ( Tail a ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce15 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos__3_ ->
    let ((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos__3_ in
    let _v : (Ast.expr) =                       ( Head a ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run112 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run124 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_run128 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128

and _menhir_run130 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130

and _menhir_run132 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_endp in
                    let _ = _menhir_discard _menhir_env in
                    _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _endpos
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _), _, _, _), _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run134 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134

and _menhir_run136 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_run138 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138

and _menhir_run120 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | HD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) _endpos
    | TL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run126 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState103 | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _), _, xs) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_reduce22 : _menhir_env -> (('ttv_tail * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _ _endpos__4_ ->
    let (((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, a, _startpos_a_, _endpos_a_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__4_ in
    let _v : (Ast.expr) =                                   ( Atom a ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | PIPE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _startpos = _menhir_env._menhir_startp in
                let _endpos = _menhir_env._menhir_endp in
                let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_endp in
                    let _ = _menhir_discard _menhir_env in
                    _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) _endpos
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _), _, _, _), _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack) _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | HD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack) _endpos
    | TL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack) _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_reduce32 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string list)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), _, vl), _, e) = _menhir_stack in
    let _v : ((string * Ast.decl) list) =                                                                       ( split_var_tuple vl e ) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_error144 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string list)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)

and _menhir_error87 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_error75 : _menhir_env -> (('ttv_tail * _menhir_state) * _menhir_state * (string) * Lexing.position * Lexing.position) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce23 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Ast.expr) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos__3_ ->
    let ((_menhir_stack, _menhir_s, _startpos__1_), _, e) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__3_ in
    let _v : (Ast.expr) =                                   ( e ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_error67 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce18 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Ast.expr list) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos__3_ ->
    let ((_menhir_stack, _menhir_s, _startpos__1_), _, xs0) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__3_ in
    let _v : (Ast.expr) = let l =
      let xs = xs0 in
          ( xs )
    in
                                                                ( listize(l) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce24 : _menhir_env -> (('ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * (Ast.expr list) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos__4_ ->
    let (((_menhir_stack, _menhir_s, f, _startpos_f_, _endpos_f_), _startpos__2_), _, xs0) = _menhir_stack in
    let _startpos = _startpos_f_ in
    let _endpos = _endpos__4_ in
    let _v : (Ast.expr) = let args =
      let xs = xs0 in
          ( xs )
    in
                                                                  ( Call(f,args) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s a _startpos_a_ _endpos_a_ ->
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_a_ in
    let _v : (Ast.expr) =                  ( SetFileInfo(a, _startpos.pos_cnum, _endpos.pos_cnum) ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState17 in
              let _endpos = _menhir_env._menhir_endp in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack) _menhir_s _endpos
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp
          | COMMA | DIV | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN | TIMES ->
              _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState32
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
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
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_startp
          | COMMA | DIV | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN | TIMES ->
              _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState103 | MenhirState90 | MenhirState70 | MenhirState51 | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState50 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
              | ID _v ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | IF ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
              | INT _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | LBRACKET ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
              let _v : (Ast.expr list) =     ( [ x ] ) in
              _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | COMMA | ELSE | EOF | IN | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState55 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
              | ID _v ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | IF ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
              | INT _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | LBRACKET ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState57 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
              | ID _v ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | IF ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
              | INT _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | LBRACKET ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | COMMA | ELSE | EOF | IN | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState153 | MenhirState93 | MenhirState4 | MenhirState64 | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | SEMICOLON ->
              _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState63
          | EOF | IN | RACCO | RBRACKET | RPAREN ->
              _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState69 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
              | ID _v ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | IF ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
              | INT _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | LBRACKET ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | SEMICOLON ->
              _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState69
          | RPAREN ->
              _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | COMMA | ELSE | EOF | IN | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState74
          | COMMA | ELSE | EOF | IN | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN ->
              _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState89 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
              | ID _v ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | IF ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
              | INT _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | LBRACKET ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | SEMICOLON ->
              _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState89
          | RPAREN ->
              _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState98 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
              | ID _v ->
                  _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | IF ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
              | INT _v ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | LBRACKET ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
              | PRINT ->
                  _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
              | RETURN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState98
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
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState100 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
              | ID _v ->
                  _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | IF ->
                  _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
              | INT _v ->
                  _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | LBRACKET ->
                  _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
              | LPAREN ->
                  _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
              | PRINT ->
                  _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
              | RETURN ->
                  _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | DOT ->
              _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | LBRACKET ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | PLUS ->
              _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState109 in
              let _endpos = _menhir_env._menhir_endp in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack) _menhir_s _endpos
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | DOUBLEEQUALS ->
              _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | GREATER ->
              _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | GREATEREQUALS ->
              _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | LESSEQUALS ->
              _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState111
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState113
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_startp
          | ATOM | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON | TIMES ->
              _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState125
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState125
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_startp
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState125
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState127
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_startp
          | ATOM | DIV | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON | TIMES ->
              _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _menhir_env._menhir_startp
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState129
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState131
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState133
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState135
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState135
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState135
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState135
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState135
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState137
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState137
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState137
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState137
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState137
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState139
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState139
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _menhir_env._menhir_startp
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState139
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState139
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState139
          | ATOM | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LESS | LESSEQUALS | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | DOUBLEEQUALS ->
              _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | GREATER ->
              _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | GREATEREQUALS ->
              _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | LESSEQUALS ->
              _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState140
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | DOUBLEEQUALS ->
              _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | GREATER ->
              _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | GREATEREQUALS ->
              _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | LESSEQUALS ->
              _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState141
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | DOUBLEEQUALS ->
              _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | GREATER ->
              _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | GREATEREQUALS ->
              _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | LESSEQUALS ->
              _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState142
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN | SEMICOLON ->
              _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
    | MenhirState161 | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run126 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | DOT ->
              _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | DOUBLEEQUALS ->
              _menhir_run138 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | GREATER ->
              _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | GREATEREQUALS ->
              _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | LBRACKET ->
              _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_startp
          | LESS ->
              _menhir_run132 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | LESSEQUALS ->
              _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | MINUS ->
              _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | PLUS ->
              _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | SEMICOLON ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState160 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ATOM ->
                  _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
              | ID _v ->
                  _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | IF ->
                  _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
              | INT _v ->
                  _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
              | LBRACKET ->
                  _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
              | LPAREN ->
                  _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
              | PRINT ->
                  _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
              | RETURN ->
                  _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
          | TIMES ->
              _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState160
          | ATOM | ID _ | IF | IN | INT _ | LPAREN | PRINT | RETURN ->
              _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
    | _ ->
        _menhir_fail ()

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s l ->
    let _v : (Ast.expr) =                                                    ( chainize(l) ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error67 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _endpos = _menhir_env._menhir_endp in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) _endpos
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error67 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error75 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | IN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _ = _menhir_discard _menhir_env in
              let _menhir_stack = Obj.magic _menhir_stack in
              let (((_menhir_stack, _menhir_s), _, v, _startpos_v_, _endpos_v_), _, e) = _menhir_stack in
              let _v : (string * Ast.decl) =                                          ( (v, DVar e) ) in
              _menhir_goto_base_decl _menhir_env _menhir_stack _menhir_s _v
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error75 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error87 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _endpos = _menhir_env._menhir_endp in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) _endpos
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error87 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_error144 _menhir_env (Obj.magic _menhir_stack)
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | IN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | FUN ->
                  _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState145
              | LET ->
                  _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState145
              | ATOM | ID _ | IF | INT _ | LBRACKET | LPAREN | PRINT | RETURN ->
                  _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState145
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
          | ATOM | ID _ | IF | INT _ | LBRACKET | LPAREN | PRINT | RETURN ->
              _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_error144 _menhir_env (Obj.magic _menhir_stack))
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, d), _, r) = _menhir_stack in
        let _v : (Ast.t) =                              ( (d,r) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState152 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RACCO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), f, _startpos_f_, _endpos_f_), _startpos__3_), _, xs0), _endpos__5_), _, b) = _menhir_stack in
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

and _menhir_reduce50 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _ xs ->
    let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _) = _menhir_stack in
    let _v : (Ast.expr list) =     ( x :: xs ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) _endpos
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
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState85 in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) _menhir_s _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run93 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp
    | RBRACKET ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v _startpos _endpos

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LEFTARROW ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
        | ID _v ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LBRACKET ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
        | LPAREN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp
        | RPAREN ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | ATOM | DIV | DOT | DOUBLEEQUALS | GREATER | GREATEREQUALS | ID _ | IF | IN | INT _ | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | PRINT | RETURN | SEMICOLON | TIMES ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_bexpr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    match _menhir_s with
    | MenhirState153 | MenhirState97 | MenhirState99 | MenhirState108 | MenhirState103 | MenhirState93 | MenhirState85 | MenhirState90 | MenhirState4 | MenhirState6 | MenhirState7 | MenhirState8 | MenhirState70 | MenhirState10 | MenhirState64 | MenhirState12 | MenhirState56 | MenhirState58 | MenhirState53 | MenhirState14 | MenhirState51 | MenhirState16 | MenhirState45 | MenhirState43 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState31 | MenhirState33 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v _startpos _endpos
    | MenhirState81 | MenhirState161 | MenhirState83 | MenhirState84 | MenhirState101 | MenhirState106 | MenhirState138 | MenhirState136 | MenhirState134 | MenhirState132 | MenhirState130 | MenhirState128 | MenhirState124 | MenhirState126 | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v _startpos _endpos
    | _ ->
        _menhir_fail ()

and _menhir_goto_base_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157
    else
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | FUN ->
          _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState157
      | LET ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState157
      | ATOM | ID _ | IF | INT _ | LBRACKET | LPAREN | PRINT | RETURN ->
          _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState157
      | _ ->
          (assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157)

and _menhir_reduce35 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, _startpos__1_, _endpos__1_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : ((string * Ast.decl) list) =             ( raise (SyntaxError ("decl", _startpos, _endpos)) ) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState93 | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState153 | MenhirState85 | MenhirState4 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce29 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos__1_ _endpos__1_ ->
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.expr) =             ( raise (SyntaxError ("expr", _startpos, _endpos)) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce17 : _menhir_env -> 'ttv_tail * _menhir_state * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _ _endpos__2_ ->
    let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__2_ in
    let _v : (Ast.expr) =                     ( Nop ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_SEMICOLON_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s c _startpos_c_ _endpos_c_ ->
    let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : (Ast.expr) =               ( Const c ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce27 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, v, _startpos_v_, _endpos_v_) = _menhir_stack in
    let _startpos = _startpos_v_ in
    let _endpos = _endpos_v_ in
    let _v : (Ast.expr) =              ( Var v ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_), _, xs) = _menhir_stack in
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
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
            | ID _v ->
                _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
            | INT _v ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LBRACKET ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
            | LPAREN ->
                _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
            | PRINT ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce21 : _menhir_env -> ('ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, v, _startpos_v_, _endpos_v_), _, a, _startpos_a_, _endpos_a_) = _menhir_stack in
    let _startpos = _startpos_v_ in
    let _endpos = _endpos_a_ in
    let _v : (Ast.expr) =                                 ( Assign(v,a) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce5 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _), _, b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.expr) =                               ( Mul(a,b) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce4 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _), _, b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.expr) =                               ( Add(a,b) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce6 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _), _, b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.expr) =                             ( Div(a,b) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce7 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _), _, b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.expr) =                               ( Sub(a,b) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce12 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _), _, b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.expr) =                                    ( GreaterEquals(b,a) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce10 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _), _, b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.expr) =                              ( Greater(b,a) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce11 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _), _, b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.expr) =                                       ( GreaterEquals(a,b) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce9 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _), _, b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.expr) =                                 ( Greater(a,b) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce8 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a, _startpos_a_, _endpos_a_), _), _, b, _startpos_b_, _endpos_b_) = _menhir_stack in
    let _startpos = _startpos_a_ in
    let _endpos = _endpos_b_ in
    let _v : (Ast.expr) =                                      ( Equals(a,b) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce28 : _menhir_env -> ((((('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position) * _menhir_state) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((((_menhir_stack, _menhir_s, _startpos__1_), _, cond, _startpos_cond_, _endpos_cond_), _), _, bthen, _startpos_bthen_, _endpos_bthen_), _), _, belse, _startpos_belse_, _endpos_belse_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_belse_ in
    let _v : (Ast.expr) =                                                          ( If(cond,bthen,belse) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce20 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _startpos__1_), _, a, _startpos_a_, _endpos_a_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_a_ in
    let _v : (Ast.expr) =                      ( Print a ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_reduce19 : _menhir_env -> ('ttv_tail * _menhir_state * Lexing.position) * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, _startpos__1_), _, a, _startpos_a_, _endpos_a_) = _menhir_stack in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos_a_ in
    let _v : (Ast.expr) =                       ( Chain(Print a, a) ) in
    _menhir_goto_bexpr _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM | ID _ | IF | INT _ | LBRACKET | LPAREN | PRINT | RETURN ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
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
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce49 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
    let _v : (Ast.expr list) =     ( [ x ] ) in
    _menhir_goto_separated_nonempty_list_SEMICOLON_expr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.decl) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ATOM ->
              _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_startp
          | ID _v ->
              _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
          | IF ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_startp
          | INT _v ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
          | LBRACKET ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_startp
          | LPAREN ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_startp
          | PRINT ->
              _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_startp
          | RETURN ->
              _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_startp
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, t), _, q) = _menhir_stack in
        let _v : ((string * Ast.decl) list) =                              ( t::q ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, vl), _, e), _, q) = _menhir_stack in
        let _v : ((string * Ast.decl) list) =                                                                                   ( split_var_tuple vl e @ q ) in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState8 in
        let _endpos = _menhir_env._menhir_endp in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) _menhir_s _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_startp
    | RBRACKET ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v _startpos _endpos

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ATOM ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | ID _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | IF ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | INT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | PRINT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | RETURN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LEFTARROW ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
        | RPAREN ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
    | COMMA | DIV | DOT | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | IN | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN | TIMES ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ATOM ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | ID _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | IF ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | INT _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | PRINT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | RETURN ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (string) * Lexing.position * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LACCO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FUN ->
                _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | LET ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | ATOM | ID _ | IF | INT _ | LBRACKET | LPAREN | PRINT | RETURN ->
                _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
    | EQUALS | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x, _startpos_x_, _endpos_x_) = _menhir_stack in
        let _v : (string list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
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
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState157 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState157 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp _menhir_env._menhir_endp
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

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | COMMA ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ATOM ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_startp
            | ID _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | IF ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_startp
            | INT _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LBRACKET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_startp
            | LPAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_startp
            | PRINT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run146 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState148 in
                let _v : (string list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _, _) = _menhir_stack in
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
        _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ATOM | ID _ | IF | INT _ | LBRACKET | LPAREN | PRINT | RETURN ->
        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



