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
  | MenhirState122
  | MenhirState121
  | MenhirState120
  | MenhirState119
  | MenhirState114
  | MenhirState112
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
  | MenhirState98
  | MenhirState97
  | MenhirState96
  | MenhirState95
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState76
  | MenhirState74
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState67
  | MenhirState66
  | MenhirState64
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState47
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
  | MenhirState23
  | MenhirState22
  | MenhirState21
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState13
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState6
  | MenhirState4
  | MenhirState0

  
open Ast


let _eRR =
  Error

let rec _menhir_reduce15 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), b) = _menhir_stack in
    let _v : (Ast.expr) =                                          ( Tuple(a,b,2) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce14 : _menhir_env -> ((('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * (int)) * (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((_menhir_stack, _menhir_s, a), _), b), c) = _menhir_stack in
    let _v : (Ast.expr) =                                                       ( Tuple(a,b,c) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce17 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, a), _) = _menhir_stack in
    let _v : (Ast.expr) =                       ( Tail a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce16 : _menhir_env -> ('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, a), _) = _menhir_stack in
    let _v : (Ast.expr) =                       ( Head a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce20 : _menhir_env -> ('ttv_tail * _menhir_state * (string)) * _menhir_state * (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, f), _, xs0) = _menhir_stack in
    let _v : (Ast.expr) = let args =
      let xs = xs0 in
          ( xs )
    in
                                                                  ( Call(f,args) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run99 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState99
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99

and _menhir_run101 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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
                    _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_run103 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState103
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState103
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
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState105
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
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState107
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | HD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | TL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | IF ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | INT _v ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | LBRACKET ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | LPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_reduce21 : _menhir_env -> ((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((((_menhir_stack, _menhir_s), _, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                             ( Cons(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce19 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s), _, e) = _menhir_stack in
    let _v : (Ast.expr) =                              ( e ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce18 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _ ->
    let ((_menhir_stack, _menhir_s), _, a) = _menhir_stack in
    let _v : (Ast.expr) =                                 ( Print a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState76 | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ast.expr list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((_menhir_stack, _menhir_s, x), _) = _menhir_stack in
        let _v : (Ast.expr list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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
                    _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | HD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | TL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 | MenhirState47 | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState17 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
              | IF ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
              | INT _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
              | LBRACKET ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let (_menhir_stack, _menhir_s, x) = _menhir_stack in
              let _v : (Ast.expr list) =     ( [ x ] ) in
              _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
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
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
          | COMMA | DIV | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | THEN | TIMES ->
              _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
          | COMMA | ELSE | EOF | RACCO | RBRACKET | RPAREN | THEN ->
              _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState23
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | THEN ->
              _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
          | COMMA | DIV | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | THEN | TIMES ->
              _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | THEN ->
              _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | THEN ->
              _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | THEN ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | THEN ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | THEN ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
          | COMMA | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LESS | LESSEQUALS | RACCO | RBRACKET | RPAREN | THEN ->
              _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState49
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState49 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
              | IF ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
              | INT _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
              | LBRACKET ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49
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
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState51 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
              | IF ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
              | INT _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
              | LBRACKET ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
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
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
          | COMMA | ELSE | EOF | RACCO | RBRACKET | RPAREN | THEN ->
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | RBRACKET ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState54 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState56 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
              | IF ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
              | INT _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
              | LBRACKET ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState58
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState56 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
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
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState59 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | COMMA ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState61 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
              | IF ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63
              | INT _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
              | LBRACKET ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState63
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState61 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState61
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | RPAREN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState64 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce21 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState64
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | RBRACKET ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState67 in
              let _ = _menhir_discard _menhir_env in
              _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) _menhir_s
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState67
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | THEN ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState71 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
              | IF ->
                  _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
              | INT _v ->
                  _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
              | LBRACKET ->
                  _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72
              | LPAREN ->
                  _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState71
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | ELSE ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let _menhir_s = MenhirState73 in
              let _menhir_stack = (_menhir_stack, _menhir_s) in
              let _tok = _menhir_discard _menhir_env in
              (match _tok with
              | ID _v ->
                  _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
              | IF ->
                  _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState74
              | INT _v ->
                  _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
              | LBRACKET ->
                  _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState74
              | LPAREN ->
                  _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState74
              | _ ->
                  assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                  _menhir_env._menhir_shifted <- (-1);
                  _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | DOUBLEEQUALS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | GREATER ->
              _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | GREATEREQUALS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | LESS ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | LESSEQUALS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | MINUS ->
              _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | PLUS ->
              _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79
          | FUN | ID _ | IF | INT _ | LPAREN | VAR ->
              _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
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
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
          | DIV | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | TIMES | VAR ->
              _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | DOUBLEEQUALS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | GREATER ->
              _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | GREATEREQUALS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | LESS ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | LESSEQUALS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | MINUS ->
              _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | PLUS ->
              _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState83
          | FUN | ID _ | IF | INT _ | LPAREN | VAR ->
              _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState85
          | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | VAR ->
              _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState96
          | DIV | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | TIMES | VAR ->
              _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState98
          | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LESS | LESSEQUALS | LPAREN | MINUS | PLUS | VAR ->
              _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | MINUS ->
              _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | PLUS ->
              _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState100
          | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LESS | LESSEQUALS | LPAREN | VAR ->
              _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
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
          | DIV ->
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState102
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState102
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState102
          | MINUS ->
              _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState102
          | PLUS ->
              _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState102
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState102
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState102
          | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LESS | LESSEQUALS | LPAREN | VAR ->
              _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | MINUS ->
              _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | PLUS ->
              _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState104
          | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LESS | LESSEQUALS | LPAREN | VAR ->
              _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | MINUS ->
              _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | PLUS ->
              _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState106
          | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LESS | LESSEQUALS | LPAREN | VAR ->
              _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
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
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | MINUS ->
              _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | PLUS ->
              _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState108
          | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LESS | LESSEQUALS | LPAREN | VAR ->
              _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | DOT ->
              _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | DOUBLEEQUALS ->
              _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | GREATER ->
              _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | GREATEREQUALS ->
              _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | LBRACKET ->
              _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | LESS ->
              _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | LESSEQUALS ->
              _menhir_run99 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | MINUS ->
              _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | PLUS ->
              _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | SEMICOLON ->
              _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | TIMES ->
              _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState109
          | FUN | ID _ | IF | INT _ | LPAREN | VAR ->
              _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | DIV ->
              _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | DOT ->
              _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | DOUBLEEQUALS ->
              _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | GREATER ->
              _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | GREATEREQUALS ->
              _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | LBRACKET ->
              _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | LESS ->
              _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | LESSEQUALS ->
              _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | MINUS ->
              _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | PLUS ->
              _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | SEMICOLON ->
              _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | TIMES ->
              _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState121
          | EOF | RACCO ->
              let _menhir_stack = Obj.magic _menhir_stack in
              let ((_menhir_stack, _menhir_s, d), _, r) = _menhir_stack in
              let _v : (Ast.t) =                               ( (d,r) ) in
              let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
              (match _menhir_s with
              | MenhirState119 ->
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
                      _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
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
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | _ ->
        _menhir_fail ()

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos__1_ _endpos__1_ ->
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (Ast.expr) =             ( raise (SyntaxError ("expr", _startpos, _endpos)) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
      _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122
    else
      let _tok = _menhir_env._menhir_token in
      match _tok with
      | FUN ->
          _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState122
      | VAR ->
          _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState122
      | ID _ | IF | INT _ | LBRACKET | LPAREN ->
          _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState122
      | _ ->
          (assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
          _menhir_env._menhir_shifted <- (-1);
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce22 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s c ->
    let _v : (Ast.expr) =               ( Const c ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce23 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, v) = _menhir_stack in
    let _v : (Ast.expr) =              ( Var v ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | IF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | INT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | LBRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | RPAREN ->
            _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | COMMA | DIV | DOT | DOUBLEEQUALS | ELSE | EOF | GREATER | GREATEREQUALS | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | RACCO | RBRACKET | RPAREN | SEMICOLON | THEN | TIMES ->
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_ID_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_ID_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce24 : _menhir_env -> ((((('ttv_tail * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((((((_menhir_stack, _menhir_s), _, cond), _), _, bthen), _), _, belse) = _menhir_stack in
    let _v : (Ast.expr) =                                                          ( If(cond,bthen,belse) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce6 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                               ( Mul(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce25 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                   ( Chain(a, b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce5 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                              ( Add(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                             ( Div(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce8 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                               ( Sub(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce13 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                    ( GreaterEquals(b,a) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce11 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                              ( Greater(b,a) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce12 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                       ( GreaterEquals(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce10 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                 ( Greater(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_reduce9 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, a), _), _, b) = _menhir_stack in
    let _v : (Ast.expr) =                                      ( Equals(a,b) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce2 : _menhir_env -> (('ttv_tail * _menhir_state) * (string)) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s), v), _, e) = _menhir_stack in
    let _v : (string * Ast.decl) =                                  ( (v, DVar e) ) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) _menhir_s _startpos _endpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _endpos__1_ = _endpos in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (string * Ast.decl) =             ( raise (SyntaxError ("decl", _startpos, _endpos)) ) in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.decl) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        if Pervasives.(=) _menhir_env._menhir_shifted (-1) then
          _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120
        else
          let _tok = _menhir_env._menhir_token in
          (match _tok with
          | ID _v ->
              _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
          | IF ->
              _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | INT _v ->
              _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
          | LBRACKET ->
              _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | LPAREN ->
              _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState120
          | _ ->
              assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
              _menhir_env._menhir_shifted <- (-1);
              _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : ((string * Ast.decl) list) =     ( x :: xs ) in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    _menhir_reduce22 _menhir_env (Obj.magic _menhir_stack) _menhir_s _v

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IF ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | INT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LPAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | IF ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | LBRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LPAREN ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | RPAREN ->
            _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | DIV | DOT | DOUBLEEQUALS | FUN | GREATER | GREATEREQUALS | ID _ | IF | INT _ | LBRACKET | LESS | LESSEQUALS | MINUS | PLUS | SEMICOLON | TIMES | VAR ->
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack)

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
                _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | VAR ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | ID _ | IF | INT _ | LBRACKET | LPAREN ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
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

and _menhir_run113 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | RPAREN ->
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
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce12 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp _menhir_env._menhir_endp

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Ast.decl) list) =     ( [] ) in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | EQUALS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | IF ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | INT _v ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
            | LBRACKET ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | LPAREN ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
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

and _menhir_run110 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState112 in
                let _v : (string list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_ID__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
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
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _ | IF | INT _ | LBRACKET | LPAREN ->
        _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



