open Common

let tickcount = ref 0

let tick_lambda_normal = 127
let tick_lambda_eating = 137
let tick_ghosts = [| 130; 132; 134; 136 |]
let tick_ghosts_fright = [| 195; 198; 201; 204 |]

type lambdaman = {
    mutable vit : int;
    mutable pos : pos;
    mutable lives : int;
    mutable dir : direction;
    mutable score : int;
    mac : Gccsim.machine;
    mutable state : Gcc.data
    }

let lambdaman = { vit = 0; pos = { x=0; y=0 };
    lives = 3; dir = UP; score = 0; mac = Gccsim.dummy_machine (); state =
        Gcc.Int Int32.zero }

let ghosts : Ghcsim.ghost array ref = ref [||]

let fruit = ref 0

let map = ref [||]

let load_sim lambdaman_code text_map =
    ()

let show_map () =
    for j = 0 to Array.length !map - 1 do
        for i = 0 to Array.length !map.(j) - 1 do
            if lambdaman.pos = {x=i;y=j}
            then print_char '\\'
            else begin
                let ghost_here = ref false in
                Array.iter (fun g ->
                    if g.Ghcsim.pos = {x=i;y=j} && not !ghost_here
                    then begin
                        print_char (match g.Ghcsim.vit with
                            Ghcsim.Standard -> '=' | Ghcsim.Invisible -> 'i'
                            | _ -> 'X') ; ghost_here := true
                    end) !ghosts;
                if not !ghost_here
                then print_char (match !map.(j).(i) with
                Wall -> '#' | Pill -> '.'
                | Powerpill -> 'o'
                | Fruit when !fruit > 0 -> '%'
                | _ -> ' ')
            end
        done;
        print_newline ()
    done;
    Printf.printf "Score: %d Lives: %d Ticks: %d\n" lambdaman.score lambdaman.lives
    !tickcount

let load_map fn =
    let fmap = open_in fn in
    let lines = ref [] in    
    let lines = try
            while true do
                let l = input_line fmap in
                lines := l :: !lines
            done;
            failwith "Out of reach"
    with End_of_file -> Array.of_list (List.rev !lines) in
    map := Array.make_matrix
        (Array.length lines) (String.length lines.(0)) Wall;
    for y = 0 to Array.length lines - 1 do
        for x = 0 to String.length lines.(0) - 1 do
            !map.(y).(x) <- match lines.(y).[x] with
                | '#' -> Wall | ' ' -> Empty
                | '.' -> Pill | 'o' -> Powerpill | '%' -> Fruit
                | '\\' -> LStart | '=' -> GStart
                | _ -> failwith "Unknown cell"
        done
    done

let init_lambdaman code =
    Gccsim.init_machine lambdaman.mac code;
    for y = 0 to Array.length !map - 1 do
        for x = 0 to Array.length !map.(0) - 1 do
            if !map.(y).(x) = LStart
            then lambdaman.pos <- {x=x;y=y}
        done
    done;
    lambdaman.vit <- 0;
    lambdaman.lives <- 3;
    lambdaman.dir <- UP;
    lambdaman.score <- 0;
    lambdaman.pos

let init_ghosts codes =
    let nghosts = ref 0 in
    for y = 0 to Array.length !map - 1 do
        for x = 0 to Array.length !map.(0) - 1 do
            if !map.(y).(x) = GStart
            then incr nghosts
        done
    done;
    ghosts := Array.make !nghosts (Ghcsim.init_ghost {x=0;y=0} codes.(0));
    let ghosts_starting_pos = Array.make !nghosts {x=0;y=0} in
    let ncodes = Array.length codes in
    let i = ref 0 in
    for y = 0 to Array.length !map - 1 do
        for x = 0 to Array.length !map.(0) - 1 do
            if !map.(y).(x) = GStart
            then begin
                !ghosts.(!i) <- Ghcsim.init_ghost {x=x;y=y} codes.(!i mod ncodes);
                ghosts_starting_pos.(!i) <- {x=x;y=y};
                incr i
            end
        done
    done;
    ghosts_starting_pos

let encode_arg a =
    match a with
    | Ghc.Reg n -> Gcc.Cons(Gcc.Int Int32.zero, Gcc.Int (Int32.of_int n))
    | Ghc.IReg n -> Gcc.Cons(Gcc.Int Int32.one, Gcc.Int (Int32.of_int n))
    | Ghc.Const n -> Gcc.Cons(Gcc.Int (Int32.of_int 2), Gcc.Int (Int32.of_int n))
    | Ghc.Mem n -> Gcc.Cons(Gcc.Int (Int32.of_int 3), Gcc.Int (Int32.of_int n))

let encode_int n = Gcc.Int (Int32.of_int n)

let encode_ghost_code g =
    let gcccode = ref (Gcc.Int Int32.zero) in
    for i = Array.length g - 1 downto 0 do
        let instr = g.(i) in
        let gccinstr = match instr with
        | Ghc.MOV(a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 0), 
            Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero)))
        | Ghc.INC a -> Gcc.Cons(Gcc.Int (Int32.of_int 1),  
            Gcc.Cons(encode_arg a, Gcc.Int Int32.zero))
        | Ghc.DEC a -> Gcc.Cons(Gcc.Int (Int32.of_int 2),  
            Gcc.Cons(encode_arg a, Gcc.Int Int32.zero))
        | Ghc.ADD(a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 3), 
            Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero)))
        | Ghc.SUB(a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 4), 
            Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero)))
        | Ghc.MUL(a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 5), 
            Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero)))
        | Ghc.DIV(a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 6), 
            Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero)))
        | Ghc.AND(a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 7), 
            Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero)))
        | Ghc.OR(a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 8), 
            Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero)))
        | Ghc.XOR(a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 9), 
            Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero)))
        | Ghc.JLT(n,a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 10), 
            Gcc.Cons(encode_int n, Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero))))
        | Ghc.JEQ(n,a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 11), 
            Gcc.Cons(encode_int n, Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero))))
        | Ghc.JGT(n,a,b) -> Gcc.Cons(Gcc.Int (Int32.of_int 12), 
            Gcc.Cons(encode_int n, Gcc.Cons(encode_arg a, Gcc.Cons(encode_arg b, Gcc.Int Int32.zero))))
        | Ghc.INT n -> Gcc.Cons(Gcc.Int (Int32.of_int 13),  
            Gcc.Cons(encode_int n, Gcc.Int Int32.zero))
        | Ghc.HLT -> Gcc.Cons(Gcc.Int (Int32.of_int 14),  Gcc.Int Int32.zero)
        in gcccode := Gcc.Cons(gccinstr, !gcccode)
    done;
    !gcccode

let encode_ghosts_code ghosts_code =
    let gccgc = ref (Gcc.Int Int32.zero) in
    for i = Array.length ghosts_code - 1 downto 0 do
        let g = ghosts_code.(i) in
        gccgc := Gcc.Cons(encode_ghost_code g, !gccgc)
    done;
    !gccgc

let encode_world () =
    let gccmap = ref (Gcc.Int Int32.zero) in
    for y = Array.length !map - 1 downto 0 do
        let line = ref (Gcc.Int Int32.zero) in
        for x = Array.length !map.(0) - 1 downto 0 do
            let c = Common.int_of_cell !map.(y).(x) in
            line := Gcc.Cons(Gcc.Int (Int32.of_int c), !line)
        done;
        gccmap := Gcc.Cons(!line, !gccmap)
    done;

    let gcclam = Gcc.Cons(Gcc.Int (Int32.of_int (if lambdaman.vit > 0
                then lambdaman.vit - !tickcount else 0)),
        Gcc.Cons(Gcc.Cons(Gcc.Int (Int32.of_int lambdaman.pos.x),
            Gcc.Int (Int32.of_int lambdaman.pos.y)),
        Gcc.Cons(Gcc.Int (Int32.of_int (Common.int_of_dir lambdaman.dir)),
        Gcc.Cons(Gcc.Int (Int32.of_int lambdaman.lives),
        Gcc.Int (Int32.of_int lambdaman.score))))) in

    let gccghs = ref (Gcc.Int Int32.zero) in
    for i = Array.length !ghosts - 1 downto 0 do
        let g = !ghosts.(i) in
        let gccgh = Gcc.Cons(Gcc.Int (Int32.of_int (Ghcsim.int_of_vit
        g.Ghcsim.vit)),
        Gcc.Cons(Gcc.Cons(Gcc.Int (Int32.of_int g.Ghcsim.pos.x),
            Gcc.Int (Int32.of_int g.Ghcsim.pos.y)),
        Gcc.Int (Int32.of_int (Common.int_of_dir g.Ghcsim.dir)))) in
        gccghs := Gcc.Cons( gccgh, !gccghs )
    done;

    let gccfruit = Gcc.Int (Int32.of_int (if !fruit > 0 then !fruit -
    !tickcount else 0)) in

    Gcc.Cons( !gccmap, Gcc.Cons( gcclam, Gcc.Cons( !gccghs, gccfruit)))

let load_matrix_from_data f frame_data off mapX mapY =
    let map = Array.make mapY [||] in
    for y = 0 to mapY - 1 do
        let l = Gccsim.data_to_list f frame_data.(y+256*off) in
        let v = Array.of_list l in
        map.(y) <- v
    done;
    map
 
let load_map_from_data frame_data off mapX mapY =
    let map = Array.make_matrix mapY mapX Wall in
    for y = 0 to mapY - 1 do
        let l = Gccsim.data_to_list Gccsim.data_to_int frame_data.(y+256*off) in
        let v = Array.of_list l in

        for x = 0 to mapX - 1 do
            map.(y).(x) <- cell_of_int (Int32.to_int v.(x))
        done
    done;
    map

let trace_lambdaman () =  ()
(*
        Printf.printf "('L',[%d,%d,%d])"
        lambdaman.pos.x lambdaman.pos.y
        (int_of_dir lambdaman.dir)
*)

let trace_score () =
     Printf.printf "[%d,%d]," !tickcount lambdaman.score
    (*
    show_map()
    *)

let trace_ghost g = ()
let trace_fright_mode b = ()
let trace_pill_eaten () = trace_score ()
let trace_powerpill_eaten () = trace_score ()
let trace_fruit_eaten () = trace_score ()
let trace_ghost_eaten g = trace_score ()

let ticks_lost = ref []
let trace_life_lost () =
    ticks_lost := !tickcount :: !ticks_lost

let trace_game s = 
    Printf.printf "\n%s,%d,%d,%d\n["
    s
    lambdaman.score
    lambdaman.lives
    !tickcount ;
    List.iter (fun t -> Printf.printf "%d," t)
        (List.rev !ticks_lost);
    Printf.printf "]\n"

let trace_game_won () = trace_game "W"
let trace_game_lost () = trace_game "L"
let trace_error () = trace_game "E"

let refresh_display = ref (fun () -> ())

exception FoundPill
exception GameEnded

let run_sim map ghosts_code lambdaman_code =
    let cyclemax = ref 0 in
    let mapY = Array.length !map in
    let mapX = Array.length !map.(0) in

    let level =
        if (mapX * mapY) mod 100 = 0
        then (mapX * mapY) / 100
        else 1 + (mapX * mapY) / 100
    in

    let fruit_score_array = [| 0; 100; 300; 500; 500; 700; 700; 1000; 1000; 2000;
            2000; 3000; 3000; 5000 |] in
    let fruit_score = if level <= 12 then fruit_score_array.(level) else 5000
    in

    let m = max mapY mapX in

    let lambdaman_source = "" in

    (*
    let lambdaman_code, lambdaman_source = Compiler.compile_file () in

    let fo = open_out "lambdaman.S" in
    let icount = ref 0 in
    for i = 0 to Array.length lambdaman_code - 1 do
        let s = Gcc.pp_instr lambdaman_source lambdaman_code.(i) in
        if s.[0] = ';' then 
            Printf.fprintf fo "%s\n" (String.sub s 1 (String.length s - 1))
        else Printf.fprintf fo "%s\n" s
    done;
    close_out fo;
    *)
    let lambdaman_start = init_lambdaman lambdaman_code in
    let ghosts_start = init_ghosts ghosts_code in
    let gccworld = encode_world () in
    let gcccodes = encode_ghosts_code ghosts_code in

    try
    let state, step_closure = Gccsim.main lambdaman.mac gccworld gcccodes in
    lambdaman.state <- state;
    (*
    Printf.printf "Initial state: "; Gccsim.print_data state; print_newline ();
    *)
    let lam_tick = ref tick_lambda_normal in
    let ghosts_tick = Array.make (Array.length !ghosts) 0 in
    for i = 0 to Array.length !ghosts - 1 do
        ghosts_tick.(i) <- tick_ghosts.(i mod 4)
    done;

    let run_lambdaman () =
        let gccworld = encode_world () in

        try
            let new_state, dir, cycle = 
                Gccsim.step lambdaman.mac lambdaman.state gccworld step_closure in
            lambdaman.state <- new_state;
            lambdaman.dir <- dir;
            cyclemax := max !cyclemax cycle
        with _ -> ()
    in

    let run_ghosts ?verbose:(bverb=false) i =
        let g = !ghosts.(i) in
        let d = Ghcsim.run ~verbose:bverb g !map !ghosts lambdaman.pos in
        g.Ghcsim.dir <- d
    in

    let next {x=x;y=y} dir = 
        match dir with
        | UP -> {x=x;y=y-1}
        | DOWN -> {x=x;y=y+1}
        | LEFT -> {x=x-1;y=y}
        | RIGHT -> {x=x+1;y=y}
    in

    let free pos = !map.(pos.y).(pos.x) <> Wall in
    let pill pos = !map.(pos.y).(pos.x) = Pill || !map.(pos.y).(pos.x) = Powerpill 
        || (!map.(pos.y).(pos.x) = Fruit && !fruit > 0) in

    let free_adjacent x y =
        let free = ref 0 in
        if !map.(y).(x-1) <> Wall
        then incr free;
        if !map.(y).(x+1) <> Wall
        then incr free;
        if !map.(y-1).(x) <> Wall
        then incr free;
        if !map.(y+1).(x) <> Wall
        then incr free;
        !free
    in

    let oppdir d =
        match d with
        UP -> DOWN | DOWN -> UP
        | LEFT -> RIGHT | RIGHT -> LEFT
    in

    let opposite d1 d2 =
        match d1, d2 with
        UP, DOWN -> true
        | DOWN, UP -> true
        | LEFT, RIGHT -> true
        | RIGHT, LEFT -> true
        | _ -> false
    in

    let firstmove = Array.make (Array.length !ghosts) false in

    let fright_mode = ref None in
    let gh_score = ref 200 in

    let update = ref false in

    while true do
        (* Step 1 *)    
        (* Update lambdaman *)
        if !tickcount = !lam_tick
        then begin
            run_lambdaman ();
            if free (next lambdaman.pos lambdaman.dir)
            then lambdaman.pos <- next lambdaman.pos lambdaman.dir;

            update := true;
            trace_lambdaman ();

            lam_tick := !lam_tick + 
                    if pill lambdaman.pos then tick_lambda_eating 
                    else tick_lambda_normal
        end;
        (* Update ghosts *)
        let olddir_vect = Array.map (fun g -> g.Ghcsim.dir) !ghosts in
        for i = 0 to Array.length !ghosts - 1 do
            if ghosts_tick.(i) = !tickcount
            then begin
                try
                    trace_ghost i;
                    run_ghosts ~verbose:false i;
                with e -> ()
            end
        done;

        (* Then move ghosts *)
        for i = 0 to Array.length !ghosts - 1 do
            if ghosts_tick.(i) = !tickcount
            then begin
                let g = !ghosts.(i) in

                let nfree = free_adjacent g.Ghcsim.pos.x g.Ghcsim.pos.y in
                let olddir = olddir_vect.(i) in

                let illegal d = not (free (next g.Ghcsim.pos d))
                    || (nfree > 1 && opposite olddir d
                        && not firstmove.(i)) in

                let legal d = not (illegal d) in

                firstmove.(i) <- false;

                if illegal g.Ghcsim.dir
                then begin
                    (*
                    Printf.printf "Ghost %d dir from %d to %d is illegal\n" i
                    (int_of_dir olddir) (int_of_dir g.Ghcsim.dir);
                    *)

                    (if legal olddir
                    then g.Ghcsim.dir <- olddir
                    else if legal UP
                    then g.Ghcsim.dir <- UP
                    else if legal RIGHT
                    then g.Ghcsim.dir <- RIGHT
                    else if legal DOWN
                    then g.Ghcsim.dir <- DOWN
                    else g.Ghcsim.dir <- LEFT);

                    (*
                    Printf.printf "Set dir to %d\n" (int_of_dir g.Ghcsim.dir);
                    flush stdout;
                    *)
                    (*
                    pause ()
                    *)
                end;

                g.Ghcsim.pos <- next g.Ghcsim.pos g.Ghcsim.dir;

                ghosts_tick.(i) <- ghosts_tick.(i)
                    + if g.Ghcsim.vit <> Ghcsim.Standard
                      then tick_ghosts_fright.(i mod 4)
                      else tick_ghosts.(i mod 4)
            end
        done;

        (* Step 2 *)
        (* tick events *)
        (match !fright_mode with
        | None -> ()
        | Some tick -> if !tickcount = tick
            then begin
                trace_fright_mode false;
                fright_mode := None;
                gh_score := 200;
                lambdaman.vit <- 0;
                for i = 0 to Array.length !ghosts - 1 do
                    let g = !ghosts.(i) in
                    g.Ghcsim.vit <- Ghcsim.Standard
                done
            end);

        if !tickcount = 127 * mapX * mapY * 16
        then begin lambdaman.lives <- 0 end;

        if !tickcount = 127 * 200 || !tickcount = 127 * 400 
        then begin fruit := !tickcount + 80 * 127 end;
        if !tickcount = !fruit
        then begin fruit := 0 end;

        (* Step 3 *)
        let c = !map.(lambdaman.pos.y).(lambdaman.pos.x) in
        if c = Pill
        then begin
            lambdaman.score <- lambdaman.score + 10;
            trace_pill_eaten ();
            !map.(lambdaman.pos.y).(lambdaman.pos.x) <- Empty
        end;

        if c = Powerpill
        then begin
            lambdaman.vit <- !tickcount + 127 * 20;
            lambdaman.score <- lambdaman.score + 50;
            trace_powerpill_eaten ();
            !map.(lambdaman.pos.y).(lambdaman.pos.x) <- Empty;
            fright_mode := Some (!tickcount + 127 * 20);
            trace_fright_mode true;
            gh_score := 200;
            for i = 0 to Array.length !ghosts - 1 do
                let g = !ghosts.(i) in
                g.Ghcsim.dir <- oppdir g.Ghcsim.dir;
                g.Ghcsim.vit <- Ghcsim.Fright
            done
        end;

        if !fruit > 0 && c = Fruit
        then begin
            lambdaman.score <- lambdaman.score + fruit_score;
            fruit := 0
        end;

        (* Step 4 *)
        for i = 0 to Array.length !ghosts - 1 do
            let g = !ghosts.(i) in
            let gpos = g.Ghcsim.pos in
            if gpos = lambdaman.pos && g.Ghcsim.vit <> Ghcsim.Invisible
            then begin
                if !fright_mode = None
                then begin
                    lambdaman.pos <- lambdaman_start;
                    for j = 0 to Array.length !ghosts - 1 do
                        let g = !ghosts.(j) in
                        g.Ghcsim.pos <- ghosts_start.(j);
                        g.Ghcsim.dir <- DOWN
                    done;
                    trace_life_lost ();
                    lambdaman.lives <- lambdaman.lives - 1
                end else begin
                    g.Ghcsim.vit <- Ghcsim.Invisible;
                    g.Ghcsim.pos <- ghosts_start.(i);
                    g.Ghcsim.dir <- DOWN;
                    trace_ghost_eaten g;
                    (* score *)
                    lambdaman.score <- lambdaman.score + !gh_score;
                    if !gh_score < 8 * 200 then gh_score := !gh_score * 2
                end
            end
        done;

        (* Step 5 *)
        let won = 
            let npills = ref 0 in

            for x = 0 to mapX - 1 do
                for y = 0 to mapY - 1 do
                    if !map.(y).(x) = Pill
                    then incr npills
                done
            done;
            !npills = 0 
        in

        if won
        then begin
            lambdaman.score <- (lambdaman.lives + 1) * lambdaman.score;
            trace_game_won ();
            raise GameEnded
        end;

        (* Step 6 *)
        if lambdaman.lives = 0
        then begin 
            trace_game_lost ();
            raise GameEnded
        end;

        incr tickcount;

        if !update then begin
            (*
            let frame_data = Gccsim.get_main_frame_data lambdaman.mac in
            *)
            update := false;
            (*
            display_map (load_map_from_data frame_data 0 mapX mapY)
            (!adap_cell_size * mapX + sep);
            
            display_couple_int_map 
               (load_matrix_from_data Gccsim.data_to_couple frame_data 2 mapX mapY)
                (!adap_cell_size * 2 *mapX + 2 * sep);
            *)
            (*
            display_int_map (load_matrix_from_data frame_data 1 mapX mapY)
            (!adap_cell_size * 3 *mapX + 3 * sep);

            show_map ()
            *)
            !refresh_display ()
        end
    done

    with Gccsim.GccRun(se,p) as e -> begin
            let a, b = p in
            let s = lambdaman_source in
            let n = String.length s in
            let context = 50 in
            let a0 = max 0 (a - context) in
            let b0 = min (n-1) (b + context) in
            let sub a b = String.sub s a (b-a) in
            Printf.eprintf "Exception %s" se;
            trace_error () ;
            (*
            "while evaluating:\n%s***%s***%s\n"
                se (sub a0 a) (sub a b) (sub b b0);
            *)
            (* Gccsim.dump_machine lambdaman.mac; *)
            (* raise e *)
        end
    | GameEnded -> () (* 
    ;
        Printf.printf "Score :%d Lives:%d (tickcount : %d, max cycle : %d)\n" lambdaman.score
        lambdaman.lives
        !tickcount !cyclemax;
        Hashtbl.iter 
            (fun  m i  -> Printf.printf "%s,%d\n" m i)
        Gccsim.profiling
        *)
    | e -> trace_error () ;
         (* raise e *)
        (* Gccsim.dump_machine lambdaman.mac; *)

