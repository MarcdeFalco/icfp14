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
    mutable state : Gccsim.data
    }

let lambdaman = { vit = 0; pos = { x=0; y=0 };
    lives = 3; dir = UP; score = 0; mac = Gccsim.dummy_machine (); state =
        Gccsim.Int 42 }

let ghosts : Ghcsim.ghost array ref = ref [||]

let fruit = ref 0

let map = ref [||]

let load_sim lambdaman_code text_map =
    ()

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

let show_map () =
    for y = 0 to Array.length !map - 1 do
        for x = 0 to Array.length !map.(0) - 1 do
            let cpos = {x=x;y=y} in

            let c = ref ' ' in

            if cpos = lambdaman.pos
            then c := '\\';

            for i = 0 to Array.length !ghosts - 1 do
                let g = !ghosts.(i) in
                if cpos = g.Ghcsim.pos
                then c := '='
            done;

            (if !c = ' '
            then c := match !map.(y).(x) with
            | Wall -> '#' | Empty -> ' '
            | Pill -> '.' | Powerpill -> 'o'
            | Fruit -> if !fruit > 0 then '%' else ' ' 
            | LStart ->  ' '
            | GStart -> ' ');

            print_char !c
        done;
        print_newline ()
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
    lambdaman.score <- 0

let init_ghosts codes =
    let nghosts = ref 0 in
    for y = 0 to Array.length !map - 1 do
        for x = 0 to Array.length !map.(0) - 1 do
            if !map.(y).(x) = GStart
            then incr nghosts
        done
    done;
    ghosts := Array.make !nghosts (Ghcsim.init_ghost {x=0;y=0} codes.(0));
    let ncodes = Array.length codes in
    let i = ref 0 in
    for y = 0 to Array.length !map - 1 do
        for x = 0 to Array.length !map.(0) - 1 do
            if !map.(y).(x) = GStart
            then begin
                !ghosts.(!i) <- Ghcsim.init_ghost {x=x;y=y} codes.(!i mod ncodes);
                incr i
            end
        done
    done

let encode_world () =
    let gccmap = ref (Gccsim.Int 0) in
    for y = Array.length !map - 1 downto 0 do
        let line = ref (Gccsim.Int 0) in
        for x = Array.length !map.(0) - 1 downto 0 do
            let c = Common.int_of_cell !map.(y).(x) in
            line := Gccsim.Cons(Gccsim.Int c, !line)
        done;
        gccmap := Gccsim.Cons(!line, !gccmap)
    done;

    let gcclam = Gccsim.Cons(Gccsim.Int lambdaman.vit,
        Gccsim.Cons(Gccsim.Cons(Gccsim.Int lambdaman.pos.x,Gccsim.Int lambdaman.pos.y),
        Gccsim.Cons(Gccsim.Int (Common.int_of_dir lambdaman.dir),
        Gccsim.Cons(Gccsim.Int lambdaman.lives,
        Gccsim.Int lambdaman.score)))) in

    let gccghs = ref (Gccsim.Int 0) in
    for i = Array.length !ghosts - 1 downto 0 do
        let g = !ghosts.(i) in
        let gccgh = Gccsim.Cons(Gccsim.Int (Ghcsim.int_of_vit g.Ghcsim.vit),
        Gccsim.Cons(Gccsim.Cons(Gccsim.Int g.Ghcsim.pos.x,Gccsim.Int g.Ghcsim.pos.y),
        Gccsim.Int (Common.int_of_dir g.Ghcsim.dir))) in
        gccghs := Gccsim.Cons( gccgh, !gccghs )
    done;

    let gccfruit = Gccsim.Int !fruit in

    Gccsim.Cons( !gccmap, Gccsim.Cons( gcclam, Gccsim.Cons( !gccghs, gccfruit)))
    
let _ =
    load_map "map1.txt";
    let lambdaman_code = Compiler.compile_file "lambdaman.pml" in
    (*
    for i = 0 to Array.length lambdaman_code - 1 do
        Printf.printf "%s ; %d\n"
            (Gcc.pp_instr lambdaman_code.(i)) i
    done;
    *)
    let ghosts_code = [| Ghc.alwaysdown |] in
    init_lambdaman lambdaman_code; 
    init_ghosts ghosts_code;
    show_map ();
    let gccworld = encode_world () in
    let gcccodes = Gccsim.Int 42 in

    let state, step_closure = Gccsim.main lambdaman.mac gccworld gcccodes in
    lambdaman.state <- state;

    let lam_tick = ref tick_lambda_normal in
    let ghosts_tick = Array.make (Array.length !ghosts) 0 in
    for i = 0 to Array.length !ghosts - 1 do
        ghosts_tick.(i) <- tick_ghosts.(i mod 4)
    done;

    let run_lambdaman () =
        let gccworld = encode_world () in
        let new_state, dir = 
            Gccsim.step lambdaman.mac lambdaman.state gccworld step_closure in
        lambdaman.state <- new_state;
        lambdaman.dir <- dir
    in

    let run_ghosts i =
        let g = !ghosts.(i) in
        Ghcsim.run g !map !ghosts lambdaman.pos
    in

    let next {x=x;y=y} dir = 
        match dir with
        | UP -> {x=x;y=y-1}
        | DOWN -> {x=x;y=y+1}
        | LEFT -> {x=x-1;y=y}
        | RIGHT -> {x=x+1;y=y}
    in

    let free pos = !map.(pos.y).(pos.x) <> Wall in
    let pill pos = !map.(pos.y).(pos.x) = Pill in

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

    let opposite d1 d2 =
        match d1, d2 with
        UP, DOWN -> true
        | DOWN, UP -> true
        | LEFT, RIGHT -> true
        | RIGHT, LEFT -> true
        | _ -> false
    in

    let firstmove = Array.make (Array.length !ghosts) true in

    for i = 0 to 10000 do
        if !tickcount mod 100 = 0 
        then begin Printf.printf "Tick %d\n" !tickcount;
        show_map () end;

        (* Step 1 *)    
        (* Update lambdaman *)
        if !tickcount = !lam_tick
        then begin
            run_lambdaman ();
            if free (next lambdaman.pos lambdaman.dir)
            then lambdaman.pos <- next lambdaman.pos lambdaman.dir;

            lam_tick := !lam_tick + 
                    if pill lambdaman.pos then tick_lambda_eating 
                    else tick_lambda_normal
        end;
        (* Update ghosts *)
        for i = 0 to Array.length !ghosts - 1 do
            if ghosts_tick.(i) = !tickcount
            then begin
                let g = !ghosts.(i) in

                let nfree = free_adjacent g.Ghcsim.pos.x g.Ghcsim.pos.y in
                let olddir = g.Ghcsim.dir in

                run_ghosts i;

                let illegal = not (free (next g.Ghcsim.pos g.Ghcsim.dir))
                    || (nfree > 1 && opposite olddir g.Ghcsim.dir 
                        && firstmove.(i)) in

                firstmove.(i) <- false;

                if illegal
                then begin
                    if free (next g.Ghcsim.pos olddir)
                    then g.Ghcsim.dir <- olddir
                    else if free (next g.Ghcsim.pos UP)
                    then g.Ghcsim.dir <- UP
                    else if free (next g.Ghcsim.pos RIGHT)
                    then g.Ghcsim.dir <- RIGHT
                    else if free (next g.Ghcsim.pos DOWN)
                    then g.Ghcsim.dir <- DOWN
                    else g.Ghcsim.dir <- LEFT
                end;

                g.Ghcsim.pos <- next g.Ghcsim.pos g.Ghcsim.dir;

                ghosts_tick.(i) <- ghosts_tick.(i)
                    + if g.Ghcsim.vit = Ghcsim.Fright
                      then tick_ghosts_fright.(i mod 4)
                      else tick_ghosts.(i mod 4)
            end
        done;

        incr tickcount
    done
