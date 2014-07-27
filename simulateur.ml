open Common
open Graphics

let pause () =
    let _ = wait_next_event [ Key_pressed ] in
    ()

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

let adap_cell_size = ref 10

let show_map () =
    let cell_size = !adap_cell_size in
    let height = Array.length !map in
    for y = 0 to Array.length !map - 1 do
        for x = 0 to Array.length !map.(0) - 1 do
            let cpos = {x=x;y=y} in

            let color = match !map.(y).(x) with
            | Wall -> blue | Empty -> white
            | Pill -> red | Powerpill -> red
            | Fruit -> if !fruit > 0 then green else white
            | LStart ->  white
            | GStart -> white in

            set_color white;
            fill_rect (cell_size*x) (cell_size * (height - y -1)) cell_size
            cell_size;
            set_color color;

            match !map.(y).(x) with
            | Pill -> fill_circle (cell_size*x+cell_size/2) (cell_size * (height - y -1)+cell_size/2) (cell_size/5)
            | Powerpill -> fill_circle (cell_size*x+cell_size/2) (cell_size * (height - y -1)+cell_size/2) (cell_size/3)
            | _ -> fill_rect (cell_size*x) (cell_size * (height - y -1)) cell_size cell_size
        done;
    done;

    set_color black;
    let lx = (cell_size*lambdaman.pos.x) in
    let ly = cell_size*(height-1-lambdaman.pos.y) in
    set_line_width (cell_size/5);
    moveto lx (ly+cell_size);
    lineto (lx+cell_size) ly;
    moveto lx ly;
    lineto (lx+cell_size/2) (ly+cell_size/2);
    (*
    fill_rect (cell_size*lambdaman.pos.x+cell_size/4)
    (cell_size*(height-lambdaman.pos.y)+cell_size/4) (cell_size/2)
    (cell_size/2);
    *)
    for i = 0 to Array.length !ghosts - 1 do
        let g = !ghosts.(i) in
        let gpos = g.Ghcsim.pos in
        if g.Ghcsim.vit = Ghcsim.Fright
        then set_color green
        else if g.Ghcsim.vit = Ghcsim.Standard
        then set_color cyan
        else set_color yellow;
        fill_circle (cell_size*gpos.x+cell_size/2) (cell_size *
        (height-gpos.y-1)+cell_size/2) (cell_size/2);
        (*
        if g.Ghcsim.vit = Ghcsim.Fright
        then set_color red
        else if g.Ghcsim.vit = Ghcsim.Invisible
        then set_color white
        else set_color black;
        moveto (20*gpos.x+4) (600 - 20*gpos.y+4);
        draw_string (string_of_int i)
        *)
    done;

    moveto 0 (3+cell_size * height);
    set_color white;
    fill_rect 0 (3+cell_size * height) 1000 20;
    moveto 0 (3+cell_size * height);
    set_color black;
    draw_string (Printf.sprintf
        "tick:%d   score:%d"
        !tickcount lambdaman.score)


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

    let gcclam = Gccsim.Cons(Gccsim.Int (if lambdaman.vit > 0 then lambdaman.vit - !tickcount else 0),
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

    let gccfruit = Gccsim.Int (if !fruit > 0 then !fruit - !tickcount else 0) in

    Gccsim.Cons( !gccmap, Gccsim.Cons( gcclam, Gccsim.Cons( !gccghs, gccfruit)))
    
exception FoundPill

let _ =
    load_map Sys.argv.(1);
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
    adap_cell_size := 1000 / m;

    let cell_size = !adap_cell_size in
    let szX = cell_size * mapX in
    let szY = cell_size * mapY + 20 in
    open_graph (Printf.sprintf " %dx%d" szX szY);
    let lambdaman_code, lambdaman_source = Compiler.compile_file "lambdaman.pml" in

    let fo = open_out "lambdaman.S" in
    let icount = ref 0 in
    for i = 0 to Array.length lambdaman_code - 1 do
        let s = Gcc.pp_instr lambdaman_source lambdaman_code.(i) in
        if s.[0] = ';' then 
            Printf.fprintf fo "%s\n" (String.sub s 1 (String.length s - 1))
        else Printf.fprintf fo "%s\n" s
    done;
    close_out fo;
    let ghosts_code = [| Ghc.random |] in
    let lambdaman_start = init_lambdaman lambdaman_code in
    let ghosts_start = init_ghosts ghosts_code in
    show_map ();
    pause ();
    let gccworld = encode_world () in
    let gcccodes = Gccsim.Int 42 in

    try
    let state, step_closure = Gccsim.main lambdaman.mac gccworld gcccodes in
    lambdaman.state <- state;
    Printf.printf "Initial state: "; Gccsim.print_data state; print_newline ();
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
        let d = Ghcsim.run g !map !ghosts lambdaman.pos in
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

    let update = ref false in

    let ghosts_eaten = ref 0 in

    while true do
        (* Step 1 *)    
        (* Update lambdaman *)
        if !tickcount = !lam_tick
        then begin
            run_lambdaman ();
            if free (next lambdaman.pos lambdaman.dir)
            then lambdaman.pos <- next lambdaman.pos lambdaman.dir;

            update := true;

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

                (*show_map ();  *)

                ghosts_tick.(i) <- ghosts_tick.(i)
                    + if g.Ghcsim.vit = Ghcsim.Fright
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
                fright_mode := None;
                ghosts_eaten := 0;
                lambdaman.vit <- 0;
                for i = 0 to Array.length !ghosts - 1 do
                    let g = !ghosts.(i) in
                    g.Ghcsim.vit <- Ghcsim.Standard
                done
            end);

        if !tickcount = 127 * mapX * mapY * 16
        then begin lambdaman.lives <- 0 end;

        if !tickcount = 127 * 200 || !tickcount = 127 * 200 
        then begin fruit := !tickcount + 80 * 127 end;
        if !tickcount = !fruit
        then begin fruit := 0 end;

        (* Step 3 *)
        let c = !map.(lambdaman.pos.y).(lambdaman.pos.x) in
        if c = Pill
        then begin
            lambdaman.score <- lambdaman.score + 10;
            !map.(lambdaman.pos.y).(lambdaman.pos.x) <- Empty
        end;

        if c = Powerpill
        then begin
            lambdaman.vit <- !tickcount + 127 * 20;
            lambdaman.score <- lambdaman.score + 50;
            !map.(lambdaman.pos.y).(lambdaman.pos.x) <- Empty;
            fright_mode := Some (!tickcount + 127 * 20);
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
                        g.Ghcsim.pos <- ghosts_start.(j)
                    done;
                    lambdaman.lives <- lambdaman.lives - 1
                end else begin
                    g.Ghcsim.vit <- Ghcsim.Invisible;
                    g.Ghcsim.pos <- ghosts_start.(i);
                    let ghost_score = match !ghosts_eaten with
                        | 0 -> 200 | 1 -> 400 | 2 -> 800
                        | _ -> 1600  in
                    lambdaman.score <- ghost_score + lambdaman.score;
                    incr ghosts_eaten
                end
            end
        done;

        (* Step 5 *)
        let won = try
            for x = 0 to mapX - 1 do
                for y = 0 to mapY - 1 do
                    if !map.(y).(x) = Pill
                    then raise FoundPill
                done
            done;
            true
        with FoundPill -> false in

        if won
        then begin
            lambdaman.score <- (lambdaman.lives + 1) * lambdaman.score;
            failwith "Game won"
        end;

        (* Step 6 *)
        if lambdaman.lives = 0
        then begin failwith "Game lost" end;

        incr tickcount;

        if !update then begin
            update := false;
            show_map (); pause()
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
            Printf.printf "Exception %s while evaluating:\n%s***%s***%s\n"
                se (sub a0 a) (sub a b) (sub b b0);
            Gccsim.dump_machine lambdaman.mac;
            raise e
        end
