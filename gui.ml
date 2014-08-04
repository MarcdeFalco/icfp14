open Sim
open Graphics
open Common

let pause () =
    let _ = wait_next_event [ Key_pressed ] in
    ()

let adap_cell_size = ref 10

let display_map map sx =
    let cell_size = !adap_cell_size in
    let height = Array.length map in
    for y = 0 to Array.length map - 1 do
        for x = 0 to Array.length map.(0) - 1 do
            let cpos = {x=x;y=y} in

            let color = match map.(y).(x) with
            | Wall -> blue | Empty -> white
            | Pill -> red | Powerpill -> red
            | Fruit -> if !fruit > 0 then green else white
            | LStart ->  white
            | GStart -> white 
            | Ghost -> green
            | GhostWall -> cyan
            | _ -> cyan in

            set_color white;
            fill_rect (sx+cell_size*x) (cell_size * (height - y -1)) cell_size
            cell_size;
            set_color color;

            match map.(y).(x) with
            | Pill -> fill_circle (sx+cell_size*x+cell_size/2) (cell_size * (height - y -1)+cell_size/2) (cell_size/5)
            | Powerpill -> fill_circle (sx+cell_size*x+cell_size/2) (cell_size * (height - y -1)+cell_size/2) (cell_size/3)
            | _ -> fill_rect (sx+cell_size*x) (cell_size * (height - y -1)) cell_size cell_size
        done;
    done

let show_map () = display_map !map 0;
    let cell_size = !adap_cell_size in
    let height = Array.length !map in
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
        if g.Ghcsim.vit = Ghcsim.Fright
        then set_color red
        else if g.Ghcsim.vit = Ghcsim.Invisible
        then set_color white
        else set_color black;
        moveto (cell_size*gpos.x+0) (cell_size *
            (height-gpos.y-1)+0);
        draw_string (string_of_int i)
    done;

    moveto 0 (3+cell_size * height);
    set_color white;
    fill_rect 0 (3+cell_size * height) 1000 20;
    moveto 0 (3+cell_size * height);
    set_color black;
    draw_string (Printf.sprintf
        "tick:%d   score:%d    lives:%d     vit:%d     fruit:%d"
        !tickcount lambdaman.score
        lambdaman.lives lambdaman.vit !fruit)
    (*
    pause ()
    *)


let _ =
    Printexc.record_backtrace true;
    load_map Sys.argv.(2);
    let mapY = Array.length !map in
    let mapX = Array.length !map.(0) in
    let m = max mapY mapX in
    refresh_display := show_map;
    adap_cell_size := 1000 / m;

    let cell_size = !adap_cell_size in
    let szX = cell_size * mapX in
    let szY = cell_size * mapY + 20 in
    let sep = cell_size / 2 in
    open_graph (Printf.sprintf " %dx%d" szX szY);
 
    let lambdaman_code = Gcc.read_gcc_from_file Sys.argv.(3) in
    let ghosts_code = Array.map Ghc.read_ghc_from_file
        (Array.sub Sys.argv 4 (Array.length Sys.argv - 4)) in
    try
        let _ = run_sim map ghosts_code lambdaman_code in
        ()
    with e -> 
        Printexc.print_backtrace stdout;
        raise e
