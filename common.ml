type pos = { x : int; y : int }

type direction = UP | DOWN | LEFT | RIGHT

let dir_of_int n = match n with
    0 -> UP | 2 -> DOWN | 1 -> RIGHT | _ -> LEFT

let int_of_dir n = match n with
    UP -> 0 | DOWN -> 2 | RIGHT -> 1 | LEFT -> 3

type cell = Wall | Empty | Pill | Powerpill | LStart | GStart | Fruit | Ghost

let cell_of_int c =
    match c with
    0 -> Wall | 1 -> Empty | 2 -> Pill | 3 -> Powerpill 
    | 4 -> Fruit | 5 -> LStart | 6 -> GStart | _ -> Ghost

let int_of_cell c =
    match c with
    Wall -> 0 | Empty -> 1 | Pill -> 2 | Powerpill -> 3
    | Fruit -> 4 | LStart -> 5 | GStart -> 6
    | Ghost -> 7

exception Found of pos
let map_get_starting_ghost_pos m i =
try
    let current = ref 0 in
    for y = 0 to Array.length m - 1 do
    for x = 0 to Array.length m.(0) - 1 do
        if m.(y).(x) = GStart
        then begin
            (if !current = i
            then raise (Found {x=x;y=y}));
            incr current
        end
    done
    done;
    raise Not_found
with Found p -> p
