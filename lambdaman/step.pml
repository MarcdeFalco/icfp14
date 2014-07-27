let depth_max = 8 in

fun setup_depth_max(map)
{
    let h = length(map) in
    let w = length(map.hd) in

    if w < 10
    then depth_max <- 1000 else ();

    if w >= 10
    then depth_max <- 100 else ();

    if w >= 20
    then depth_max <- 100 else ();

    if w >= 25
    then depth_max <- 30 else ();

    if w >= 30
    then depth_max <- 10 else ();

    if w >= 50
    then depth_max <- 8 else ();
    
    ()
}

let closest_pill = 0 in
let closest_powerpill = 0 in
let closest_fruit = 0 in
let closest_ghost = 0 in
let closest_unvisited = 0 in
let least_visit_count = 0 in

let fruit = 0 in

fun init_closest()
{
    closest_pill <- 0;
    closest_powerpill <- 0;
    closest_fruit <- 0;
    closest_ghost <- 0;
    closest_unvisited <- 0;
    least_visit_count <- 3000000
}

fun visit(x,y,d)
{
    let c = map_cell(x,y) in
    let v = map_visit_count(x,y) in

    if and(c == cell_pill, closest_pill.isempty)
    then closest_pill <- (x,y,d) else ();
    if and(c == cell_powerpill, closest_powerpill.isempty)
    then closest_powerpill <- (x,y,d) else ();
    if and(and(fruit, c == cell_fruit), closest_fruit.isempty)
    then closest_fruit <- (x,y,d) else ();
    if and(c == cell_ghost, closest_ghost.isempty)
    then closest_ghost <- (x,y,d) else ();
    if and(and(d > 1, c > 0), v < least_visit_count)
    then ( least_visit_count <- v; closest_unvisited <- (x,y,d)) else ()
}

fun fill_map_dist(pos)
{
    fun next_walk()
    {
        if queue_empty()
        then ()
        else (aux(queue_pop()))
    }

    fun aux(a)
    {
        fun add(pos,d)
        {
            let x, y = pos in
            let str_d = map_dist_cell(x,y) in
            let c = map_cell(x,y) in
            if and(c>0,and(str_d == 0,d<depth_max))
            then queue_push( (pos,d+1) )
            else ()
        }

        let pos, d = a in
        let x, y = pos in

        let posUp = advance(x,y,up) in
        let posDown = advance(x,y,down) in
        let posLeft = advance(x,y,left) in
        let posRight = advance(x,y,right) in

        let str_d = map_dist_cell(x,y) in

        if and(str_d > 0,str_d < d)
        then ()
        else (
            map_dist_cell_set(x,y,d);

            if d > 1 then visit(x,y,d) else ();

            add(posUp,d);
            add(posDown,d);
            add(posLeft,d);
            add(posRight,d)
        );

        next_walk()
    }

    queue_clear();
    queue_push( (pos,1) );
    next_walk()
}

fun free(x,y)
{
    let n = 0 in

    let nxu,nyu = advance(x,y,up) in
    let nxd,nyd = advance(x,y,down) in
    let nxl,nyl = advance(x,y,left) in
    let nxr,nyr = advance(x,y,right) in

    if map_cell(nxu,nyu) > 0 then n <- n + 1 else ();
    if map_cell(nxd,nyd) > 0 then n <- n + 1 else ();
    if map_cell(nxr,nyr) > 0 then n <- n + 1 else ();
    if map_cell(nxl,nyl) > 0 then n <- n + 1 else ();
    n
}

fun load_ghosts(xl,yl,ghosts)
{
    fun load_ghost(g)
    {
        fun wall_ghost(x,y,d,n)
        {
            let nx,ny = advance(x,y,d) in
            let nxu,nyu = advance(x,y,up) in
            let nxd,nyd = advance(x,y,down) in
            let nxl,nyl = advance(x,y,left) in
            let nxr,nyr = advance(x,y,right) in

            if n == 0
            then ()
            else if and(not(and(x==xl,y==yl)),map_cell(x,y) > 0)
             then (
                map_cell_set(x,y,cell_wall);
                if free(x,y) == 1
                then 
                    if map_cell(nx,ny) > 0
                    then wall_ghost(nx, ny, d, n-1)
                    else 
                        if map_cell(nxu,nyu) > 0
                        then wall_ghost(nxu, nyu, up, n-1)
                        else if map_cell(nxd,nyd) > 0
                        then wall_ghost(nxd, nyd, down, n-1)
                        else if map_cell(nxl,nyl) > 0
                        then wall_ghost(nxl, nyl, left, n-1)
                        else if map_cell(nxr,nyr) > 0
                        then wall_ghost(nxr, nyr, right, n-1)
                    else ()
                else ()
             ) else ()
        }

        let vit, pos, gdir = g in
        let x,y = pos in
        let nx,ny = advance(x,y,gdir) in

        if vit == ghost_standard
        then (
            wall_ghost(x, y, gdir, 5)
        ) else if vit == ghost_fright
        then (
            map_cell_set(x,y,cell_ghost);
            if map_cell(nx,ny) > 0 then map_cell_set(nx,ny,cell_ghost) else ()
        ) else ()
    }
    
    if ghosts.isempty
    then ()
    else ( load_ghost(ghosts.hd); load_ghosts(xl,yl,ghosts.tl) )
}

fun closest_target()
{
    fun closer(a,b)
    {
        fun aux(a,b)
        {
            let xa,ya,da = a in
            let xb,yb,db = b in
            da < db
        }

        if a.isempty then 0
        else if b.isempty then 1
        else aux(a,b)
    }
    fun closest(a,b)
    {
        if closer(a,b)
        then a else b
    }

    let target = closest_unvisited in
    if closest_pill.isempty then ()
    else target <- closest_pill;
    if closest_powerpill.isempty then ()
    else target <- closest_powerpill;
    if closest_fruit.isempty then ()
    else target <- closest_fruit;
    if closest_ghost.isempty then ()
    else target <- closest_ghost;
    target
}

fun path_to_target(tgt)
{
    let x, y, d = tgt in
    fun test(dir, x, y, d)
    {
        if d == 1
        then dir
        else path_to_target( (x,y,d) )
    }

    let posUp = advance(x,y,up) in
    let posDown = advance(x,y,down) in
    let posLeft = advance(x,y,left) in
    let posRight = advance(x,y,right) in

    if map_dist_cell(posUp[0],posUp[1]) == d-1
    then test(down,posUp[0],posUp[1],d-1)
    else if map_dist_cell(posLeft[0],posLeft[1]) == d-1
    then test(right,posLeft[0],posLeft[1],d-1)
    else if map_dist_cell(posDown[0],posDown[1]) == d-1
    then test(up,posDown[0],posDown[1],d-1)
    else test(left,posRight[0],posRight[1],d-1)
}

fun reach_target()
{
    let tgt = closest_target() in

    if tgt.isempty
    then down
    else path_to_target(tgt)
}

fun step(s,world)
{
    let map, lambdaman, ghosts, step_fruit = world in
    let vitality, location, dir, lives, score = lambdaman in
    let x,y = location in

    fruit <- step_fruit;

    setup_depth_max(map);
    init_closest();
    map_visit_incr(x,y);
    load_map(map);
    load_ghosts(x,y,ghosts);
    (*
    print_map();
    print 0;
    *)
    init_map_dist(map);
    fill_map_dist(location);
    (*
    print_map_dist();
    print 1;
    *)
    (s, reach_target())
}
