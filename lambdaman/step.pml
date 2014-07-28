let max_wall_depth = 3 in
let safe_ghost = 2 in
let safe_fruit = 1 in

let depth_max = 8 in
fun setup_depth_max(map)
{
    let h = length(map) in
    let w = length(map.hd) in

    if w < 10
    then depth_max <- 1000 else ();

    if w >= 10
    then depth_max <- 80 else ();

    if w >= 20
    then depth_max <- 20 else ();

    if w >= 25
    then depth_max <- 15 else ();

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

fun visit(x,y,vit,d)
{
    let c = map_cell(x,y) in
    let v = map_visit_count(x,y) in

    if and(or(c == cell_powerpill, c == cell_pill), closest_pill.isempty)
    then closest_pill <- (x,y,d) else ();
    (* don't go crazy for power pills 
    if and(c == cell_powerpill, closest_powerpill.isempty)
    then closest_powerpill <- (x,y,d) else ();
    *)
    if and(and(fruit - safe_fruit * 137 >= d[1], c == cell_fruit), closest_fruit.isempty)
    then closest_fruit <- (x,y,d) else ();
    if and(and(vit - safe_ghost * 137 >= d[1], (* can we eat it in time ? *)
        c == cell_ghost), closest_ghost.isempty)
    then closest_ghost <- (x,y,d) else ();
    if and(and(d[0] > 1, c > 0), v < least_visit_count)
    then ( least_visit_count <- v; closest_unvisited <- (x,y,d)) else ()
}

fun eatable(c)
{
    or(c == cell_pill, or(c == cell_powerpill,
        and(fruit > 0, c == cell_fruit)))
}

fun fill_map_dist(pos,vit)
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
            let t = if eatable(c) then 137 else 127 in
            if and(c>0,and(or(str_d[0] == 0,str_d[1]>d[1]+t),d[0]<depth_max))
            then queue_push( (pos,(d[0]+1,d[1]+t)) )
            else ()
        }

        let pos, d = a in
        let x, y = pos in

        let posUp = advance(x,y,up) in
        let posDown = advance(x,y,down) in
        let posLeft = advance(x,y,left) in
        let posRight = advance(x,y,right) in

        let str_d = map_dist_cell(x,y) in
        let c = map_cell(x,y) in
        let t = if eatable(c) then 137 else 127 in

        if and(str_d[0] > 0,str_d[1] <= d[1]+t)
        then ()
        else (
            map_dist_cell_set(x,y,d);

            if d[0] > 1 then visit(x,y,vit,d) else ();

            add(posUp,d);
            add(posDown,d);
            add(posLeft,d);
            add(posRight,d)
        );

        next_walk()
    }

    queue_clear();
    queue_push( (pos,(1,0)) );
    next_walk()
}

fun sfree(x,y)
{
    let n = 0 in

    let nxu,nyu = advance(x,y,up) in
    let nxd,nyd = advance(x,y,down) in
    let nxl,nyl = advance(x,y,left) in
    let nxr,nyr = advance(x,y,right) in

    if map_cell(nxu,nyu) == 0 then () else n <- n + 1;
    if map_cell(nxd,nyd) == 0 then () else n <- n + 1;
    if map_cell(nxr,nyr) == 0 then () else n <- n + 1;
    if map_cell(nxl,nyl) == 0 then () else n <- n + 1;
    n
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

fun iter_dir(f,x,y)
{
    fun main_aux(d)
    {
        if d >= 4
        then ()
        else (
            aux(d);
            main_aux(d+1)
        )
    }

    fun aux(d)
    {
        let nx, ny = advance(x,y,d) in
        f(nx,ny,d)
    }

    main_aux(0)
}

fun corridor(x,y)
{
    if map_cell(x,y) > 0
    then sfree(x,y) == 2
    else 0
}

fun load_ghosts(xl,yl,ghosts)
{
    fun load_ghost(g)
    {
        fun fix_ghost_dir(x,y,d)
        {
            let fixed_dir = d in

            fun test_dir(nx,ny,nd)
            {
                if map_cell(nx,ny) > 0
                then fixed_dir <- nd
                else ()
            }

            let nx, ny = advance(x,y,d) in

            if map_cell(nx,ny) > 0
            then ()
            else iter_dir(test_dir,x,y);

            fixed_dir
        }

        fun wall_ghost(x,y,d)
        {
            let bounced = 0 in

            fun walk_depth(x,y,d,depth)
            {
                let n = sfree(x,y) in
                let inlambdaman = and(x==xl,y==yl) in
                
                fun test_dir(nx,ny,nd)
                {
                    if are_opposite(d,nd)
                    then () else (
                        if map_cell(nx,ny) > 0
                        then walk_depth(nx, ny, nd, depth+1)
                        else ()
                    )
                }

                if and(not(inlambdaman),depth < max_wall_depth)
                then (
                    if map_cell(x,y) > 0
                    then (
                        map_cell_set(x,y,cell_ghost_wall);
                        iter_dir(test_dir,x,y)
                    ) else ()
                )
                else ()
            }

            fun walk_corridor(x,y,d)
            {
                let n = sfree(x,y) in
                let nfl = free(x,y) in
                let inlambdaman = and(x==xl,y==yl) in
                
                fun test_dir(nx,ny,nd)
                {
                    if are_opposite(d,nd)
                    then () else (
                        if map_cell(nx,ny) > 0
                        then walk_corridor(nx, ny, nd)
                        else ()
                    )
                }

                if and(not(inlambdaman),n <= 2)
                then (
                    map_cell_set(x,y,cell_ghost_wall);
                    iter_dir(test_dir,x,y)
                )
                else ();
                
                if and(not(inlambdaman),n <= 1)
                 then bounced <- 1
                 else ()
            }

            (* wall of any corridor, taking into account rebound *)
            let nx,ny = advance(x,y,d) in

            if not(corridor(nx,ny))
            then (
                if map_cell(nx,ny) > 0
                then walk_depth(nx, ny, d, 0)
                else ()
            )
            else walk_corridor(nx,ny,d);
            if and(bounced,corridor(x,y))
            then walk_corridor(x,y,mod(d+2,4))
            else (
                map_cell_set(x,y,cell_ghost_wall)
            )
        }

        let vit, pos, gdir = g in
        let x,y = pos in
        let nx,ny = advance(x,y,gdir) in

        gdir <- fix_ghost_dir(x,y,gdir);

        if vit == ghost_standard
        then (
            wall_ghost(x, y, gdir)
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
            da[1] < db[1]
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
    if closest_ghost.isempty then ()
    else target <- closest_ghost;
    if closest_fruit.isempty then ()
    else target <- closest_fruit;
    target
}

fun path_to_target(tgt)
{
    let x, y, d = tgt in

    fun test(mindir, d)
    {
        let nx, ny = advance(x,y,mindir) in
        if d[0] == 1
        then mod(mindir + 2, 4)
        else path_to_target( (nx,ny,d) )
    }

    fun test_dir(x,y,dir)
    {
        let dist = map_dist_cell(x,y) in
        if and(dist[0] > 0, dist[1] < mindist[1])
        then (
            mindist <- dist;
            mindir <- dir
        )
        else ()
    }

    let mindir = up in
    let mindist = (0,10000000) in

    iter_dir(test_dir, x, y);
    test(mindir,mindist)
}

fun reach_target(olddir)
{
    let tgt = closest_target() in

    if tgt.isempty
    then olddir
    else path_to_target(tgt)
}

fun available_dir(x,y)
{
    let l = 0 in

    fun aux(d)
    {
        let nx, ny = advance(x,y,d) in
        if map_cell(nx,ny) > 0
        then l <- (d, l)
        else ()
    }

    aux(up); aux(down); aux(left); aux(right);
    l
}

fun step(s,world)
{
    let map, lambdaman, ghosts, step_fruit = world in
    let vitality, location, dir, lives, score = lambdaman in
    let x,y = location in
    let avail = available_dir(x,y) in

    fruit <- step_fruit;

    setup_depth_max(map);
    init_closest();
    map_visit_incr(x,y);
    load_map(map);
    load_ghosts(x,y,ghosts);
    init_map_dist(map);
    fill_map_dist(location,vitality);
    ( mod(s+1,15), reach_target(dir))
}
