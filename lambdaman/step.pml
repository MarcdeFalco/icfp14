let depth_max = 50 in

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

fun step(s,world) {
    let map, lambdaman, ghosts, fruit = world in
    let vitality, location, dir, lives, score = lambdaman in

    load_map(map);
    init_map_dist(map);
    fill_map_dist(location);
    (s, down)
}
