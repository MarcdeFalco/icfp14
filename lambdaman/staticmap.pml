fun split_by(l,s)
{
    fun aux(l,acc,n)
    {
        if l.isempty
        then if n > 1
             then [rev(acc)] else []
        else if n == s
             then (rev( (l.hd,acc) ), aux(l.tl,[],1))
             else aux(l.tl, (l.hd,acc), n+1)
    }

    aux(l,[],1)
}

fun load_map(l)
{
    fun aux(l,i)
    {
        if l.isempty
        then ()
        else (
            map_set(i, l.hd);
            aux(l.tl,i+1)
        )
    }
    aux(l,0)
}

fun map_cell(x,y)
{
    nth(map_get(y),x)
}

fun map_cell_set(x,y,v)
{
    map_set(y, replace(map_get(y),x,v))
}

fun map_visit_count(x,y)
{
    nth(map_visit_get(y),x)
}

fun map_visit_cell_set(x,y,v)
{
    map_visit_set(y, replace(map_visit_get(y),x,v))
}

fun map_visit_incr(x,y)
{
    map_visit_cell_set(x,y,map_visit_count(x,y)+1)
}

fun init_map_visit(l)
{
    fun aux(l,i)
    {
        if l.isempty
        then ()
        else (
            map_visit_set(i, list_zero(length(l.hd)));
            aux(l.tl,i+1)
        )
    }
    aux(l,0)
}

fun init_map_dist(l)
{
    fun aux(l,i)
    {
        if l.isempty
        then ()
        else (
            map_dist_set(i, list_make(length(l.hd),(0,0)));
            aux(l.tl,i+1)
        )
    }
    aux(l,0)
}

fun map_dist_cell(x,y)
{
    nth(map_dist_get(y),x)
}

fun map_dist_cell_set(x,y,v)
{
    map_dist_set(y, replace(map_dist_get(y),x,v))
}

fun print_map()
{
    let i = 0 in

    fun next_row()
    {
        let r = map_get(i) in
        if r.isempty
        then ()
        else (
            print r;
            i <- i + 1;
            next_row ()
        )
    }

    next_row()
}

fun print_map_dist()
{
    let i = 0 in

    fun next_row()
    {
        let r = map_dist_get(i) in
        if r.isempty
        then ()
        else (
            print r;
            i <- i + 1;
            next_row ()
        )
    }

    next_row()
}
