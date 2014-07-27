fun load_map(l)
{
    fun aux(l,i)
    {
        if l.isempty
        then ()
        else (
            map_set(i,l.hd);
            aux(l.tl,i+1)
        )
    }
    aux(l,0)
}

fun map_cell(x,y)
{
    nth(map_get(y),x)
}

fun init_map_dist(l)
{
    fun aux(l,i)
    {
        if l.isempty
        then ()
        else (
            map_dist_set(i, list_zero(length(l.hd)));
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
