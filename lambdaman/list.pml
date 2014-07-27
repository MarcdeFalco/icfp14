fun eq(x,y)
{
    if and(x.isempty, y.isempty)
    then x == y
    else and(x[0] == y[0],x[1] == y[1])
}

fun length(l)
{
    fun auxLength(l,a)
    {
        if l.isempty
        then a
        else auxLength(l.tl,1+a)
    }
    auxLength(l,0)
}

fun rev(l)
{
    fun aux(l,a)
    {
        if l.isempty
        then a
        else aux(l.tl, (l.hd,a))
    }
    aux(l,[])
}

fun list_map(l,f)
{
    fun aux(l,a)
    {
        if l.isempty
        then a
        else aux(l.tl,(f(l.hd),a))
    }
    rev(aux(l,[]))
}

fun iter(l,f)
{
    if l.isempty then ()
    else (f(l.hd); iter(l.tl,f))
}

fun mem(l,x)
{
    if l.isempty
    then 0
    else if eq(x, l.hd) then 1 else mem(l.tl,x)
}

fun prefix(l,n)
{
    if or(l.isempty, n==0)
    then []
    else (l.hd, prefix(l.tl,n-1))
}

fun filter(l,f)
{
    if l.isempty then []
    else (if f(l.hd) 
          then (l.hd, filter(l.tl,f))
          else filter(l.tl,f))
}

fun occ(l,x)
{
    if l.isempty
    then 0
    else (if eq(x, l.hd) then 1 else 0) + occ(l.tl,x)
}

fun nth(l,n)
{
    if n == 0
    then l.hd
    else nth(l.tl,n-1)
}

fun list_zero(n)
{
    fun aux (n,a)
    {
        if n == 0 then a
        else aux(n-1, (0,a))
    }
    aux(n,[])
}

fun replace(l,i,x)
{
    if i == 0
    then (x, l.tl)
    else (l.hd, replace(l.tl,i-1,x))
}
