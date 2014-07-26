let up = 0 in
let right = 1 in
let down = 2 in
let left = 3 in

fun nth(l,n) {
    if n == 0
    then l.hd
    else nth(l.tl,n-1)
}

fun getCell(map,x,y) {
    nth(nth(map,y),x)
}

fun advance(x,y,dir) {
    if dir == up
    then (x,y-1)
    else if dir == left
    then (x-1,y)
    else if dir == down
    then (x,y+1)
    else (x+1,y)
}

fun getNextCell(map,x,y,dir)
{
    let nx, ny = advance(x,y,dir) in
    getCell(map, nx, ny)
}

fun and(a,b) { a * b }
fun or(a,b) { a + b - a * b }
fun mod(a,b) { a - b * (a / b) }

fun length(l)
{
    fun aux(l,a)
    {
        if isempty(l)
        then a
        else aux(l.tl,1+a)
    }
    aux(l,0)
}

(*
fun length(l)
{
    if isempty(l)
    then 0
    else 1 + length(l.tl)
}
*)

fun isCrossing(cache,x,y)
{
    length(getCell(cache,x,y)) > 2
}

fun opp(dir)
{
    mod(dir+2, 4)
}

fun rot(dir,n)
{
    mod(dir+n, 4)
}

fun pill(c)
{
    and(c >= 2, c <= 3)
}

fun step(s,world) {
    let map, lambdaman, ghost, fruit = world in
    let vitality, location, dir, lives, score = lambdaman in
    let x, y = location in

    let cache = s in

    if isCrossing(cache,x,y)
    then 
        if pill(getNextCell(map,x,y,rot(dir,0))) then (s, rot(dir,0))
        else if pill(getNextCell(map,x,y,rot(dir,1))) then (s, rot(dir,1))
        else if pill(getNextCell(map,x,y,rot(dir,2))) then (s, rot(dir,2))
        else if pill(getNextCell(map,x,y,rot(dir,3))) then (s, rot(dir,3))
        else if getNextCell(map,x,y,rot(dir,0)) > 0 then (s, rot(dir,0))
        else if getNextCell(map,x,y,rot(dir,1)) > 0 then (s, rot(dir,1))
        else if getNextCell(map,x,y,rot(dir,2)) > 0 then (s, rot(dir,2))
        else (s, rot(dir,3))
    else
        if getNextCell(map,x,y,rot(dir,0)) > 0 then (s, rot(dir,0))
        else if getNextCell(map,x,y,rot(dir,1)) > 0 then (s, rot(dir,1))
        else if getNextCell(map,x,y,rot(dir,3)) > 0 then (s, rot(dir,3))
        else (s, rot(dir,2))
}

fun listDir(map,x,y)
{
    fun a() {
        if getNextCell(map,x,y,up) > 0
        then (up, b())
        else b()
    }
    fun b() {
        if getNextCell(map,x,y,right) > 0
        then (right, c())
        else c()
    }
    fun c() {
        if getNextCell(map,x,y,down) > 0
        then (down, d())
        else d()
    }
    fun d() {
        if getNextCell(map,x,y,left) > 0
        then (left, 0)
        else left
    }

    if getCell(map,x,y) > 0 
    then a()
    else 0
}

fun cacheDir(map) {
    fun auxY(l, y) {
        if isempty(l)
        then 0
        else (auxX(l.hd,0,y), auxY(l.tl,y+1))
    }
    fun auxX(l, x, y) {
        if isempty(l)
        then 0
        else (listDir(map,x,y), auxX(l.tl, x+1, y))
    }
    auxY(map, 0)
}

let map, lambdaman, ghost, fruit = world in
(cacheDir(map), step)
