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
    let npos = advance(x,y,dir) in
    getCell(map, npos[0], npos[1])
}

fun and(a,b) { a * b }

fun or(a,b) { a + b - a * b }

fun isCrossing(map,x,y)
{
    let nfree = 
        (getNextCell(map,x,y,up) > 0)
        + (getNextCell(map,x,y,down) > 0)
        + (getNextCell(map,x,y,left) > 0)
        + (getNextCell(map,x,y,right) > 0)
    in

    nfree > 2
}

fun opp(dir)
{
    if dir == up then down
    else if dir == down then up
    else if dir == left then right
    else left
}

fun rot(dir,n)
{
    let ndir = dir + n in

    if ndir > 3 then ndir-4 else ndir
}

fun pill(c)
{
    (c >= 2) * (c <= 3)
}

fun step(s,world) {
    let map, lambdaman, ghost, fruit = world in
    let vitality, location, dir, lives, score = lambdaman in
    let x, y = location in

    print (lambdaman, x, y, dir) ;

    if isCrossing(map,x,y)
    then 
        if pill(getNextCell(map,x,y,rot(dir,0))) then (s+1, rot(dir,0))
        else if pill(getNextCell(map,x,y,rot(dir,1))) then (s+1, rot(dir,1))
        else if pill(getNextCell(map,x,y,rot(dir,2))) then (s+1, rot(dir,2))
        else if pill(getNextCell(map,x,y,rot(dir,3))) then (s+1, rot(dir,3))
        else if getNextCell(map,x,y,rot(dir,0)) > 0 then (s+1, rot(dir,0))
        else if getNextCell(map,x,y,rot(dir,1)) > 0 then (s+1, rot(dir,1))
        else if getNextCell(map,x,y,rot(dir,2)) > 0 then (s+1, rot(dir,2))
        else (s+1, rot(dir,3))
    else
        if getNextCell(map,x,y,rot(dir,0)) > 0 then (s+1, rot(dir,0))
        else if getNextCell(map,x,y,rot(dir,1)) > 0 then (s+1, rot(dir,1))
        else if getNextCell(map,x,y,rot(dir,3)) > 0 then (s+1, rot(dir,3))
        else (s+1, rot(dir,2))
}

(0, step)
