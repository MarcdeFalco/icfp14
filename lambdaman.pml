let up = 0 in
let right = 1 in
let down = 2 in
let left = 3 in

let cell_pill = 2 in
let cell_powerpill = 3 in
let cell_fruit = 4 in

let map, lambdaman, ghost, fruit = world in

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
    fun auxLength(l,a)
    {
        if isempty(l)
        then a
        else auxLength(l.tl,1+a)
    }
    auxLength(l,0)
}

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

fun max3i(a,b,c)
{
    if and(a >= b, a >= c)
    then 0
    else if and(b >= a, b >= c)
    then 1
    else 3
}
fun max4i(a,b,c,d)
{
    if and(a >= b, and(a >= c, a >= d))
    then 0
    else if and(b >= a, and(b >= c, b >= d))
    then 1
    else if and(c >= a, and(c >= b, c >= d))
    then 2
    else 3
}

fun ghostIn(x,y,g)
{
    let vit, pos, gdir = g in
    let x0,y0 = pos in
    and(x==x0,y==y0)
}

fun isGhost(x,y,ghosts)
{
    if isempty(ghosts)
    then 0
    else ghostIn(x,y,ghosts.hd) + isGhost(x,y,ghosts.tl)
}

fun step(s,world) {
    let map, lambdaman, ghosts, fruit = world in
    let vitality, location, dir, lives, score = lambdaman in
    let x, y = location in

    let cache = s in

    fun freeNoGhost(map,x,y,d)
    {
        let nx, ny = advance(x,y,d) in 
        if vitality > 0
        then getCell(map,nx,ny) > 0
        else and(getCell(map,nx,ny) > 0, isGhost(nx,ny,ghosts) == 0)
    }

    fun pillNoGhost(map,x,y,d)
    {
        let nx, ny = advance(x,y,d) in 

        if vitality > 0
        then pill(getCell(map,nx,ny))
        else and(pill(getCell(map,nx,ny)), isGhost(nx,ny,ghosts) == 0)
    }

    if isCrossing(cache,x,y)
    then 
        if pillNoGhost(map,x,y,rot(dir,0)) then (s, rot(dir,0))
        else if pillNoGhost(map,x,y,rot(dir,1)) then (s, rot(dir,1))
        else if pillNoGhost(map,x,y,rot(dir,3)) then (s, rot(dir,3))
        else if pillNoGhost(map,x,y,rot(dir,2)) then (s, rot(dir,2))
        else (s, rot(dir, max4i(
            countCone(map,ghosts,fruit,vitality,x,y,rot(dir,0)),
            countCone(map,ghosts,fruit,vitality,x,y,rot(dir,1)),
            countCone(map,ghosts,fruit,vitality,x,y,rot(dir,2)),
            countCone(map,ghosts,fruit,vitality,x,y,rot(dir,3)))))
    else
        if freeNoGhost(map,x,y,rot(dir,0)) then (s, rot(dir,0))
        else if freeNoGhost(map,x,y,rot(dir,1)) then (s, rot(dir,1))
        else if freeNoGhost(map,x,y,rot(dir,3)) then (s, rot(dir,3))
        else (s, rot(dir,2))
}

fun dim(map)
{
    (length(map.hd), length(map))
}

fun valid(X,Y,x,y)
{
    and(x >= 0, and(x < X, and(y >= 0, y < Y)))
}

fun getCone(x0,y0,x,y)
{
    fun abs(v) {
        if v < 0
        then 0 - v
        else v
    }
    
    if x >= x0
    then if abs(y-y0) <= x-x0
         then right
         else if y >= y0 then up else down
    else if abs(y-y0) <= x0-x
         then left
         else if y >= y0 then up else down
}

fun inSameCone(x0,y0,dir,ghost)
{
    let vit, pos, gdir = ghost in
    let x, y = pos in
    dir == getCone(x0,y0,x,y)
}

fun countGhostsInCone(x,y,dir,ghosts)
{
    if isempty(ghosts)
    then 0
    else inSameCone(x,y,dir,ghosts.hd) + countGhostsInCone(x,y,dir,ghosts.tl)
}

fun countCone(map,ghosts,fruit,powered,x,y,dir)
{
    let X, Y = dim(map) in
    let maxDim = if X > Y then X else Y in

    let od1 = mod(dir+1,4) in
    let od2 = mod(dir+3,4) in

    fun eval(x,y)
    {
        if valid(X,Y,x,y)
        then if getCell(map,x,y) == cell_pill then 1
        else if getCell(map,x,y) == cell_powerpill then 10
        else if and(fruit > 0, getCell(map,x,y) == cell_fruit) then fruit / 2
             else 0
        else 0
    }

    fun aux(x,y,i) {
        fun aux2(x1,y1,x2,y2,j)
        {
            let px1, py1 = advance(x1,y1,od1) in
            let px2, py2 = advance(x2,y2,od2) in

            if j < i
            then (X - i + Y - j) * (eval(px1,py1) + eval(px2,py2)) + aux2(px1,py1,px2,py2,j+1)
            else 0
        }

        let nx, ny = advance(x,y,dir) in

        if and( valid(X,Y,nx,ny), i < 10 )
        then eval(nx,ny) + aux2(nx,ny,nx,ny,0) + aux(nx,ny,i+1)
        else 0
    }

    let nx, ny = advance(x,y,dir) in

    if and(getCell(map,nx,ny)> 0, isGhost(nx,ny,ghosts) <= powered)
    then aux(x,y,1) + (if powered > 0 then powered else 0 - 100) * countGhostsInCone(x,y,dir,ghosts)
    else 0 - 100000
}

fun list_map(f,l) {
    if isempty(l)
    then 0
    else ( f(l.hd), list_map(f,l.tl) )
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


(cacheDir(map), step)
