let up = 0 in
let right = 1 in
let down = 2 in
let left = 3 in

let cell_pill = 2 in
let cell_powerpill = 3 in
let cell_fruit = 4 in

(* Direction grading system *)
let unit = 1 in
let repetition = unit in
let direct_pill = unit * 10 in
let direct_ghost = unit * 100 in
let direct_powerpill = unit * 50 in
let direct_fruit = unit * 70 in
let penaly_opposite = 5 * unit in
let least_tol = unit * 2 in
let distant_pill = unit * 1 in
let distant_ghost = unit * 10 in
let distant_powerpill = unit * 5 in
let distant_fruit = unit * 10 in

let map, lambdaman, ghost, fruit = world in

fun eq(x,y)
{
    if isempty(x) * isempty(y)
    then x == y
    else (x[0] == y[0]) * (x[1] == y[1])
}

fun notmem(l,x)
{
    if isempty(l)
    then 1
    else if eq(x, l.hd) then 0 else notmem(l.tl,x)
}

fun mem(l,x)
{
    if isempty(l)
    then 0
    else if eq(x, l.hd) then 1 else mem(l.tl,x)
}

fun prefix(l,n)
{
    if or(isempty(l), n==0)
    then []
    else (l.hd, prefix(l.tl,n-1))
}

fun filter(f,l)
{
    if isempty(l) then []
    else (if f(l.hd) then (l.hd, filter(f,l.tl)) else filter(f,l.tl))
}

fun occ(l,x)
{
    if isempty(l)
    then 0
    else (if eq(x, l.hd) then 1 else 0) + occ(l.tl,x)
}

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

fun iter(l,f)
{
    if isempty(l) then ()
    else (f(l.hd); iter(l.tl,f))
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

fun max(a,b)
{
    if a > b then a else b
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
    let nx0,ny0 = advance(x0,y0,gdir) in
    or(and(x==x0,y==y0), and(x==nx0,y==ny0))
}

fun isGhost(x,y,ghosts)
{
    if isempty(ghosts)
    then 0
    else ghostIn(x,y,ghosts.hd) + isGhost(x,y,ghosts.tl)
}

fun are_opposite(d1,d2)
{
    abs(d1-d2) == 2
}

fun step(s,world) {
    let map, lambdaman, ghosts, fruit = world in
    let mapX, mapY = dim(map) in
    let vitality, location, dir, lives, score = lambdaman in
    let x, y = location in

    let cache, last = s in

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
        
    fun getAvailableDir()
    {
        let dirList = listDir(map,x,y) (*getCell(cache,x,y)*) in
        let orderedList = 0 in

        fun addIfInDirList(d)
        {
            if mem(dirList,d)
            then orderedList <- (d, orderedList)
            else ()
        }

        iter( [ rot(dir,2); rot(dir,3); rot(dir,1); rot(dir,0) ], addIfInDirList );
        orderedList
    }

    fun nextCell(d) {
        let nx, ny = advance(x,y,d) in
        getCell(map,nx,ny)
    }

    fun gradeDir(d)
    {
        let grade = 0 in
        let nx, ny = advance(x,y,d) in 

        let pills, powerpills, fruit, nghosts = walk(world,6,(nx,ny)) in

        let c = nextCell(d) in

        grade <- grade
            + distant_pill * pills
            + distant_powerpill * powerpills
            + distant_fruit * fruit
            + (if vitality > 0 then distant_ghost else 0 - distant_ghost)*nghosts;

        if c == cell_pill
        then grade <- grade + direct_pill else ();

        if c == cell_powerpill
        then grade <- grade + direct_powerpill else ();

        if c == cell_fruit
        then grade <- grade + direct_fruit else ();

        if are_opposite(d,dir)
        then grade <- grade - penaly_opposite
        else ();

        grade <- grade - repetition * occ(last, (nx,ny));

        grade <- grade + (if vitality > 0 then direct_ghost else 0 - direct_ghost)* isGhost(nx,ny,ghosts);

        (d,grade)
    }

    fun target(l)
    {
        let bestdist = 256 * 256 in
        let closest = (0,0) in

        let bestdir = l.hd in
        let bestdirdist = 256 * 256 in

        fun evalCell(map,x0,y0)
        {
            let x, y = location in
            let c = getCell(map,x0,y0) in
            let dist = max(abs(x-x0),abs(y-y0)) in

            if and(or(pill(c),or(and(vitality > 0,isGhost(x,y,ghosts)),and(fruit>0,c==cell_fruit))), dist < bestdist)
            then (
                bestdist <- dist; closest <- (x0,y0)
            )
            else ()
        }

        fun auxY0(l, y0) {
            if isempty(l)
            then ()
            else (auxX0(l.hd,0,y0); auxY0(l.tl,y0+1))
        }

        fun auxX0(l, x0, y0) {
            if isempty(l)
            then ()
            else (evalCell(map,x0,y0); auxX0(l.tl, x0+1, y0))
        }

        fun evalDir0(dg)
        {
            let d, g = dg in
            let nx, ny = advance(x,y,d) in
            let x0, y0 = closest in
            let dist = max(abs(nx-x0),(ny-y0)) in
            if dist < bestdirdist
            then ( bestdirdist <- dist; bestdir <- d )
            else ()
        }

        auxY0(map, 0);
        iter(l, evalDir0);
        bestdir
    }

    fun max_grade(l)
    {
        let bestdir = 0 in
        let bestgrade = 0 - unit * 1000 in

        fun evalDir(e)
        {
            let d, g = e in
            if g > bestgrade
            then (bestdir <- d; bestgrade <- g)
            else ()
        }

        fun leastGraded(v)
        {
            v[1] >= bestgrade - least_tol
        }

        iter(l, evalDir);
        (*
        if and(bestgrade <= 0, max(mapX,mapY) < 50) (* targetting *)
        then (
            target(filter(leastGraded, l))
            )
        else
        *)
        bestdir
    }

    fun computeReturn(avail)
    {
        let d = max_grade(list_map(gradeDir, avail)) in

        ( (cache, (advance(x,y,d), prefix(last,1000))), d)
    }

    computeReturn(getAvailableDir())
}

fun dim(map)
{
    (length(map.hd), length(map))
}

fun valid(X,Y,x,y)
{
    and(x >= 0, and(x < X, and(y >= 0, y < Y)))
}

fun abs(v) {
    if v < 0
    then 0 - v
    else v
}

fun getCone(x0,y0,x,y)
{
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

        if and( valid(X,Y,nx,ny), 1 (*i < 20 *))
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

fun listDir(map,x0,y0)
{
    let l = [] in

    if getCell(map,x0,y0) > 0
    then (
        if getNextCell(map,x0,y0,up) > 0
        then l <- (up, l) else ();
        if getNextCell(map,x0,y0,right) > 0
        then l <- (right, l) else ();
        if getNextCell(map,x0,y0,down) > 0
        then l <- (down, l) else ();
        if getNextCell(map,x0,y0,left) > 0
        then l <- (left, l) else ()
    ) else ();
    l
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

let depth_max = 10 in

fun walk(world,depth_max,pos)
{
    let map, lambda, ghosts, fruit = world in
    let pills = 0 in
    let powerpills = 0 in
    let fruit = 0 in
    let nghosts = 0 in

    let tovisit = ( (pos,0), 0 ) in
    let visited = [] in

    fun next_walk()
    {
        if isempty(tovisit)
        then ()
        else (do_walk((tovisit.hd)[0], (tovisit.hd)[1]))
    }

    fun do_walk(pos, depth)
    {
        let x, y = pos in
        let posUp = advance(x,y,up) in
        let posDown = advance(x,y,down) in
        let posLeft = advance(x,y,left) in
        let posRight = advance(x,y,right) in
        let c = getCell(map,x,y) in
        let nghostsincell = isGhost(x,y,ghosts) in
    
        tovisit <- tovisit.tl;
        visited <- (pos, visited);

        nghosts <- nghosts + nghostsincell;
        if c == cell_pill
        then pills <- pills + 1 else ();
        if c == cell_powerpill
        then powerpills <- powerpills + 1 else ();
        if and(fruit > 0, c==cell_fruit)
        then fruit <- fruit + 1 else ();

        if and(c > 0, depth < depth_max)
        then (
            (if notmem(visited,posUp)
            then tovisit <- ( (posUp, depth+1), tovisit )
            else ());
            (if notmem(visited,posDown)
            then tovisit <- ( (posDown, depth+1), tovisit )
            else ());
            (if notmem(visited,posLeft)
            then tovisit <- ( (posLeft, depth+1), tovisit )
            else ());
            (if notmem(visited,posRight)
            then tovisit <- ( (posRight, depth+1), tovisit )
            else ())
        ) else ();

        next_walk()
    }

    next_walk();
    (pills, powerpills, fruit, nghosts)
}


((0(*cacheDir(map)*),0), step)
