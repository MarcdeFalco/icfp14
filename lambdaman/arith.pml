fun and(a,b) { a * b }
fun or(a,b) { a + b - a * b }
fun not(a) { 1 - a }
fun mod(a,b) { a - b * (a / b) }
fun advance(x,y,dir) {
    if dir == up
    then (x,y-1)
    else if dir == left
    then (x-1,y)
    else if dir == down
    then (x,y+1)
    else (x+1,y)
}
fun advance2(x,y,dir)
{
    let x0, y0 = advance(x,y,dir) in
    advance(x0,y0,dir)
}
fun abs(v) {
    if v < 0
    then 0 - v
    else v
}
fun are_opposite(d1,d2)
{
    abs(d1-d2) == 2
}
