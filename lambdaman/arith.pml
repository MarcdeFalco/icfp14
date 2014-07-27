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
