let up = 0 in
let right = 1 in
let down = 2 in
let left = 3 in

let cell_pill = 2 in
let cell_powerpill = 3 in
let cell_fruit = 4 in

fun nth (l, n) {
    if n == 0 then
        l.hd
    else
        nth (l.tl, n - 1)
}

fun getCell (map, x, y) {
    nth (nth (map, y), x)   
}

fun advance (x, y, dir) {
    if dir == up then
        (x, y - 1)
    else if dir == right then
        (x + 1, y)
    else if dir == down then
        (x, y + 1)
    else
        (x - 1, y)
}

fun getNextCell (map, x, y, dir) {
    let nx, ny = advance (x, y, dir) in
    getCell (map, nx, ny)
}



