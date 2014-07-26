fun length(l)
{
    if isempty(l)
    then 0
    else 1 + length(l.tl)
}

fun step(state, world) {
    (state+1, 2)
}

let map, lambdaman, ghosts, fruit = world in

(length(map), step)
