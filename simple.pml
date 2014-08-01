let w = 8+2 in

fun step(state, world) {
    let d = state - w * (state / w) in
    let n = (state / w) in
    let d2 = n - 2 * (n / 2) in

    if d <= 7
    then (state+1, if d2 then 0 else 2)
    else (state+1, 1)
}

(0, step)
