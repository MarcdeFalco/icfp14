let queue_start = 0 in
let queue_end = 0 in
let queue_size = 1000 in
fun queue_push(v)
{
    let next = mod(queue_end+1, queue_size) in

    if next == queue_start
    then () (* queue overflow *)
    else (
        queue_set(queue_end, v);
        queue_end <- next
    )
}
fun queue_pop(v)
{
    let v = queue_get(queue_start) in
    queue_start <- mod(queue_start+1, queue_size);
    v
}
fun queue_clear()
{
    queue_start <- 0;
    queue_end <- 0
}
fun queue_empty()
{
    queue_start == queue_end
}
