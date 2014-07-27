let queue_start = 0 in
let queue_end = 0 in
let queue_size = 1000 in
fun queue_push(v)
{
    queue_set(queue_end, v);
    queue_end <- mod(queue_end+1, queue_size)
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
