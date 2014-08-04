let max_size = 100000

type 'a t = { contents : 'a array; mutable top : int }

let create d =  { contents = Array.make max_size d; top = 0 }

let push v s = s.contents.(s.top) <- v; s.top <- s.top + 1
let pop s = s.top <- s.top - 1; s.contents.(s.top)
let clear s = s.top <- 0
let iter f s = for i = s.top - 1 downto 0 do f s.contents.(i) done

