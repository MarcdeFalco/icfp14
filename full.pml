(* on est dans le corps de main *)
(* world et undoc sont globales *)

var up = 0
var down = 2

fun step(s) {
    fun ajoute(x) {
        x+1
    }

    (ajoute(s), if s - 2 * (s / 2) == 0 then down else up)
}

(42, step)
