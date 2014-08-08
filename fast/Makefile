sim: sim.c
	gcc -m32 -march=i386 -std=c99 -g sim.c -o sim

sim.64: sim.c
	gcc -std=c99 -g sim.c -o sim.64

test: sim
	./sim ../maps/world-classic.txt ../taupegoons.gcc ../teams/lastman.ghc

debugtest: sim
	gdb sim ../maps/world-classic.txt ../htl.gcc ../teams/htl.ghc
