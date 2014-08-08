sim: sim.c
	gcc -m32 -march=i386 -std=c99 -O3 sim.c -o sim

test: sim
	./sim ../maps/world-classic.txt ../taupegoons.gcc ../teams/lastman.ghc

debugtest: sim
	gdb sim ../maps/world-classic.txt ../htl.gcc ../teams/htl.ghc
