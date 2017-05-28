all:
	$(FC) cinter.f90 -c -g
	$(FC) blocks.f90 -c -g
	$(FC) main.f90 cinter.o blocks.o -o tetran -lncurses

clean:
	rm cinter.mod cinter.o blocks.mod blocks.o main
