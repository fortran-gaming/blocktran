all:
	$(FC) cinter.f90 -c
	$(FC) blocks.f90 -c
	$(FC) main.f90 cinter.o blocks.o -o main -lncurses

clean:
	rm cinter.mod cinter.o blocks.mod blocks.o main
