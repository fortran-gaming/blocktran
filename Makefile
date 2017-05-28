all:
	$(FC) cinter.f90 -c -g
	$(FC) blocks.f90 -c -g
	$(FC) main.f90 cinter.o blocks.o -o tetran -lncurses

clean:
	$(RM) *.o *.mod tetran
