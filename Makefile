all:
	f95 cinter.f95 -c
	f95 blocks.f95 -c
	f95 main.f95 cinter.o blocks.o -o main -lncurses

clean:
	rm cinter.mod cinter.o blocks.mod blocks.o main
