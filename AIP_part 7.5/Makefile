FC=ifx
FFLAGS= -warn all -stand f23 -fpp -assume realloc_lhs -qopt-report=3 -diag-disable=10448 -qopenmp -qversion -align array32byte
FOPT=-O3 -g -xHost -ipo

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) $(FOPT)  -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT)  -o bin/app ./obj/environment.o obj/main.o
	rm -rf *.mod
	rm -rf *.yaml

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app
	cat ./bin/output.txt
