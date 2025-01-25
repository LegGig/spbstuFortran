FC=ifx
FFLAGS= -warn all -stand f23 -fpp -assume realloc_lhs -qopt-report=3 -diag-disable=10448 -qopenmp -qopenmp-simd -qversion -align array32byte
FOPT=-O3 -g -xHost -ipo

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -module obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/io_func.f90 -module obj/ -o obj/io_func.o
	$(FC) $(FFLAGS) $(FOPT) -c src/calculate.f90 -module obj/ -o obj/calculate.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -module obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app obj/environment.o obj/io_func.o obj/calculate.o obj/main.o
	rm -rf *.yaml
	rm -rf *.mod

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt
