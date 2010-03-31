SRC=pure-diffusion.f90 rand_wrapper.c

all:
	R CMD COMPILE FCFLAGS='-O2' $(SRC)
	R CMD SHLIB $(SRC)

clean: 
	rm *.o *.so
