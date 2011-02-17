all: build run movie

build: simple_advection
run: h.out
movie: simple_advection.mp4

simple_advection: 
	gfortran simple_advection.f90 -o simple_advection

h.out:
	./simple_advection

simple_advection.mp4: 
	Rscript make_movie.R
	open simple_advection.mp4

clean: 
	rm simple_advection h.out simple_advection.mp4
