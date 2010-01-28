all: R-intro.pdf

R-intro.pdf: R-intro.tex
	pdflatex R-intro.tex

R-intro.tex: R-intro.Rnw
	R CMD Sweave R-intro.Rnw

clean:
	rm -rf *.{pdf,tex,aux,log,synctex.gz,fdb_latexmk,out} figs
