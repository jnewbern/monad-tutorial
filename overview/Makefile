default: ilc09-monad-tutorial.pdf

ilc09-monad-tutorial.pdf: ilc09-monad-tutorial.tex ilc09-monad-tutorial.bib
	latex $<
	bibtex $(basename $<)
	latex $<
	latex $<
	pdflatex $<

clean:
	rm -f ilc09-monad-tutorial.pdf
	rm -f *.aux *.bbl *.log *.dvi
