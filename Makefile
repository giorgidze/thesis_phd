LHS2TEX = lhs2TeX --poly --output=main.tex main.lhs
LATEX   = pdflatex -halt-on-error -file-line-error main.tex
BIBTEX  = bibtex main

all: clean
	$(LHS2TEX)
	$(LATEX)
	$(BIBTEX)
	$(LATEX)
	$(LATEX)
	make clean

clean:
	rm -f *.aux *.blg *.log *.ptb *.toc *.out main.tex main.bbl
