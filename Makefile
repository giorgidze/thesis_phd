LHS2TEX = lhs2TeX --poly --output=main.tex main.lhs
PERL1   = perl -p -i -e 's/\\begin\{hscode\}/\\begin\{samepage\}\\onehalfspacing\n\\begin{hscode}/' main.tex
PERL2   = perl -p -i -e 's/\\end\{hscode\}\\resethooks/\\end\{hscode\}\\resethooks\n\\doublespacing\\end\{samepage\}\\vspace\{ -8 mm \}/' main.tex
LATEX   = pdflatex -halt-on-error -file-line-error main.tex
BIBTEX  = bibtex main

all: clean
	$(LHS2TEX)
	$(PERL1)
	$(PERL2)
	$(LATEX)
	$(BIBTEX)
	$(LATEX)
	$(LATEX)

clean:
	rm -f *.aux *.blg *.log *.ptb *.toc *.out *.lof *.lot *.tex *.bbl
