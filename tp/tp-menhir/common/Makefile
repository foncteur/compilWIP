.PHONY: all clean

PDFLATEX=pdflatex
BIBTEX=bibtex

SOURCE=main.tex

all: main.pdf

main.pdf: main.tex
	TEXINPUTS=common:$$TEXINPUTS $(PDFLATEX) main.tex

clean:
	rm -f *.aux *.bbl *.pdf *.dvi *.ps *.idx *.out *.log *.ans *.blg *~
