all: draft2.pdf

draft2.pdf: draft2.tex
	latex draft2.tex >/dev/null 2>&1
	bibtex draft2.aux >/dev/null 2>&1
	latex draft2.tex >/dev/null 2>&1
	latex draft2.tex >/dev/null 2>&1
	pdflatex draft2.tex >/dev/null 2>&1
	gnome-open draft2.pdf >/dev/null 2>&1

v:
	latex draft2.tex
	bibtex draft2.aux
	latex draft2.tex
	latex draft2.tex
	pdflatex draft2.tex
	gnome-open draft2.pdf

clean:
	rm draft2.pdf
