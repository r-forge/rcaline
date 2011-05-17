sweave := SWEAVE_STYLEPATH_DEFAULT="TRUE" R CMD SWEAVE

all : 

vignette : Rcaline.pdf

%.pdf : %.tex %.bbl
	latex $<
	pdflatex $<

%.bbl : %.aux
	bibtex $<

%.aux : %.tex
	latex $<

%.tex : %.Rnw
	$(sweave) $<

clean : 
	rm -f *.log *.aux *.dvi *.eps *.pdf *.tex *.bbl *.blg
