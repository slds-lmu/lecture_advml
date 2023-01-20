SLOT1=slides-1-automl-intro
SLOT2=slides-2-tuning
SLOT3=slides-3-smbo
SLOT4=slides-4-hyperband
SLOT5=slides-5-metalearning
SLOT6=slides-6-maml
SLOT7=slides-7-stacking
SLOT8=slides-8-openml

all: texclean $(SLOT1).pdf $(SLOT2).pdf 	$(SLOT3).pdf $(SLOT4).pdf $(SLOT5).pdf  $(SLOT6).pdf  $(SLOT7).pdf  $(SLOT8).pdf        

$(SLOT1).pdf: $(SLOT1).Rnw
	Rscript -e 'knitr::knit2pdf("$(SLOT1).Rnw")'

$(SLOT2).pdf: $(SLOT2).Rnw
	Rscript -e 'knitr::knit2pdf("$(SLOT2).Rnw")'

$(SLOT3).pdf: $(SLOT3).Rnw
	Rscript -e 'knitr::knit2pdf("$(SLOT3).Rnw")'

$(SLOT4).pdf: $(SLOT4).Rnw
	Rscript -e 'knitr::knit2pdf("$(SLOT4).Rnw")'

$(SLOT5).pdf: $(SLOT5).Rnw
	Rscript -e 'knitr::knit2pdf("$(SLOT5).Rnw")'

$(SLOT6).pdf: $(SLOT6).Rnw
	Rscript -e 'knitr::knit2pdf("$(SLOT6).Rnw")'

$(SLOT7).pdf: $(SLOT7).Rnw
	Rscript -e 'knitr::knit2pdf("$(SLOT7).Rnw")'

$(SLOT8).pdf: $(SLOT8).Rnw
	Rscript -e 'knitr::knit2pdf("$(SLOT8).Rnw")'
		

texclean: 
	rm -rf *.tex
	rm -rf *.out
	rm -rf *.dvi
	rm -rf *.log
	rm -rf *.aux
	rm -rf *.bbl
	rm -rf *.blg
	rm -rf *.ind
	rm -rf *.idx
	rm -rf *.ilg
	rm -rf *.lof
	rm -rf *.lot
	rm -rf *.toc
	rm -rf *.nav
	rm -rf *.snm
	rm -rf *.vrb
	rm -rf *.synctex.gz
	rm -rf *-concordance.tex


cleancache:
	rm -rf *.pdf
	rm -rf cache
	rm -rf figure	 