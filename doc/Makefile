DOCPATH =/usr/local/share/doc/libadacrypt-dev
DE_PATH=./de
EN_PATH=./en

.PHONY : clean
.SILENT : clean


all: de en
clean : clean_de clean_en
de: dvi_de ps_de pdf_de
en: dvi_en ps_en pdf_en

dvi_de:
	latex  acldoc_de
	bibtex acldoc_de
	latex  acldoc_de
	bibtex acldoc_de

pdf_de:dvi_de
	dvipdf acldoc_de.dvi acldoc-de.pdf 

ps_de:dvi_de
	dvips -o  acldoc-de.ps acldoc_de.dvi


clean_de:
	rm -f *.ps *.pdf *.dvi *.log *~ *.toc *.aux *.bbl *.blg 
	rm -f $(DE_PATH)/*~  $(DE_PATH)/*aux 




dvi_en:
	latex  acldoc_en
	bibtex acldoc_en
	latex  acldoc_en
	bibtex acldoc_en

pdf_en:dvi_en
	dvipdf acldoc_en.dvi acldoc-en.pdf 

ps_en:dvi_en
	dvips -o  acldoc-en.ps acldoc_en.dvi


clean_en:
	rm -f *.ps *.pdf *.dvi *.log *~ *.toc *.aux *.bbl *.blg 
	rm -f $(EN_PATH)/*~  $(EN_PATH)/*aux 

install :
	mkdir -p $(DOCPATH)
	cp  *.ps $(DOCPATH)/*.ps
	cp  *.pdf $(DOCPATH)/*.pdf

uninstall:	
	rm -f $(DOCPATH)/acldoc-de.ps
