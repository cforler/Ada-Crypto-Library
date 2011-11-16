.SILENT:

MAKEINFO	= $(shell which makeinfo 2> /dev/null)
DVIPS		= $(shell which dvips 2> /dev/null)
TEXI2DVI	= $(shell which texi2dvi 2> /dev/null)
PDFTEX		= $(shell which pdftex 2> /dev/null)

aunit_docs: version aunit.pdf aunit.html aunit.txt aunit.info
	echo ""
	echo Documentation built with success.

version:
	sed 's/^/@set /g;s/=/ /g' version_information  > version.texi

%.dvi: %.texi
ifneq (${TEXI2DVI},)
	echo Building $@
	${TEXI2DVI} --expand --clean --quiet $<
else
	@echo "--------------------------------------------------------"
	@echo "texi2dvi not found, cannot build DVI or PS documentation"
	@echo "--------------------------------------------------------"
endif

%.ps: %.dvi
ifneq (${DVIPS},)
	echo Building $@
	${DVIPS} -q $< -o $@
else
	@echo "------------------------------------------------------"
	@echo "dvips not found, cannot build POSTSCRIPT documentation"
	@echo "------------------------------------------------------"
endif

%.pdf: %.texi
ifneq (${TEXI2DVI},)
ifneq (${PDFTEX},)
	echo Building $@
	${TEXI2DVI} -p --expand --clean --quiet $<
else
	@echo "-----------------------------------------------"
	@echo "pdftex not found, cannot build PDF documentation"
	@echo "-----------------------------------------------"
endif
else
	@echo "--------------------------------------------------"
	@echo "texi2dvi not found, cannot build PDF documentation"
	@echo "--------------------------------------------------"
endif

%.info: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	${MAKEINFO} $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build INFO documentation"
	@echo "---------------------------------------------------"
endif

%.html: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	${MAKEINFO} --html --no-split --css-include=aunit.css --ifinfo $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build HTML documentation"
	@echo "---------------------------------------------------"
endif

%.txt: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	${MAKEINFO} --plaintext --no-headers $< --output $@
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build TEXT documentation"
	@echo "---------------------------------------------------"
endif

clean:
	-$(RM) -f aunit.dvi aunit.html aunit.info* aunit.log 
	-$(RM) -f aunit.ps aunit.pdf aunit.txt *~ version.texi
