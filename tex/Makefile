quick: gitrev
	pdflatex main && cp main.pdf book.pdf
all: gitrev
	pdflatex main
	bibtex main
	pdflatex main
	pdflatex main
	pdflatex main && cp main.pdf book.pdf
.PHONY: gitrev
gitrev:
	git describe --tags > gitrev

clean:
	rm -rf .*/rt *.aux

COQBIN ?= 
COQC := $(COQBIN)coqc
MC ?= 
COQARGS := "-no-glob"

coq: $(patsubst %.tex,.%/rt,$(wildcard *.tex))

.%/rt : %.tex
	@../roundtrip/rt \
		--coqc $(COQC) \
		--coqargs $(COQARGS) \
		--defenv coqdef \
		--run coqrun \
		--outenv coqout \
		--prtenv coq $*.tex
	@touch $@

stats:
	gitinspector -F htmlembedded -f tex -T -r -H  > stats.html
