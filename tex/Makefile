quick:
	pdflatex main && cp main.pdf book.pdf
all:
	pdflatex main
	bibtex main
	pdflatex main
	pdflatex main
	pdflatex main && cp main.pdf book.pdf

clean:
	rm -rf .*/rt *.aux

COQBIN ?= /home/gares/COQ/coq/bin/
COQC := $(COQBIN)/coqc
MC ?= /home/gares/INRIA/MathComp/coqfinitgroup
COQARGS := "-I $(MC)/ssreflect/v8.5/src -R $(MC)/theories Ssreflect"

coq: $(patsubst %.tex,.%/rt,$(wildcard *.tex))

.%/rt:%.tex
	@../roundtrip/rt \
		--coqc $(COQC) \
		--coqargs $(COQARGS) \
		--defenv coqdef \
		--run coqrun \
		--outenv coqout \
		--prtenv coq $*.tex
	@touch $@
