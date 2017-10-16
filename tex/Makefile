quick: gitrev
	pdflatex main && cp main.pdf book.pdf
all: gitrev
	pdflatex main
	makeindex main.idx
	biber main
	biber main
	pdflatex main
	pdflatex main
	pdflatex main && cp main.pdf book.pdf
.PHONY: gitrev
gitrev:
	date > gitrev
	echo , >> gitrev
	git describe --tags >> gitrev

clean:
	rm -rf .*/rt *.aux *.bbl *.bcf *.idx *.out *.toc *.ilg *.ind *.listing *.ptc *.run.xml

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

spell: dictionary spell_without_dic

dictionary:
	for g in spell_files/*.txt; do cat $$g; done| sort | uniq > spell_files/all_sorted_words_to_ignore.txt

spell_without_dic:
	for f in *.tex; do cat $$f | aspell --master=en_US -t list; done | sort | uniq | comm -2 -3 - spell_files/all_sorted_words_to_ignore.txt
