all: book.pdf jscoq coq artwork

jscoq:
	git clone https://github.com/ejgallego/jscoq-builds.git --depth 1 jscoq
	git checkout c219bd7e4b207846a607ce5a19513412d826ce7a

book.pdf:
	make -C ../tex all && cp ../tex/book.pdf .

coq:
	make -C ../coq && cp ../coq/ch*.html .

artwork:
	cp ../artwork/cover/cover-front-web.png .

clean:
	rm book.pdf ch*.html cover-front-web.png

spell:
	for f in ../tex/*.tex; do cat $$f | aspell --master=en_US -t list; done | sort | uniq | comm -2 -3 - ../tex/sorted_words_to_ignore.txt
