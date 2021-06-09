all: book.pdf jscoq coq artwork revision

.PHONY: revision
revision:
	sed 's/<!--date-->.*/<!--date-->(last update $(shell date))/' -i index.html

jscoq:
	git clone https://github.com/ejgallego/jscoq-builds.git --depth 1 jscoq
	git checkout c219bd7e4b207846a607ce5a19513412d826ce7a

book.pdf:
	make -C ../tex all && cp ../tex/book.pdf .

coq:
	make -C ../coq && cp -r ../coq/ch*.html ../coq/index.html ../coq/node_modules snippets

artwork:
	cp ../artwork/cover/cover-front-web.png .

clean:
	rm book.pdf ch*.html cover-front-web.png

