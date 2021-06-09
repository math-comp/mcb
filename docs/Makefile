all: book.pdf coq artwork revision

.PHONY: revision
revision:
	sed 's/<!--date-->.*/<!--date-->(last update $(shell date))/' -i index.html

book.pdf:
	make -C ../tex all && cp ../tex/book.pdf .

coq:
	make -C ../coq && cp -r ../coq/ch*.html ../coq/index.html ../coq/node_modules snippets

artwork:
	cp ../artwork/cover/cover-front-web.png .

clean:
	rm book.pdf ch*.html cover-front-web.png

