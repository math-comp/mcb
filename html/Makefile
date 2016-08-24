all: book.pdf jscoq coq artwork

jscoq:
	git clone https://github.com/ejgallego/jscoq-builds.git --depth 1 jscoq

book.pdf:
	make -C ../tex all && cp ../tex/book.pdf .

coq:
	make -C ../coq && cp ../coq/ch*.html .

artwork:
	cp ../artwork/cover/cover-front-web.png .

clean:
	rm book.pdf ch*.html cover-front-web.png
