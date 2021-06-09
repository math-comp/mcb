all: book.pdf index.html artwork revision

.PHONY: revision
revision:
	sed 's/<!--date-->.*/<!--date-->(last update $(shell date))/' -i index.html

book.pdf:
	make -C ../tex all && cp ../tex/book.pdf .

index.html: chapterlist.html
	cat index.htpl | sed '/-*- CHAPTER_LIST -*-/r chapterlist.html' > $@

chapterlist.html: chapters.txt
	awk -F@@@ '{printf "<a href=\"snippets/%s\">Chapter %s</a><br>\n", $$1, $$2}' $^ > $@

chapters.txt: coq
	cp ../coq/chapters.txt .

coq:
	make -C ../coq && cp -r ../coq/ch*.html ../coq/index.html ../coq/node_modules snippets

artwork:
	cp ../artwork/cover/cover-front-web.png .

iclean:
	rm index.html chapterlist.html chapters.txt

sclean: iclean
	rm -rf snippets
	mkdir snippets

clean: sclean
	rm book.pdf cover-front-web.png snippets/ch*.html
