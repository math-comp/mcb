files=ch0.v  ch1.v  ch2.v  ch3.v  ch4.v  ch6.v  ch7_1.v ch7_2.v ch7_3.v ch7_4.v ch7_5.v  ch8.v

all: check $(files:%.v=%.html) index.html

check:
	for f in $(files); do coqc $$f || exit 1; done

ch%.html: ch%.v
	cat jscoq_header.htpl > $@
	echo "<h1>Chapter $*</h1><div><textarea id='coq-code'>" >> $@
	cat $< >> $@
	echo >> $@
	echo "</textarea></div>" >> $@
	echo "<script type='text/javascript'>" >> $@
	echo "var coqdoc_ids = ['coq'];" >> $@
	echo "</script>" >> $@
	cat jscoq_footer.htpl >> $@

chapterlist.html:
	echo > $@
	for file in $(files) ; do \
		echo -n "<a href=\"" >> $@ ; \
		echo -n $$file | sed "s/\.v/\.html/" >> $@ ; \
		echo -n "\">" >> $@ ; \
                echo -n "Chapter " >> $@ ; \
                echo -n $$file | sed "s/\.v//" >> $@ ; \
                echo "</a><br> " >> $@ ; \
	done

index.html: chapterlist.html
	cat index.htpl | sed '/-*- CHAPTER_LIST -*-/r chapterlist.html' > $@

clean: cclean
	rm -f $(files:%.v=%.html) chapterlist.html index.html

# clean compilation artifacts
cclean:
	rm -f $(files:%.v=%.vo) $(files:%.v=%.vok) $(files:%.v=%.vos) $(files:%.v=.%.aux) $(files:%.v=%.glob)
