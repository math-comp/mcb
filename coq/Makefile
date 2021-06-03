files=ch0.v  ch1.v  ch2.v  ch3.v  ch4.v  ch6.v  ch7.v ch7.1.v ch7.2.v ch7.3.v ch7.4.v ch7.5.v

all: $(files:%.v=%.html) index.html

ch%.html: ch%.v
	cat jscoq_header.html > $@
	echo "<h1>Chapter $*</h1><div><textarea id='coq-code'>" >> $@
	cat $< >> $@
	echo >> $@
	echo "</textarea></div>" >> $@
	echo "<script type='text/javascript'>" >> $@
	echo "var coqdoc_ids = ['coq'];" >> $@
	echo "</script>" >> $@
	cat jscoq_footer.html >> $@

index.html:
	cat index_header.html > $@
	for file in $(files) ; do \
		echo -n "<a href=\"" >> $@ ; \
		echo -n $$file | sed "s/\.v/\.html/" >> $@ ; \
		echo -n "\">" >> $@ ; \
                echo -n "Chapter " >> $@ ; \
                echo -n $$file | sed "s/\.v//" >> $@ ; \
                echo "</a><br> " >> $@ ; \
	done
	cat index_footer.html >> $@

clean:
	rm -f $(files:%.v=%.html) index.html
