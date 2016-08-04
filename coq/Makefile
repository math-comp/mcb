files=ch0.v  ch1.v  ch2.v  ch3.v  ch4.v  ch6.v  ch7.v

all: $(files:%.v=%.html)

ch%.html: ch%.v
	cat jscoq_header.html > $@
	echo "<h1>Chapter $*</h1><div><textarea id='coq'>" >> $@
	cat $< >> $@
	echo >> $@
	echo "</textarea></div>" >> $@
	echo "<script type='text/javascript'>" >> $@
	echo "var coqdoc_ids = ['coq'];" >> $@
	echo "</script>" >> $@
	cat jscoq_footer.html >> $@

clean:
	rm -f $(files:%.v=%.html)

