RT=./rt --coqc /home/gares/COQ/coq/bin/coqc --coqargs '-I /home/gares/INRIA/MathComp/coqfinitgroup/ssreflect/v8.5/src -R /home/gares/INRIA/MathComp/coqfinitgroup/theories Ssreflect'

H=@

all:
	$(H) $(RT) test.tex
	@ pdflatex test

clean:
	rm -f *.log *.aux test.pdf
