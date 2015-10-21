COQBIN=/home/gares/COQ/coq/bin/
COQARGS="--coqargs '-I /home/gares/INRIA/MathComp/coqfinitgroup/ssreflect/v8.5/src -R /home/gares/INRIA/MathComp/coqfinitgroup/theories Ssreflect'"

RT=./rt --coqc $(COQBIN)coqc $(COQARGS)

H=@

all:
	$(H) $(RT) test.tex
	@ pdflatex test

clean:
	rm -f *.log *.aux test.pdf
