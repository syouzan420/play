ghc eigoMain.hs
./eigoMain $@
uplatex -src -interaction=nonstopmode "eigoQ".tex
uplatex -src -interaction=nonstopmode "eigoA".tex
dvipdfmx "eigoQ".dvi
dvipdfmx "eigoA".dvi
rm eigoMain.o
rm eigoMain.hi
rm Myfile.o
rm Myfile.hi
rm eigoQ.log
rm eigoQ.aux
rm eigoQ.dvi
rm eigoQ.tex
rm eigoA.log
rm eigoA.aux
rm eigoA.dvi
rm eigoA.tex
