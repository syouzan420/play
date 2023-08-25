ghc math.hs
./math $@
uplatex -src -interaction=nonstopmode "mathQ".tex
uplatex -src -interaction=nonstopmode "mathA".tex
dvipdfmx "mathQ".dvi
dvipdfmx "mathA".dvi
rm math.o
rm math.hi
rm Myfile.o
rm Myfile.hi
rm Yokotex.o
rm Yokotex.hi
rm mathQ.log
rm mathQ.aux
rm mathQ.dvi
rm mathQ.tex
rm mathA.log
rm mathA.aux
rm mathA.dvi
rm mathA.tex