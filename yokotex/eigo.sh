ghc eigo.hs
./eigo $@
uplatex -src -interaction=nonstopmode "eQuestion".tex
uplatex -src -interaction=nonstopmode "eAnswer".tex
dvipdfmx "eQuestion".dvi
dvipdfmx "eAnswer".dvi
rm eigo.o
rm eigo.hi
rm Myfile.o
rm Myfile.hi
rm eQuestion.log
rm eQuestion.aux
rm eQuestion.dvi
rm eAnswer.log
rm eAnswer.aux
rm eAnswer.dvi
