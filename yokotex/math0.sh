ghc math0.hs
./math0 $@
uplatex -src -interaction=nonstopmode "math0".tex
dvipdfmx "math0".dvi
rm math0.o
rm math0.hi
rm Myfile.o
rm Myfile.hi
rm math0.log
rm math0.aux
rm math0.dvi
