ghc tatex.hs
./tatex $1
uplatex -src -interaction=nonstopmode $1.tex
#dvipdfmx $1.dvi
rm *.o
rm *.hi
rm *.log
rm *.aux
rm *.dvi
rm *.tex

