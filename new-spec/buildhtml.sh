texi2any -o ./output/html/ --html bison.texi 
make4ht -d ./output/html/ ./ "mathml" | less
make4ht -d ./output/html/ chap-1.tex "mathml" | less
make4ht -d ./output/html/ ./spec-source/chap-1.dvi.Z "mathml"
make4ht -d ../output/html/ chap-1.dvi.Z "mathml"


# makehtml.sh
# make4ht file.tex "mathml"
# make4ht -d ./output/html/
make4ht -d ./output/html/ chap-1.tex "mathml" | less



make4ht -f html5+detect_engine chap-1.tex
make4ht -f html5+detect_engine chap-6.tex
make4ht -f html5+detect_engine chap-6.tex

pdftex includer.tex
make4ht -f html5+detect_engine includer.tex
