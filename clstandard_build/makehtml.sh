# makehtml.sh
# make4ht file.tex "mathml"
# make4ht -d ./output/html/
make4ht -d ./output/html/ chap-1.tex "mathml" | less



make4ht -f html5+detect_engine chap-1.tex
make4ht -f html5+detect_engine chap-6.tex
make4ht -f html5+detect_engine chap-6.tex



# makehtml.sh
# make4ht file.tex "mathml"
# make4ht -d ./output/html/
make4ht -d ./output/html/ chap-1.tex "mathml"
make4ht -d ./output/html/ includer.tex "mathml"
make4ht includer "mathml"

make4ht -f html5+detect_engine includer
