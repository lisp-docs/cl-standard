texi2any -o ./output/html/ --html bison.texi 
make4ht -d ./output/html/ ./ "mathml" | less
make4ht -d ./output/html/ chap-1.tex "mathml" | less
make4ht -d ./output/html/ ./spec-source/chap-1.dvi.Z "mathml"
make4ht -d ../output/html/ chap-1.dvi.Z "mathml"
