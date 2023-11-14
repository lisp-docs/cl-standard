#!/bin/sh


# echo "%\!TEX TS-program = tex \n\input plain-4ht.tex \n\document\n" > plaintext_pre
# echo "\n\\\enddocument\n" > plaintext_post
for f in *.tex ; do
    echo "Processing $f"
    cat plaintext_pre >> tempfile
    cat "$f" >> tempfile
    cat plaintext_post >> tempfile
    mv tempfile "$f"
done






# sed -i "1i \%\!TEX TS-program = tex \n\input plain-4ht.tex \n\document\n" *.tex

# \%\!TEX TS-program = tex \n\input plain-4ht.tex \n\document\n

# \%\!TEX TS-program = tex \n\\input plain-4ht.tex \n\\document\n 
# sed -i "1i \%\!TEX TS-program = tex \n\\input plain-4ht.tex \n\\document\n" *.tex
# sed -i "1i \%\!TEX TS-program\n" *.tex
