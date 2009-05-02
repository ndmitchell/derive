mkdir obj
copy *.* obj\*.*
chdir obj
lhs2tex derive.tex -o final.tex
bibtex final
texify final.tex
cd ..
del derive.dvi
copy obj\final.dvi derive.dvi
