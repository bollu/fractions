fraction: fraction.cpp
	g++ fraction.cpp -o fraction -std=c++14

# https://pandoc.org/MANUAL.html#literate-haskell-support
index.html: Reference.lhs makefile header
	pandoc --standalone -f markdown+lhs Reference.lhs -t html -o index.html --highlight-style=tango -H header --mathjax

kmett.html: Fractions.lhs makefile header
	pandoc --standalone -f markdown+lhs Fractions.lhs -t html -o kmett.html --highlight-style=tango -H header

