.PHONY: run-fraction clean

run-fraction: fraction
	./fraction

fraction: fraction.cpp
	g++ fraction.cpp -o fraction -std=c++14 -g -O0 -fsanitize=address -fsanitize=undefined -static-libasan

# https://pandoc.org/MANUAL.html#literate-haskell-support
index.html: Reference.lhs makefile header
	pandoc --standalone -f markdown+lhs Reference.lhs -t html -o index.html --highlight-style=tango -H header --mathjax

kmett.html: Fractions.lhs makefile header
	pandoc --standalone -f markdown+lhs Fractions.lhs -t html -o kmett.html --highlight-style=tango -H header

clean:
	rm ./fracton *.html
