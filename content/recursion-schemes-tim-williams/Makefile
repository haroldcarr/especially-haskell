all: pdf html

pdf:
	cd src; pandoc    -t beamer --template tim.beamer -o ../.generated/RS.pdf   -V theme:tim RS.lhs

html:
	cd src; pandoc -s                                 -o ../.generated/RS.html               RS.lhs

clean:
	rm .generated/*
