#
# Created       : 2015 Oct 28 (Wed) 17:42:11 by Harold Carr.
# Last Modified : 2015 Dec 14 (Mon) 18:54:44 by Harold Carr.
#

G                   = .generated

all : pdf html

pdf:
	cd src; pandoc      -t beamer --template tim.beamer \
                      -o ../$(G)/RS.pdf   -V theme:tim RS.lhs

html:
	cd src; pandoc -s   -o ../$(G)/RS.html               RS.lhs

clean :
	stack clean
	rm $(G)/*

# End of file.
