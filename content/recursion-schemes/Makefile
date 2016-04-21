#
# Created       : 2015 Oct 28 (Wed) 17:42:11 by Harold Carr.
# Last Modified : 2016 Apr 20 (Wed) 11:57:54 by Harold Carr.
#

G                   = .generated

all : pdf html

pdf:
	cd src; pandoc RS.lhs -o ../$(G)/RS.pdf \
                        -V theme:tim -t beamer --template tim.beamer

html:
	cd src; pandoc RS.lhs -o ../$(G)/RS.html \
                        -s

clean :
	stack clean --full
	rm $(G)/*

# End of file.
