#
# Created       : 2015 Oct 28 (Wed) 17:42:11 by Harold Carr.
# Last Modified : 2017 Jun 11 (Sun) 11:42:40 by Harold Carr.
#

G                   = .generated

all : pdf pdf17 html

pdf:
	cd src; pandoc RS.lhs \
                       -o ../$(G)/RS.pdf \
                       -V theme:tim \
                       -t beamer \
                       --template tim.beamer

pdf17:
	cd src; pandoc RSL17.lhs \
                       -o ../$(G)/RSL17.pdf \
                       --variable fontsize=14pt \
                       -V theme:tim \
                       -t beamer \
                       --template tim.beamer

html:
	cd src; pandoc RS.lhs \
                       -o ../$(G)/RS.html \
                       -s
	cd src; pandoc RSL17.lhs \
                       -o ../$(G)/RSL17.html \
                       -s

md:
	cd src; pandoc RSL17.lhs \
                       -o ../$(G)/RSL17.md \
                       -s

clean :
	stack clean --full
	rm $(G)/*

# End of file.
