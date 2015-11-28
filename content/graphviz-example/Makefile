#
# Created       : 2015 Oct 28 (Wed) 17:42:11 by Harold Carr.
# Last Modified : 2015 Nov 28 (Sat) 15:36:00 by Harold Carr.
#

G                   = .generated
INFRA               = ../../infrastructure/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/bin/infra
GRAPHVIZ_EXECUTABLE = .stack-work/install/x86_64-osx/lts-3.15/7.10.2/bin/graphviz-example

all : $(G)/graphviz.hs.org $(G)/ex1.png $(G)/ex3-dot.png $(G)/graphviz.html

# hs_to_org
$(G)/graphviz.hs.org : src/graphviz.hs
	$(INFRA) $< $@

$(G)/ex1.png : $(GRAPHVIZ_EXECUTABLE) src/graphviz.hs
	stack exec graphviz-example $(G)

$(G)/ex3-dot.png : src/ex3.dot
	dot -Tpng $< > $@

# TODO why is hcEmacsOrgModeExportHtml output relative to input rather than absolute?
# org_to_html
$(G)/graphviz.html : src/graphviz.org
	hcEmacsOrgModeExportHtml $< ../$(G)/graphviz.html

$(GRAPHVIZ_EXECUTABLE) : src/graphviz.hs src/WriteRunDot.hs
	stack build

clean :
	stack clean
	rm $(G)/*

# End of file.
