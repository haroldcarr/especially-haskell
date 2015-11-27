#
# Created       : 2015 Oct 28 (Wed) 17:42:11 by Harold Carr.
# Last Modified : 2015 Nov 27 (Fri) 14:13:06 by Harold Carr.
#

GENERATED = .generated
INFRA = infrastructure/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/bin/infra

# TODO why is hcEmacsOrgModeExportHtml output relative to input rather than absolute?
all :
	cd infrastructure; stack build
	$(INFRA) graphviz-example/src/graphviz.hs graphviz-example/$(GENERATED)/graphviz.hs.org
	cd graphviz-example; stack build
	dot -Tpng graphviz-example/src/ex3.dot > graphviz-example/$(GENERATED)/ex3-dot.png
	cd graphviz-example; stack exec graphviz-example $(GENERATED)
	hcEmacsOrgModeExportHtml graphviz-example/src/graphviz.org ../$(GENERATED)/graphviz.html

clean :
	cd infrastructure; stack clean
	cd graphviz-example; stack clean; rm $(GENERATED)/*

# End of file.
