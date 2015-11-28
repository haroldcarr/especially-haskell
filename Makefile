#
# Created       : 2015 Oct 28 (Wed) 17:42:11 by Harold Carr.
# Last Modified : 2015 Nov 28 (Sat) 14:49:16 by Harold Carr.
#

GENERATED = .generated
INFRA = infrastructure/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/bin/infra
GRAPHVIZ_EXECUTABLE=/Users/carr/.sync/.esync/openhc/especially-haskell/graphviz-example/.stack-work/install/x86_64-osx/lts-3.15/7.10.2/bin/graphviz-example

all : graphviz

graphviz : $(GRAPHVIZ_EXECUTABLE) graphviz-example/$(GENERATED)/graphviz.hs.org graphviz-example/$(GENERATED)/ex1.png graphviz-example/$(GENERATED)/ex3-dot.png graphviz-example/$(GENERATED)/graphviz.html

graphviz-example/$(GENERATED)/ex1.png : graphviz-example/src/graphviz.hs
	cd graphviz-example; stack exec graphviz-example $(GENERATED)

graphviz-example/$(GENERATED)/ex3-dot.png : graphviz-example/src/ex3.dot
	dot -Tpng $< > $@

# hs_to_org
graphviz-example/$(GENERATED)/graphviz.hs.org : graphviz-example/src/graphviz.hs
	$(INFRA) $< $@

graphviz-clean-generated :
	cd graphviz-example; rm $(GENERATED)/*

graphviz-clean-compile :
	cd graphviz-example; stack clean

# TODO why is hcEmacsOrgModeExportHtml output relative to input rather than absolute?
# org_to_html
graphviz-example/$(GENERATED)/graphviz.html : graphviz-example/src/graphviz.org
	hcEmacsOrgModeExportHtml $< ../$(GENERATED)/graphviz.html

infra : compile
	cd infrastructure; stack build

$(GRAPHVIZ_EXECUTABLE) : graphviz-example/src/graphviz.hs
	cd graphviz-example; stack build

clean : graphviz-clean-generated graphviz-clean-compile
	cd infrastructure; stack clean

# End of file.
