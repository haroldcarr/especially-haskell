GENERATED = .generated
INFRA = infrastructure/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/bin/infra

# remove nix from path and run this
all :
	cd infrastructure; stack build
	$(INFRA) graphviz-example/src/graphviz.hs graphviz-example/$(GENERATED)/graphviz.hs.org
	cd graphviz-example; stack build

# nix on path (for dot)
workaround :
	dot -Tpng graphviz-example/src/ex3.dot > graphviz-example/$(GENERATED)/ex3-dot.png
	cd graphviz-example; stack exec graphviz-example $(GENERATED)

