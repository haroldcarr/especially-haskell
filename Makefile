GENERATED = .generated
INFRA = infrastructure/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/bin/infra

all :
	cd infrastructure; stack build
	$(INFRA) graphviz-example/src/graphviz.hs graphviz-example/$(GENERATED)/graphviz.hs.org
	cd graphviz-example; stack build
	dot -Tpng graphviz-example/src/ex3.dot > graphviz-example/$(GENERATED)/ex3-dot.png
	cd graphviz-example; stack exec graphviz-example $(GENERATED)

clean :
	cd infrastructure; stack clean
	cd graphviz-example; stack clean

