INFRA = infrastructure/.stack-work/install/x86_64-osx/lts-3.7/7.10.2/bin/infra

all :
	cd infrastructure; stack build
	$(INFRA) graphviz-example/src/graphviz.hs graphviz-example/src/graphviz.hs.org
	cd graphviz-example; stack build

