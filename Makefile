.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile = tap-$(version).tgz

SWIPL := swipl

all: test

version:
	@echo $(version)

check: test

install:
	@echo "(none)"

test:
	@$(SWIPL) -q -g "asserta(user:file_search_path(library, 'prolog')),main,halt(0)" -t "halt(1)" -s test/examples.pl

package: test
	tar cvzf $(packfile) prolog test pack.pl README.md LICENSE

release: test
	hub release create -m v$(version) v$(version)
