.PHONY: build dependencies expand format simulate setup test visualize visualize.render

build:
	dune build

test:
	dune runtest

simulate:
	mkdir -p data
	dune exec experiments/honest_net.exe -- data/honest_net.tsv
	dune exec experiments/withholding.exe -- data/withholding.tsv

visualize:
	mkdir -p fig/chains/
	rm -rf fig/chains/*
	dune exec experiments/visualize.exe
	make -j $(shell nproc) visualize.render

.SECONDEXPANSION:
visualize.render: $$(patsubst %.dot, %.png, $$(wildcard fig/chains/*.dot))

%.png: %.dot
	dot -Tpng < $^ > $@

expand:
	 pipenv run python eval/honest_net.py
	 pipenv run python eval/withholding.py

format:
	dune build @fmt --auto-promote

setup:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
	opam switch create . "4.11.1+flambda" --deps-only

dependencies:
	dune build cpr.opam cpr-dev.opam
	opam install . --deps-only --working-dir
