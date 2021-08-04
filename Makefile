.PHONY: build dependencies expand format simulate setup test visualize visualize.render

build:
	dune build

test: build
	dune runtest
	pytest python

simulate:
	mkdir -p data
	dune exec ocaml/experiments/honest_net.exe -- data/honest_net.tsv
	dune exec ocaml/experiments/withholding.exe -- data/withholding.tsv

visualize:
	mkdir -p fig/chains/
	rm -rf fig/chains/*
	dune exec ocaml/experiments/visualize.exe
	make -j $(shell nproc) visualize.render

.SECONDEXPANSION:
visualize.render: $$(patsubst %.dot, %.png, $$(wildcard fig/chains/*.dot))

%.png: %.dot
	dot -Tpng < $^ > $@

expand:
	python python/eval/honest_net.py
	python python/eval/withholding.py

format:
	dune build @fmt --auto-promote

setup:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
	opam switch create . "4.11.1+flambda" --deps-only
	opam install dune

dependencies:
	dune build ocaml/{cpr,cpr-dev}.opam
	opam install ./ocaml --deps-only --working-dir
