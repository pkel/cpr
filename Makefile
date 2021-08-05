build:
	dune build

test: build
	dune runtest
	pytest python

check-format:
	dune build @fmt
	black --check python
	cd python && flake8

format:
	dune build @fmt --auto-promote
	black python

pre-commit: check-format test

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

setup:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
	opam switch create . "4.11.1+flambda" --deps-only
	opam install dune

dependencies:
	dune build ocaml/{cpr,cpr-dev}.opam
	opam install ./ocaml --deps-only --working-dir
