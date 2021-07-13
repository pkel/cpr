.PHONY: build dependencies expand format simulate setup test

build:
	dune build

test:
	dune runtest

simulate:
	mkdir -p data
	dune exec experiments/honest_net.exe -- data/honest_net.tsv
	dune exec experiments/withholding.exe -- data/withholding.tsv

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
