.PHONY: dependencies format setup

format:
	dune build @fmt --auto-promote

setup:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
	opam switch create . "4.11.1+flambda" --deps-only

dependencies:
	dune build cpr.opam cpr-dev.opam
	opam install . --deps-only --working-dir
