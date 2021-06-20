.PHONY: format configure

format:
	dune build @fmt --auto-promote

configure:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
