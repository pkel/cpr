export CPR_MALFORMED_DAG_TO_FILE=/tmp/malformed.dot

build:
	cd ocaml && dune build

test: build bridge
	cd ocaml && dune runtest
	cd python && pytest

watch-malformed-dag:
	echo "$$CPR_MALFORMED_DAG_TO_FILE" \
		| entr -r bash -c "clear; cat \"$$CPR_MALFORMED_DAG_TO_FILE\" ; dot \"$$CPR_MALFORMED_DAG_TO_FILE\" -Tpng | feh - -."

check-format:
	cd ocaml && dune build @fmt
	cd python && black --check .
	cd python && flake8

format:
	cd ocaml && dune build @fmt --auto-promote
	cd python && black .

pre-commit: check-format test

setup:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
	opam switch create . "4.11.1+flambda"
	opam install ./ocaml --deps-only --working-dir

dependencies:
	dune build ocaml/{cpr,cpr-dev}.opam
	opam install ./ocaml --deps-only --working-dir

# bridge OCaml and Python

bridge: python/gym/cpr_gym/bridge.so

python/gym/cpr_gym/bridge.so: _build/default/ocaml/gym/bridge.so
	cp $< $@

# long-running simulations

simulate:
	mkdir -p data
	dune exec ocaml/experiments/honest_net.exe -- data/honest_net.tsv
	dune exec ocaml/experiments/withholding.exe -- data/withholding.tsv

expand:
	python python/eval/honest_net.py
	python python/eval/withholding.py

# visualizations from short simulations

visualize:
	mkdir -p fig/chains/
	rm -rf fig/chains/*
	dune exec ocaml/experiments/visualize.exe
	make -j $(shell nproc) visualize.render

.SECONDEXPANSION:
visualize.render: $$(patsubst %.dot, %.png, $$(wildcard fig/chains/*.dot))

%.png: %.dot
	dot -Tpng < $^ > $@
