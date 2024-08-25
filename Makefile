export CPR_MALFORMED_DAG_TO_FILE=/tmp/malformed.dot
export CPR_VERSION=$(shell git describe --tags --dirty || git describe --all --long --dirty)

python=$(shell tools/select_python.sh 3.9 3.10)

.PHONY: build
build:
	opam exec dune build

test: build
	opam exec dune runtest
	make _venv
	_venv/bin/pytest --forked --benchmark-disable

watch-malformed-dag:
	echo "$$CPR_MALFORMED_DAG_TO_FILE" \
		| entr -r bash -c "clear; cat \"$$CPR_MALFORMED_DAG_TO_FILE\" ; dot \"$$CPR_MALFORMED_DAG_TO_FILE\" -Tpng | feh - -."

check-format: _venv
	opam exec dune build @fmt
	_venv/bin/black --check .
	_venv/bin/flake8
	git ls-tree -r HEAD --name-only | grep Cargo.toml | while read -r line ; do \
		cargo fmt --check --manifest-path "$$line" ; done

format: _venv
	opam exec dune -- build @fmt --auto-promote || true
	_venv/bin/black . || true
	git ls-tree -r HEAD --name-only | grep Cargo.toml | while read -r line ; do \
		cargo fmt --manifest-path "$$line" ; done

pre-commit: check-format test

manylinux-opam:
	curl -LO https://github.com/ocaml/opam/releases/download/2.1.3/opam-2.1.3-x86_64-linux
	install opam-2.1.3-x86_64-linux /usr/local/bin/opam
	rm opam-2.1.3-x86_64-linux

musllinux-opam:
	apk add opam

macosx-opam:
	brew install opam

cibuildwheel-setup:
	opam init --auto-setup --disable-sandboxing --bare
	opam switch create . --packages=ocaml-variants.4.12.1+options,ocaml-option-flambda --yes --no-install
	opam install ./cpr.opam --deps-only --inplace-build --yes

setup:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
	opam switch create . --packages=ocaml-variants.4.12.1+options,ocaml-option-flambda --yes --deps-only --inplace-build

dependencies:
	opam exec dune build cpr.opam cpr-dev.opam
	opam install . --deps-only --inplace-build

_venv: setup.py requirements.txt
	${python} -m venv _venv
	_venv/bin/python -m pip install --upgrade 'pip<24.1'
	_venv/bin/pip install -r requirements.txt
	touch _venv

# long-running simulations

simulate:
	mkdir -p data
	dune exec experiments/simulate/honest_net.exe -- data/honest_net.tsv
	dune exec experiments/simulate/withholding.exe -- data/withholding.tsv

expand: _venv
	_venv/bin/python experiments/simuluate/honest_net.py
	_venv/bin/python experiments/simuluate/withholding.py

simulate-topology:
	nix-shell -p R -p rPackages.igraph --command "Rscript experiments/simulate-topology/create-networks.R"
	dune exec experiments/simulate-topology/igraph.exe

# visualizations from short simulations

visualize:
	mkdir -p data/viz/
	rm -rf data/viz/*
	echo '*' > data/viz/.gitignore
	dune exec experiments/simulate/visualize.exe
	make -j $(shell nproc) visualize.render

.SECONDEXPANSION:
visualize.render: $$(patsubst %.dot, %.png, $$(wildcard data/viz/*.dot))

%.png: %.dot
	dot -Tpng < $^ > $@
