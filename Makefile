export CPR_MALFORMED_DAG_TO_FILE=/tmp/malformed.dot
export CPR_VERSION=local-$(shell git describe --tags --dirty || git describe --all --long --dirty)

python=python3.9

build:
	opam exec dune build

test:
	opam exec dune runtest
	make bridge _venv
	_venv/bin/pytest --forked

watch-malformed-dag:
	echo "$$CPR_MALFORMED_DAG_TO_FILE" \
		| entr -r bash -c "clear; cat \"$$CPR_MALFORMED_DAG_TO_FILE\" ; dot \"$$CPR_MALFORMED_DAG_TO_FILE\" -Tpng | feh - -."

check-format: _venv
	opam exec dune build @fmt
	_venv/bin/black --check .
	_venv/bin/flake8

format: _venv
	opam exec dune -- build @fmt --auto-promote || true
	_venv/bin/black . || true

pre-commit: check-format test

setup:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
	opam switch create . --package=ocaml-variants.4.12.1+options,ocaml-option-flambda
	opam install . --deps-only --working-dir

dependencies:
	opam exec dune build {cpr,cpr-dev}.opam
	opam install . --deps-only --working-dir

_venv: requirements.txt
	${python} -m venv _venv
	_venv/bin/python -m pip install --upgrade pip
	_venv/bin/python -m pip install wheel
	_venv/bin/pip install -r requirements.txt
	touch _venv

# bridge OCaml and Python

bridge gym/cpr_gym/bridge.so:
	opam exec dune -- build --release simulator/gym/bridge.so
	rm -f gym/cpr_gym/bridge.so
	cp _build/default/simulator/gym/bridge.so gym/cpr_gym/bridge.so

update-bridge-from-ci:
	_venv/bin/python -m cpr_gym --update
	_venv/bin/python -m cpr_gym --version

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

# RL

reset-config:
	rm -f experiments/train/config.ini

train-online: bridge _venv
	if [ ! -e experiments/train/config.ini ] ; then\
		cp experiments/train/defaults.ini experiments/train/config.ini ; fi
	${EDITOR} experiments/train/config.ini
	. _venv/bin/activate && python experiments/train/ppo.py

train-offline: export WANDB_MODE=offline
train-offline: _venv train-online
