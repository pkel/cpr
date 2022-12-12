export CPR_MALFORMED_DAG_TO_FILE=/tmp/malformed.dot
export CPR_VERSION=local-$(shell git describe --tags --dirty || git describe --all --long --dirty)

python=python3.9

build:
	opam exec dune build

test:
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

format: _venv
	opam exec dune -- build @fmt --auto-promote || true
	_venv/bin/black . || true

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
	opam switch create . --package=ocaml-variants.4.12.1+options,ocaml-option-flambda --no-install --yes
	opam install ./cpr.opam --deps-only --working-dir --yes

setup:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
	opam switch create . --package=ocaml-variants.4.12.1+options,ocaml-option-flambda --no-install --yes
	opam install . --deps-only --working-dir --yes

dependencies:
	opam exec dune build {cpr,cpr-dev}.opam
	opam install . --deps-only --working-dir

_venv: requirements.txt
	${python} -m venv _venv
	_venv/bin/python -m pip install --upgrade pip
	_venv/bin/python -m pip install wheel
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

# RL

reset-config:
	rm -f experiments/train/config.ini

train-online: _venv
	if [ ! -e experiments/train/config.ini ] ; then\
		cp experiments/train/defaults.ini experiments/train/config.ini ; fi
	${EDITOR} experiments/train/config.ini
	. _venv/bin/activate && python experiments/train/ppo.py

train-offline: export WANDB_MODE=offline
train-offline: _venv train-online
