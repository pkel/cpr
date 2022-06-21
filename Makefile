export CPR_MALFORMED_DAG_TO_FILE=/tmp/malformed.dot
export CPR_VERSION=local-$(shell git describe --tags --dirty || git describe --all --long --dirty)

python=python3.9

build:
	cd ocaml && opam exec dune build

test: build bridge _venv
	cd ocaml && opam exec dune runtest
	cd python && ../_venv/bin/pytest --forked

watch-malformed-dag:
	echo "$$CPR_MALFORMED_DAG_TO_FILE" \
		| entr -r bash -c "clear; cat \"$$CPR_MALFORMED_DAG_TO_FILE\" ; dot \"$$CPR_MALFORMED_DAG_TO_FILE\" -Tpng | feh - -."

check-format: _venv
	cd ocaml && opam exec dune build @fmt
	cd python && ../_venv/bin/black --check .
	cd python && ../_venv/bin/flake8

format: _venv
	cd ocaml && opam exec dune -- build @fmt --auto-promote || true
	cd python && ../_venv/bin/black . || true

pre-commit: check-format test

setup:
	ln -sf ../../tools/pre-commit-hook.sh .git/hooks/pre-commit
	opam switch create . "4.11.2+flambda"
	opam install ./ocaml --deps-only --working-dir

dependencies:
	opam exec dune build ocaml/{cpr,cpr-dev}.opam
	opam install ./ocaml --deps-only --working-dir

_venv: python/requirements.txt
	rm -rf _venv
	${python} -m venv _venv
	_venv/bin/python -m pip install --upgrade pip
	cd python && ../_venv/bin/pip install -r requirements.txt

# bridge OCaml and Python

bridge python/gym/cpr_gym/bridge.so:
	opam exec dune build ocaml/gym/bridge.so
	rm -f python/gym/cpr_gym/bridge.so
	cp _build/default/ocaml/gym/bridge.so python/gym/cpr_gym/bridge.so

update-bridge-from-ci:
	_venv/bin/python -m cpr_gym --update
	_venv/bin/python -m cpr_gym --version

# long-running simulations

simulate:
	mkdir -p data
	dune exec ocaml/experiments/honest_net.exe -- data/honest_net.tsv
	dune exec ocaml/experiments/withholding.exe -- data/withholding.tsv

expand: _venv
	_venv/bin/python python/eval/honest_net.py
	_venv/bin/python python/eval/withholding.py

# visualizations from short simulations

visualize:
	mkdir -p fig/chains/
	rm -rf fig/chains/*
	echo '*' > fig/chains/.gitignore
	dune exec ocaml/experiments/visualize.exe
	make -j $(shell nproc) visualize.render

.SECONDEXPANSION:
visualize.render: $$(patsubst %.dot, %.png, $$(wildcard fig/chains/*.dot))

%.png: %.dot
	dot -Tpng < $^ > $@

# RL

train-online: bridge _venv
	. _venv/bin/activate && python python/train/ppo.py

train-offline: export WANDB_MODE=offline
train-offline: _venv train-online
