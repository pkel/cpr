now=$(shell date -Im)

all: measure-barzur.pkl measure-ours.pkl

backup_%:
	mkdir -p backup/${now}
	if [ -e $* ] ; then mv $* backup/${now}/ ; fi

backup: backup_explored-models
backup: backup_measure-barzur.pkl
backup: backup_measure-ours.pkl

clear:
	rm -r explored-models
	rm measure-barzur.pkl
	rm measure-ours.pkl

explored-models:
	mkdir $@
	python explore-models.py

measure-barzur.pkl: explored-models
	python measure-barzur.py

measure-ours.pkl: explored-models
	python measure-ours.py
