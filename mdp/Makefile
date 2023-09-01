now=$(shell date -Im)

all: measure-barzur.pkl

backup_%:
	mkdir -p backup/${now}
	if [ -e $* ] ; then mv $* backup/${now}/ ; fi

backup: backup_explored-models
backup: backup_measure-barzur.pkl

clear:
	rm -r explored-models
	rm measure-barzur.pkl

explored-models:
	mkdir $@
	python explore-models.py

measure-barzur.pkl: explored-models
	python measure-barzur.py
