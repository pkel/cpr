now=$(shell date -Im)

# transition limit
T=1000000

all: measure-validation.pkl measure-ours.pkl

backup_%:
	mkdir -p backup/${now}
	if [ -e $* ] ; then mv $* backup/${now}/ ; fi

backup: backup_explored-models
backup: backup_measure-validation.pkl
backup: backup_measure-ours.pkl

clear:
	rm -rf explored-models
	rm -f measure-validation.pkl
	rm -f measure-ours.pkl

explored-models:
	mkdir $@
	python explore-models.py -t $(T)

measure-validation.pkl: explored-models
	python measure-validation.py

measure-ours.pkl: explored-models
	python measure-ours.py
