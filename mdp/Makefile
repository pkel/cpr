now=$(shell date -Im)

# transition limit
T=1000000

all: measure-barzur.pkl measure-ours.pkl

backup_%:
	mkdir -p backup/${now}
	if [ -e $* ] ; then mv $* backup/${now}/ ; fi

backup: backup_explored-models
backup: backup_measure-barzur.pkl
backup: backup_measure-ours.pkl

clear:
	rm -rf explored-models
	rm -f measure-barzur.pkl
	rm -f measure-ours.pkl

explored-models:
	mkdir $@
	python explore-models.py -t $(T)

measure-barzur.pkl: explored-models
	python measure-barzur.py

measure-ours.pkl: explored-models
	python measure-ours.py
