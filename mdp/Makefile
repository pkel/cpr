explored-models: explore-models.py
	if [ -e $@ ] ; then mkdir -p $@.bak/$(shell date -Im) ; mv $@ $@.bak/$(shell date -Im) ; fi
	mkdir $@
	python explore-models.py

touch_%:
	touch $*

force-explored-models: touch_explore-models.py explored-models
