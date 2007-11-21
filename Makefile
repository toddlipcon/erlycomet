ERL=erl
APP_NAME=erlycomet
MNESIA_DATA=mnesia-data
NODE_NAME=erlycomet
VSN=0.1

all:
	( $(ERL) -make && \
	if [ ! -e ebin/$(APP_NAME).app ]; then cp -f src/demo/$(APP_NAME).app.src ebin/$(APP_NAME).app; fi )

doc:	
	$(ERL) -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application  "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

run:
	$(ERL) -pa `pwd`/ebin -pa `pwd`/priv/ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-sname $(NODE_NAME)