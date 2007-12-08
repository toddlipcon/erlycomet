ERL=erl
APP_NAME=erlycomet_demo
MNESIA_DATA=mnesia-data
NODE_NAME=$(APP_NAME)
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

run:	all
	$(ERL) -pa `pwd`/ebin -pa `pwd`/priv/ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-sname $(NODE_NAME)

# Starts a node at a different port (3001).
# You can load demo page from this server to do comet x-domain access to default server at port 3000.
#
runx:	all
	$(ERL) -pa `pwd`/ebin -pa `pwd`/priv/ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-erlycomet_demo http_port 3001 \
	-sname $(NODE_NAME)x
