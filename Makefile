ERL=erl
APP_NAME=erlycomet
MNESIA_DATA=mnesia-data
NODE_NAME=$(APP_NAME)
VSN=0.1

# The following properties are only required for creating and hosting a dojo xdomain build. 
#
APP_ROOT=~/opensource/$(APP_NAME)
XD_DOJO_PATH=http://rsaccon.googlepages.com
DOJO_ROOT=~/opensource/dojo-release-1.0.1-src

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

	
dojo:
	( cd $(DOJO_ROOT)/util/buildscripts && ./build.sh \
	loader=xdomain \
	xdDojoPath=$(XD_DOJO_PATH) \
	profile=cometd \
	releaseName=comet-xdomain-build \
	internStrings=true \
	copyTests=false \
	cssOptimize=comments \
	optimize=shrinksafe \
	layerOptimize=shrinksafe \
	action=release )
	
run:
	$(ERL) -pa `pwd`/ebin -pa `pwd`/priv/ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-sname $(NODE_NAME)
	
runx:
	$(ERL) -pa `pwd`/ebin -pa `pwd`/priv/ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-erlycomet http_port 3001 \
	-sname $(NODE_NAME)x
	