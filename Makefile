ERL=erl

# uncomment line below if you have installed Erlang from http://cean.process-one.net/download/
#
# ERL=~/path_to_your_cean/start.sh

APP_NAME=erlycomet_demo
MNESIA_DATA=mnesia-data
NODE_NAME=$(APP_NAME)
VSN=0.1

# APP_ROOT=~/opensource/$(APP_NAME)
# XD_DOJO_PATH=http://rsaccon.googlepages.com
# DOJO_ROOT=~/opensource/dojo-release-1.0.1-src


all:
	$(ERL) -make

doc:	
	$(ERL) -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application  "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'
	

# dojo-xd:
# 	( cd $(DOJO_ROOT)/util/buildscripts && ./build.sh \
# 	loader=xdomain \
# 	xdDojoPath=$(XD_DOJO_PATH) \
# 	profile=cometd \
# 	releaseName=comet-xdomain-build \
# 	internStrings=true \
# 	copyTests=false \
# 	cssOptimize=comments \
# 	optimize=shrinksafe \
# 	layerOptimize=shrinksafe \
# 	action=release )	
	


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
	-config erlycomet_demo.config \
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
