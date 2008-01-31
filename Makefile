ERL=erl

# uncomment line below if you have installed Erlang from http://cean.process-one.net/download/
#
# ERL=~/path_to_your_cean/start.sh

APP_NAME=erlycomet_demo
MNESIA_DATA=mnesia-data
NODE_NAME=$(APP_NAME)
VSN=0.1

all:
	$(ERL) -make

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
