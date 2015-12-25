LOAD_PATH = \
	-pa ebin \
	-pa deps/lager/ebin \
	-pa deps/goldrush/ebin \
	$(NULL)

NAME = janus
HOST = `hostname`
NODE = $(NAME)@$(HOST)

BASIC_OPTS = \
	$(LOAD_PATH) \
	+A 8 +K true +P 120000 -smp disable \
	$(NULL)

OPTS = \
	$(BASIC_OPTS) \
	$(NULL)

LOCAL_OPTS = \
	$(OPTS) \
	$(EXTRA_OPTS) \
	$(NULL)


all: compile

compile:
	erl -make -smp disable

make_boot: compile
	erl $(BASIC_OPTS) -s janus_admin make_boot -s init stop

run: compile
	erl $(LOCAL_OPTS) -name $(NODE) -s janus start

run2: compile
	erl $(LOCAL_OPTS) -name $(NODE) -boot janus

remsh:
	erl $(BASIC_OPTS) -sname remote -remsh $(NODE)

sh: compile 
	erl $(LOCAL_OPTS) -sname debug 

pas: compile 
	erl $(LOCAL_OPTS) -sname pas_test 

ns: compile 
	erl $(LOCAL_OPTS) -sname ns_test

clean:
	rm -rf ebin/*.beam
	rm -rf ebin/*.app

