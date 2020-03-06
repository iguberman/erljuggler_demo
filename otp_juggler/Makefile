BASEDIR = $(shell pwd)
REBAR = rebar3
BIN = _build/default/bin
LIB = _build/default/lib
APPNAME = otp_juggler
APPVSN = 1.0
RELPATH = _build/default/rel/$(APPNAME)
LOCALPATH = _build/local/rel/$(APPNAME)
TESTPATH = _build/test/rel/$(APPNAME)
PRODPATH = _build/prod/rel/$(APPNAME)
SHELL = /bin/bash
DEBUG=1
CT_CMD = $(shell ct_run -dir $(BASEDIR)/ct -logdir $(BASEDIR)/ct/logs \
                     	-pa $(BASEDIR)/_build/test/rel/$(APPNAME)/lib/*/ebin -erl_args \
                     	-config $(BASEDIR)/_build/test/rel/$(APPNAME)/releases/$(APPVSN)/sys.config)

compile:
	$(REBAR) compile

xref:
	$(REBAR) xref

recompile:
	find . -name ebin | xargs rm -rf
	$(REBAR) compile

unit-test: compile
	$(REBAR) eunit

testrel: compile
	$(REBAR) as test release

test: testrel
	$(CT_CMD)

local:
	$(BASEDIR)/$(LOCALPATH)/bin/$(APPNAME) start

localrel: compile
	$(REBAR) as local release

local-start: localrel
	$(BASEDIR)/$(LOCALPATH)/bin/$(APPNAME) start

local-stop:
	$(BASEDIR)/$(LOCALPATH)/bin/$(APPNAME) stop

local-console: localrel
	$(BASEDIR)/$(LOCALPATH)/bin/$(APPNAME) console

test-start: testrel
	$(BASEDIR)/$(TESTPATH)/bin/$(APPNAME) start

test-stop:
	$(BASEDIR)/$(TESTPATH)/bin/$(APPNAME) stop

test-console: testrel
	$(BASEDIR)/$(TESTPATH)/bin/$(APPNAME) console

ct_run:
	$(CT_CMD)

release:
	$(REBAR) release

prodrel: compile-with-metrics
	$(REBAR) as prod release


prod-start: prodrel
	$(BASEDIR)/$(PRODPATH)/bin/$(APPNAME) start

prod-stop:
	$(BASEDIR)/$(PRODPATH)/bin/$(APPNAME) stop

prod-console: prodrel
	$(BASEDIR)/$(PRODPATH)/bin/$(APPNAME) console

prod-tar: prodrel
	cd $(PRODRELPATH)/../ && rm -f $(APPNAME).tar.gz && tar -cvf $(APPNAME).tar $(APPNAME)/ && gzip $(APPNAME).tar
	mv $(PRODRELPATH)/../$(APPNAME).tar.gz $(BASEDIR)/_build/

clean:
	$(REBAR) clean
	rm -rf _build

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

console:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) console

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach
