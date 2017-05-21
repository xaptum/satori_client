BASEDIR = $(shell pwd)
REBAR = rebar3
APPNAME = satori_client
RELPATH = _build/default/rel/$(APPNAME)
BUILDDIR = _build
APPVSN = 1.0
SHELL = /bin/bash
PRIVDIR = $(BASEDIR)/priv
IDF = $(PRIVDIR)/build_id
ID_CMD = echo "`date +'%y-%m-%d %H:%M:%S'`" > $(IDF)
DEBUG=1
JSX_FORCE_MAPS = true

compile:
	$(REBAR) compile

recompile:
	find . -name ebin | xargs rm -rf
	rm -f ct/*.beam
	$(REBAR) compile

ct-run:
	ct_run -dir $(BASEDIR)/ct -logdir /var/log/satori/$(APPNAME)/ct/logs \
	-pa $(BASEDIR)/_build/test/rel/$(APPNAME)/lib/*/ebin -erl_args \
	-config $(BASEDIR)/_build/test/rel/$(APPNAME)/releases/$(APPVSN)/sys.config

release: id
	$(REBAR) release

prod-release: id
	$(REBAR) as prod release

test-release: release
	$(REBAR) as test release

test: recompile test-release ct-run

privdir:
	mkdir -p $(PRIVDIR)

id: privdir
	$(ID_CMD)

console: release
	cd $(RELPATH) && ./bin/$(APPNAME) console

start: release
	cd $(RELPATH) && ./bin/$(APPNAME) start

dialyzer: test
	$(REBAR) dialyzer

clean:
	$(REBAR) clean
	rm -rf _build

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach
