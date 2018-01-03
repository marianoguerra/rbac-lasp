BASEDIR = $(shell pwd)
REBAR = rebar3
RELPATH = _build/default/rel/rbacl
PRODRELPATH = _build/prod/rel/rbacl
APPNAME = rbacl
SHELL = /bin/bash

release:
	$(REBAR) release

console:
	cd $(RELPATH) && ./bin/rbacl console

prod-release:
	$(REBAR) as prod release
	mkdir -p $(PRODRELPATH)/../rbacl_config
	[ -f $(PRODRELPATH)/../rbacl_config/rbacl.conf ] || cp $(PRODRELPATH)/etc/rbacl.conf  $(PRODRELPATH)/../rbacl_config/rbacl.conf
	[ -f $(PRODRELPATH)/../rbacl_config/advanced.config ] || cp $(PRODRELPATH)/etc/advanced.config  $(PRODRELPATH)/../rbacl_config/advanced.config

prod-console:
	cd $(PRODRELPATH) && ./bin/rbacl console

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) ct

devrel1:
	$(REBAR) as dev1 release

devrel2:
	$(REBAR) as dev2 release

devrel3:
	$(REBAR) as dev3 release

devrel: devrel1 devrel2 devrel3

dev1-console:
	$(BASEDIR)/_build/dev1/rel/rbacl/bin/$(APPNAME) console

dev2-console:
	$(BASEDIR)/_build/dev2/rel/rbacl/bin/$(APPNAME) console

dev3-console:
	$(BASEDIR)/_build/dev3/rel/rbacl/bin/$(APPNAME) console

devrel-start:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/rbacl/bin/$(APPNAME) start; done

devrel-status:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/rbacl/bin/$(APPNAME)-admin cluster members; done

devrel-join:
	for d in $(BASEDIR)/_build/dev{2,3}; do $$d/rel/rbacl/bin/$(APPNAME)-admin cluster join rbacl1@127.0.0.1; done
	for d in $(BASEDIR)/_build/dev{1,3}; do $$d/rel/rbacl/bin/$(APPNAME)-admin cluster join rbacl2@127.0.0.1; done
	for d in $(BASEDIR)/_build/dev{1,2}; do $$d/rel/rbacl/bin/$(APPNAME)-admin cluster join rbacl3@127.0.0.1; done

devrel-ping:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/rbacl/bin/$(APPNAME) ping; done

devrel-stop:
	for d in $(BASEDIR)/_build/dev*; do $$d/rel/rbacl/bin/$(APPNAME) stop; done

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach

