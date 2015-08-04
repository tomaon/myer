#
 ERLANG_HOME ?= /opt/erlang/release/latest

#
 REBAR ?= ../bin/rebar

 REBAR_ENV  =
 REBAR_ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
 REBAR_ENV += ERL_LIBS=deps:..

 REBAR_OPT  =
#REBAR_OPT += --verbose 3

#
 ERL_ENV  =
 ERL_ENV += ERL_LIBS=deps

 ERL_OPT  =
 ERL_OPT += -sname $(1)@localhost
 ERL_OPT += -setcookie test
 ERL_OPT += -config priv/conf/$(1)
#ERL_OPT += -s myer

 PLT = .dialyzer_plt.local

 DIALYZER_OPT  =
 DIALYZER_OPT += --no_native
 DIALYZER_OPT += --plts $(ERLANG_HOME)/.dialyzer_plt $(PLT)
 DIALYZER_OPT += --src src
 DIALYZER_OPT += -I deps

#
default: compile

#
delete-deps get-deps:
	@$(REBAR_ENV) $(REBAR) $(REBAR_OPT) $@

compile ct:
	@$(REBAR_ENV) $(REBAR) $(REBAR_OPT) $@ skip_deps=true


all: build

build: get-deps
	@$(REBAR_ENV) $(REBAR) $(REBAR_OPT) compile

build_plt:
	@$(ERLANG_HOME)/bin/dialyzer --$@ --output_plt $(PLT) --apps deps/*/ebin

clean: delete-autosave
	@$(REBAR_ENV) $(REBAR) $(REBAR_OPT) $@ skip_deps=true

delete-autosave:
	@-find . -name "*~" | xargs rm -f

dialyzer:
	@$(ERLANG_HOME)/bin/dialyzer $(DIALYZER_OPT)

distclean: clean delete-deps
	@-rm -rf deps $(PLT)

test: compile ct

#
n%: compile
	@$(ERL_ENV) $(ERLANG_HOME)/bin/erl $(call ERL_OPT,$@)

x%: compile
	@$(ERL_ENV) $(ERLANG_HOME)/bin/escript priv/escript/$@.escript
