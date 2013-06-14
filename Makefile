#
 ERLANG_HOME ?= /opt/erlang/release/latest

#
 REBAR_BIN  = ./rebar

 REBAR_ENV  =
 REBAR_ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
 REBAR_ENV += ERL_LIB=..

 REBAR_OPT  =
#REBAR_OPT += --verbose 3

#
 ERL_OPT  =
 ERL_OPT += -pa ebin deps/*/ebin

 PLT = .dialyzer_plt.local

 DIALYZER_OPT  =
 DIALYZER_OPT += --no_native
 DIALYZER_OPT += --plts $(ERLANG_HOME)/.dialyzer_plt $(PLT)
 DIALYZER_OPT += --src src
 DIALYZER_OPT += -I ..

#
all: compile

delete-deps get-deps:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@

clean compile ct:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) $@ skip_deps=true

build: get-deps
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) compile

cleanall:
	@$(REBAR_ENV) $(REBAR_BIN) $(REBAR_OPT) clean

build_plt:
	@$(ERLANG_HOME)/bin/dialyzer --$@ --output_plt $(PLT) --apps deps/*/ebin

dialyzer:
	@$(ERLANG_HOME)/bin/dialyzer $(DIALYZER_OPT)

shell:
	@$(ERLANG_HOME)/bin/erl $(ERL_OPT) -config files/$@

test: compile ct

distclean: clean delete-deps
	@-rm -r deps

#
cmp: compile
	@ERL_FLAGS="" $(ERLANG_HOME)/bin/escript escript/$@.escript
