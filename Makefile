#
 ERLANG_HOME ?= /opt/erlang/release/latest

 REBAR ?= ../bin/rebar3

 ENV  =
 ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
#ENV += DEBUG=1

 OPT  =
 OPT += --sname $(1)@localhost
 OPT += --config priv/conf/$(1)

#
default: compile

all: build

test: ct

#
build:
	@$(ENV) $(REBAR) as prod compile

compile ct dialyzer eunit:
	@$(ENV) $(REBAR) as test $@

clean: rm
	@for P in prod test; do $(ENV) $(REBAR) as $$P clean; done # prod,test -> prod+test ?!, TODO
cleanall: rm
	@for P in prod test; do $(ENV) $(REBAR) as $$P clean --all; done
distclean:
	@-rm -rf .rebar3 rebar.lock

rm: rm-autosave rm-dump rm-logs

rm-autosave:
	@-find . -name "*~" | xargs rm -f
rm-dump:
	@-rm -f erl_crash.dump
rm-logs:
	@for D in cover logs; do rm -rf .rebar3/test/$$D; done

#
n%: compile
	@$(ENV) $(REBAR) as test shell $(call OPT,$@)

x%: compile
	@$(ENV) ERL_LIBS=.rebar3/test/lib escript priv/escript/$@.escript
