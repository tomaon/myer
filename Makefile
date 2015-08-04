#
 ERLANG_HOME ?= /opt/erlang/release/latest

#
 REBAR ?= ../bin/rebar3

 REBAR_ENV  =
 REBAR_ENV += PATH=$(ERLANG_HOME)/bin:$(PATH)
#REBAR_ENV += DEBUG=1 

 REBAR_OPT  =
 REBAR_OPT += -sname $(1)@localhost
 REBAR_OPT += -config priv/conf/$(1)

#ERL_OPT += -setcookie test
#ERL_OPT += -s myer

#
default: compile

#
build:
	@$(REBAR_ENV) $(REBAR) as prod compile

ct compile:
	@$(REBAR_ENV) $(REBAR) as test $@

build_plt:
	@$(REBAR_ENV) $(REBAR) as test dialyzer
dialyzer:
	@$(REBAR_ENV) $(REBAR) as test dialyzer --update-plt

clean: clean-autosave
	@$(REBAR_ENV) $(REBAR) clean
clean-all: clean-autosave
	@$(REBAR_ENV) $(REBAR) clean --all 
clean-autosave:
	@-find . -name "*~" | xargs rm -f
distclean:
	@-rm -rf .rebar3 rebar.lock

#
n%: compile
	@$(REBAR_ENV) $(REBAR) as test shell $(call REBAR_OPT,$@)

x%: compile
	@$(ERBAR_ENV) ERL_LIBS=.rebar3/test/lib escript priv/escript/$@.escript
