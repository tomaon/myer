#
# node: simple
#
 DRIZZLE_HOME = /opt/drizzle/release/7.1.36
 DRIZZLE_BASE = $(shell pwd)

 DRIZZLED_OPT  =
#DRIZZLED_OPT += --defaults-file=$(DRIZZLE_BASE)/etc/drizzled.conf
 DRIZZLED_OPT += --datadir $(DRIZZLE_BASE)/data
 DRIZZLED_OPT += --pid-file $(DRIZZLE_BASE)/log/dizzled.pid
 DRIZZLED_OPT += --plugin-remove=auth_all
 DRIZZLED_OPT += --plugin-add=auth_schema
 DRIZZLED_OPT += --auth-schema.table=auth.users
 DRIZZLED_OPT += --drizzle-protocol.max-connections=3
 DRIZZLED_OPT += --drizzle-protocol.port=4427
 DRIZZLED_OPT += --mysql-protocol.max-connections=3
 DRIZZLED_OPT += --mysql-protocol.port=3306

 DRIZZLE_OPT  =
 DRIZZLE_OPT += --port=4427

#
default: test

#
start:
	$(ENV) $(DRIZZLE_HOME)/sbin/drizzled $(DRIZZLED_OPT) >> log/error.log 2>&1 &
stop:
#	$(ENV) $(DRIZZLE_HOME)/bin/drizzle $(DRIZZLE_OPT) --shutdown
	$(ENV) $(DRIZZLE_HOME)/bin/drizzle $(DRIZZLE_OPT) --user root --password --shutdown

#
root:
	$(ENV) $(DRIZZLE_HOME)/bin/drizzle $(DRIZZLE_OPT) --user $@ --password
test:
	$(ENV) $(DRIZZLE_HOME)/bin/drizzle $(DRIZZLE_OPT) --user $@ --password --database $@

#
clean:
	@rm -f log/*
error.log:
	@tail -f log/$@

#
create:
	@mkdir data log
drop:
	@rm -rf data log
