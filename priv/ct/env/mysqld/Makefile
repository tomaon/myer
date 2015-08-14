#
# fugo (https://bitbucket.org/tomaon/mysql/fugo)
#

# linux: /etc/security/limits.conf  -> ulimit -Hn / -Sn
#        <user> soft nofile 4096
#        <user> hard nofile 10240

 include bashrc

 ID ?= fugo

 MYSQL_HOME ?= $(HOME)/release/latest
 MYSQL_BASE ?= $(shell pwd)
 MYSQL_DATA ?= /var/tmp/fugo/$(ID)

 OPENSSL ?= /usr/bin/openssl

#
 MYSQL_OPT  = --defaults-file=$(MYSQL_BASE)/etc/my.$(ID).cnf

 MYSQLD_OPT  = $(MYSQL_OPT)
 MYSQLD_OPT += --log-bin

#
 MYSQL_VERSION = $(basename $(shell $(MYSQL_HOME)/bin/mysql_config --version))

 ifeq (,$(filter-out 5.5 5.6,$(MYSQL_VERSION)))
   MYSQL_INSTALL_DB = $(MYSQL_HOME)/scripts/mysql_install_db
   MYSQLD_OPT += --secure_auth=OFF
 else
   MYSQL_INSTALL_DB = $(MYSQL_HOME)/bin/mysql_install_db
 endif

#
default: test

#
start:
	@(cd $(MYSQL_HOME); bin/mysqld_safe $(MYSQLD_OPT))&
stop:
	@kill -0 $(shell cat $(MYSQL_DATA)/log/mysqld.pid)
	@kill $(shell cat $(MYSQL_DATA)/log/mysqld.pid)

#
root:
	@$(MYSQL_HOME)/bin/mysql $(MYSQL_OPT) -u $@ -p
test tpce:
	@$(MYSQL_HOME)/bin/mysql $(MYSQL_OPT) -u $@ -p $@

test_nopwd:
	@$(MYSQL_HOME)/bin/mysql $(MYSQL_OPT) -u $@ test
test_oldpwd:
	@$(MYSQL_HOME)/bin/mysql $(MYSQL_OPT) --default-auth=mysql_old_password -u $@ -p test
test_oldnopwd:
	@$(MYSQL_HOME)/bin/mysql $(MYSQL_OPT) --default-auth=mysql_old_password -u $@ test
test_sha256pwd:
	@$(MYSQL_HOME)/bin/mysql $(MYSQL_OPT) --default-auth=sha256_password -u $@ -p test

#
status:
	@$(MYSQL_HOME)/bin/mysqladmin $(MYSQL_OPT) proc stat
error.log general.log slow_query.log:
	@tail -f $(MYSQL_DATA)/log/$@
clean:
	@rm -f $(MYSQL_DATA)/log/*

#
create:
	@mkdir -p $(MYSQL_DATA)/data $(MYSQL_DATA)/log
	$(OPENSSL) genrsa -out $(MYSQL_DATA)/private_key.pem 1024
	chmod 400 $(MYSQL_DATA)/private_key.pem
	$(OPENSSL) rsa -in $(MYSQL_DATA)/private_key.pem -pubout > $(MYSQL_DATA)/public_key.pem
	chmod 444 $(MYSQL_DATA)/public_key.pem
	$(MYSQL_INSTALL_DB) $(MYSQL_OPT) --basedir=$(MYSQL_HOME)
init:
	$(MYSQL_HOME)/bin/mysqladmin $(MYSQL_OPT) -u root password 'fugo'
drop:
	@rm -rf $(MYSQL_DATA)
