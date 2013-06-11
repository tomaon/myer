#!/bin/bash --norc

 GCC_HOME=/usr

 CC=$GCC_HOME/bin/gcc-4.6
 CPP=$GCC_HOME/bin/cpp-4.6
 CXX=$GCC_HOME/bin/g++-4.6

 OPTS=
 OPTS="$OPTS --prefix=/home/mysql/release/5.1.70"
 OPTS="$OPTS --disable-dependency-tracking"
 OPTS="$OPTS --enable-local-infile"
 OPTS="$OPTS --with-plugins=max-no-ndb"
 OPTS="$OPTS --without-embedded-server"

CC=$CC CPP=$CPP CXX=$CXX ./configure $OPTS

exit 0
