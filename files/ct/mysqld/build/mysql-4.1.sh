#!/bin/bash --norc

 GCC_HOME=/usr

 CC=$GCC_HOME/bin/gcc-4.6
 CPP=$GCC_HOME/bin/cpp-4.6
 CXX=$GCC_HOME/bin/g++-4.6

 OPTS=
 OPTS="$OPTS --prefix=/home/mysql/release/4.1.25"
 OPTS="$OPTS --disable-dependency-tracking"
 OPTS="$OPTS --enable-local-infile"
 OPTS="$OPTS --without-embedded-server"
 OPTS="$OPTS --without-ndbcluster"

CC=$CC CPP=$CPP CXX=$CXX ./configure $OPTS

exit 0
