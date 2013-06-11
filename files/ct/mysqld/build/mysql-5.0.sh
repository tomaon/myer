#!/bin/bash --norc

 GCC_HOME=/usr

 CC=$GCC_HOME/bin/gcc-4.6
 CPP=$GCC_HOME/bin/cpp-4.6
 CXX=$GCC_HOME/bin/g++-4.6

 OPTS=
 OPTS="$OPTS --prefix=/home/mysql/release/5.0.96"
 OPTS="$OPTS --disable-dependency-tracking"
 OPTS="$OPTS --disable-community-features"
#OPTS="$OPTS --disable-thread-safe-client"
 OPTS="$OPTS --enable-local-infile"
#OPTS="$OPTS --disable-largefile"
#OPTS="$OPTS --with-pic"
#OPTS="$OPTS --with-pthread"
#OPTS="$OPTS --with-zlib-dir=bundled"
#OPTS="$OPTS --with-ssl=/usr/local/Cellar/openssl/latest"
 OPTS="$OPTS --with-plugins=max-no-ndb"
 OPTS="$OPTS --without-embedded-server"
 OPTS="$OPTS --without-geometry"
#OPTS="$OPTS --with-extra-charsets=complex"

CC=$CC CPP=$CPP CXX=$CXX ./configure $OPTS

exit 0
