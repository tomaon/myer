#!/bin/bash --norc

# @see http://dev.mysql.com/doc/refman/5.5/en/source-configuration-options.html

 GCC_HOME=/usr

 CC=$GCC_HOME/bin/gcc-4.6
 CPP=$GCC_HOME/bin/cpp-4.6
 CXX=$GCC_HOME/bin/g++-4.6

#CMAKE=/opt/tools/cmake/2.8.7/bin/cmake
 CMAKE=cmake

 OPTS=
 OPTS="$OPTS -DBUILD_CONFIG=mysql_release"
 OPTS="$OPTS -DCMAKE_INSTALL_PREFIX=/home/mysql/release/5.6.12"
#OPTS="$OPTS -DENABLE_DTRACE=ON"
#OPTS="$OPTS -DWITH_DEBUG=ON"
 OPTS="$OPTS -DWITH_EMBEDDED_SERVER=OFF"
 OPTS="$OPTS -DWITH_SSL=system"
#OPTS="$OPTS -DENABLE_DOWNLOADS=1"

rm -f CMakeCache.txt

# help -> "cmake . -LAH"
CC=$CC CPP=$CPP CXX=$CXX $CMAKE . $OPTS

exit 0
