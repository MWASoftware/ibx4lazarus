#!/bin/sh
#This script may be used to build a clean version (with no debug symbols) of the
#fbsql application and to install it under /usr/local/bin
#
cd `dirname $0`
LAZDIR=../../../..
export LAZUTILS=$LAZDIR/components/lazutils
export FBINTFDIR=../../../fbintf
export FPCDIR=/usr/lib/fpc/`fpc -iV`

fpcmake
make clean
make
if [ -x fbsql ]; then
  sudo make install
  rm -r buildlib
fi


