#!/bin/sh

#Test suite Configuration parameters
#These may be modified if needed to suite local requirements

TESTOUTDIR=/tmp/ibx-testsuite
USERNAME=SYSDBA
PASSWORD=masterkey
EMPLOYEEDB=employee
NEWDBNAME=$TESTOUTDIR/testsuite1.fdb
NEWDBNAME2=$TESTOUTDIR/testsuite2.fdb
BAKFILE=$TESTOUTDIR/testsuite.gbk
LOGFILE=testout.`date +%N`.log

if [ -d "../fbintf" ]; then
  export FBINTF="../fbintf"
elif [ -d "../../fbintf" ]; then
  export FBINTF="../../fbintf"
else
  echo "Error: unable to locate Pascal Firebird Interface API"
  exit 2
fi

LAZARUS=$HOME/lazarus
INCDIR="$FBINTF/client/3.0/firebird $FBINTF/client/include"
UNITDIR="$FBINTF $FBINTF/client $FBINTF/client/3.0/firebird $FBINTF/client/2.5 $FBINTF/client/3.0  $LAZARUS/components/lazutils"

cd `dirname $0`
mkdir -p $TESTOUTDIR
chmod 777 $TESTOUTDIR
export FPCDIR=/usr/lib/fpc/`fpc -iV`
fpcmake
make clean
make INCDIR="$INCDIR" UNITDIR="$UNITDIR"
if [ -x testsuite ]; then
  if [ -n "$FIREBIRD" ]; then
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$FIREBIRD/lib
  fi
  echo ""
  echo "Starting Testsuite"
  echo ""
  ./testsuite -u $USERNAME -p $PASSWORD -e $EMPLOYEEDB -n $NEWDBNAME -s $NEWDBNAME2 -b $BAKFILE -o $LOGFILE $@
  echo "Comparing results with reference log"
  echo ""
  if grep 'ODS Major Version = 11' $LOGFILE >/dev/null; then
    diff FB2reference.log $LOGFILE >diff.log
  elif grep 'ODS Major Version = 12' $LOGFILE >/dev/null; then
    diff FB3reference.log $LOGFILE >diff.log
  else
    diff FB4reference.log $LOGFILE >diff.log
  fi
 # cat diff.log
  echo "`cat diff.log|wc -l` lines in diff"
else
  echo "Unable to run test suite"
fi
echo "Log File is $LOGFILE"
rm -r testunits
rm testsuite
exit 0

