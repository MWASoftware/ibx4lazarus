IBX Test Suite
==============

This is the IBX Test Suite. It is intended to provide a set of tests with a pass/fail result determined by a simple inspection of the "diff" file.

The test suite comprises 28 separate tests, testing 230 identified features present in the IBX non-visual components. It is intended to provide comprehensive test coverage of all test features in order to demonstrate a high degreee of confidence that the software performs as expected and when run on different platforms (32 bit and 64 bit, Linux and Windows) and with different versions of the Firebird database (2.5, 3 and 4). It also supports regression testing. See doc/TestCoverage.pdf for a list of tests and test features and which test covers which test features.

The test environment requires that a Firebird Server is available and running, and ideally this should also include the Firebird embedded server. The example "employee" database must also be installed. The standard Firebird distributions available from firebirdsql.org are suitable for testing. In order to test the embedded server operation, the "employee" database should be read/writable from the user under which the tests are run.

There is also a script "dotest.sh" which is used to support test environments where multiple versions of the Firebird Server are available. See below.

-----------------------------------------------------------------------

Under Linux, the test suite is run from the ibx/testsuite directory using the script "runtest.sh":

Usage:

runtest.sh [-L <lazarus root directory> ] -f <fbintf package directory> [-p <SYSDBA password] [-t <testid>]

Options:

-L	This is used when the lazarus root directory for the installed version of lazarus differs from ~/lazarus. The option is used to introduce the relative or absolute path to the lazarus installation directory.

-f  This is used when the relative path to the fbintf package directory (from ibx/testsuite) is neither ../fbintf or ../../fbintf. The option is used to introduce the relative or absolute path to the fbintf directory.

-p	This is used when the SYSDBA password is not the default "masterkey". The password can also be provided by the environment variable SYSDBAPWD.

-t	This is used to select a specific test (in the range 1..28). If absent then all tests are performed.

The script compiles the IBX units and test suite and runs the testsuite. The result is saved in the file

ibx/testsuite/testout.<current time in nanoseconds>.log

It is compared with a reference log file and the result output to diff.log. The script prints the number of lines in the output diff.

Separate reference logs are provided for Firebird 2.5, 3 and 4. This reflects the different feature sets of each release. The diff.log needs to be reviewed in order to determine pass/fail. Some differences should be expected e.g. some tests include current timestamps and some print out server statistics. Both vary every time the tests are run. Generally, 200 to 500 lines should be expected for a pass result. More is indicative of failure. Review of the diff should be diagnostic as the logs should only differ in dynamic information. 

The script can also work with the Firebird Server and Client files in a non-standard directory if the FIREBIRD environment variable is set before the script is invoked. For example, if a test version of Firebird is installed in /home/tester/firebird and started locally, then the script may be run as:

export FIREBIRD=/home/tester/firebird
./runtest.sh

-----------------------------------------------------------------------

Under Windows, the test suite is run from the ibx\testsuite directory using the script "runtest.bat". This script does not have any command line arguments.

The script assumes that lazarus is installed in C:\lazarus and includes the FPC compiler and binary tools. If lazarus is installed elsewhere then the script must be edited and the LAZARUS variable updated. e.g.#!/bin/sh

cd /opt
for FB in `ls -d firebirdVersions/*`; do
  if [ -z "`ps ax|grep /opt/$FB/bin/fb_smp_server|grep -v grep`" ] && [ -z "`ps ax|grep /opt/$FB/bin/firebird|grep -v grep`" ]; then
    echo "starting $FB/bin/fbguard"
    export FIREBIRD=/opt/$FB
    export LD_LIBRARY_PATH=/opt/$FB/lib
    $FB/bin/fbguard&
  else
    echo "$FB not started"
  fi
done


set LAZARUS=D:\lazarus

Similarly, if the SYSDBA password is other than the default "masterkey" the script must be edited and the PASSWORD variable changed to set the actual SYSDBA password.

Otherwise, the script works similarly to the linux version except that the results are always output to "testout.log".

Multiple Firebird Servers
=========================

The script dotest.sh is made available as an example of how testing with multiple Firebird servers and multiple versions of the FPC compiler can be scripted.

The general idea is that the directory /opt/firebirdVersions contains one or more subdirectories each of which contain a copy of the Firebird installation. These subdirectories are given names that reflect the release e.g. 4.0.4 and the files within them are just a copy of the files distributed in a Firebird installation binary (but with no attempt to run the installation script. The main role of the dotest.sh script is to set up the FIREBIRD environment variable and to invoke "runtest.sh". It can also select the appropriate FPC compiler.

Each Firebird installation is configured by:

1. Editing firebird.conf to set the server port to some obscure but unique port no.

2. The employee database is decompressed #!/bin/sh

cd /opt
for FB in `ls -d firebirdVersions/*`; do
  if [ -z "`ps ax|grep /opt/$FB/bin/fb_smp_server|grep -v grep`" ] && [ -z "`ps ax|grep /opt/$FB/bin/firebird|grep -v grep`" ]; then
    echo "starting $FB/bin/fbguard"
    export FIREBIRD=/opt/$FB
    export LD_LIBRARY_PATH=/opt/$FB/lib
    $FB/bin/fbguard&
  else
    echo "$FB not started"
  fi
done
if necessary and otherwise made available for use.

3. The SYSDBA password is enabled and set to (e.g.) masterkey by

export FIREBIRD=/opt/firebirdVersions/4.0.4
export LD_LIBRARY_PATH=$FIREBIRD/lib
$FIREBIRD/bin/isql -user SYSDBA employee
CREATE USER SYSDBA PASSWORD 'masterkey';
exit;

The servers can then be started by a simple script such as:
#!/bin/sh

cd /opt
for FB in `ls -d firebirdVersions/*`; do
  if [ -z "`ps ax|grep /opt/$FB/bin/fb_smp_server|grep -v grep`" ] && [ -z "`ps ax|grep /opt/$FB/bin/firebird|grep -v grep`" ]; then
    echo "starting $FB/bin/fbguard"
    export FIREBIRD=/opt/$FB
    export LD_LIBRARY_PATH=/opt/$FB/lib
    $FB/bin/fbguard&
  else
    echo "$FB not started"
  fi
done

and dotest.sh should then be able to do its magic.





