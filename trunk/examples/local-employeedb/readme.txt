Local EmployeeDB Example
========================

The purpose of this example is to demonstrate the use of the TIBLocalDB support component.
This component is used with a TIBDatabase when the database is accessed using the Firebird
Embedded Server. TIBLocalDB takes care of checking the environment and setting up FIREBIRD
environment variables and DB parameters. It also supports initialisation of the local
database from an archive in gbak format, plus save and restore of the local database.
It can also run SQL scripts to upgrade the database schema when a new software version
is released.

Both GUI and Console Examples are provided.

Before compiling and running the example, the Firebird embedded server must be installed:

Under Linux:
------------

Debian/Ubuntu/Mint: run "sudo apt-get install libfbembed2.5" to install the server

Fedora/Red hat/Centos: su -c "yum install firebird-libfbembed"

Under Windows:
--------------

Download the Firebird Embedded Server from http://www.firebirdsql.org/en/firebird-2-5-5/ and
extract the contents of the archive into the example directory i.e. ibx\examples\local-employeedb.
You may replace the firebird.conf and firebird.msg files with those in the archive,
while not replacing or renaming the readme.txt that comes with the archive.

Running the application
-----------------------

The example should just compile and run. An archive of the Firebird example employee
database is provided with the example. This will be used to create the initial database.
It should then be automatically upgraded to "version 2" using the sql scripts provided in the
"patches" directory. (see also the file upgrade.conf). Note that the upgrade adds
a "photo" to employee number 2.

Note that you will not be prompted for a username/password. The embedded server
uses normal file permissions to control access. Otherwise you can edit the employee
database as in the client/server version.

The local database will be created in:

Linux: $HOME/.MWA Software/employee.fdb
Windows: <User Local Application Data Folder>\MWA Software\employee.fdb

The File menu provides actions to save the current database to a gbak format archive,
restore it again (replacing the current database) or to restore the database to its initial state.




