Example: fbsql
==============

fbsql is more than just a simple example and is an ISQL replacement console mode program for
both interactive and non-interactive use. fbsql uses TIBXScript as its SQL Script Engine and
TIBExtract to extract metadata from the database. Select queries are handled by by outputing the
query results to stdout in CSV format suitable for loading into a spreadsheet, as insert statements,
or in a block format.

It can be used to dump an entire database in text (SQL) format including metadata and user
data, with binary blobs and array data encoded in a simple XML format. It can also be used
to create a datbase from an SQL dump. This format is less compact that gbak but allows
for metadata editing, simplifying complex metadata changes where there are many nested
dependencies to resolve.

Usage: fbsql <options> <database name>
Options:
-a            write database metadata to stdout
-A            write database metadata and table data to stdout
-b            stop on first error
-e            echo sql statements to stdout
-h            show this information
-i <filename> execute SQL script from file
-o <filename> output to this file instead of stdout
-p <password> provide password on command line (insecure)
-r <rolename> open database with this rolename
-s <sql>      Execute SQL text
-u <username> open database with this username (defaults to SYSDBA)

Environment Variables:
ISC_USER      Login user Name
ISC_PASSWORD  Login password

Saving the username and/or password as environment variables avoids having to enter
them on the command line and is a more secure means of provding the password.

If no password is provided on the command line or through the environment, then the
user is prompted for a password to be entered securely.

If neither an "-s" or a "-i" option is provided on the command line, then fbsql runs
interactively.

fbsql uses IBX in console mode. Before opening this project you should tell the Lazarus
IDE about the ibexpressconsolemode package. All you need to do in the IDE is to select "Packages->Open Package File"
and open ibexpressconsolemode.lpk which you can find in the ibx root directory. You should then close it again 
immediately afterwards. There is no need to install or compile it. Opening the package is sufficient for Lazarus to
remember it.

SQL Statements Supported
========================

All DML and DDL Statements are supported.

CREATE DATABASE, DROP DATABASE, CONNECT and COMMIT are supported.

Additionally, RECONNECT is interpreted as dropping the connection and reconnecting.

ISQL Command Support
====================

SET SQL DIALECT
SET TERM
SET AUTODDL
SET BAIL
SET ECHO
SET COUNT
SET STATS
SET NAMES <character set>
SET HEADING
SET ROWCOUNT
SET PLAN
SET PLAN ONLY
QUIT
EXIT

Examples
========

To use, compile the program in the Lazarus IDE and run it from the command line. The
above gives the command line parameters. For example:

fbsql -a -u SYSDBA -p masterkey employee

will write out the metadata for the local employee database to stdout (assuming
default password).

fbsql -A -u SYSDBA -p masterkey -o employeedump.sql employee

will dump the employee database to a text file (employeedump.sql).

fbsql -u SYSDBA -p masterkey -i employeedump.sql

will recreate the database dumped in the file "employeedump.sql". Note that the "CREATE
DATABASE" statement is at the start of this file and should be edited to identify
the database file that is to be created. Alternatively,

fbsql -u SYSDBA -p masterkey -i employeedump.sql  new-employee.fdb

will restore the database to the database file 'new-employee.fdb' provided that it
has already been created as an empty database. Note that in this case, the "CREATE
DATABASE" statement should remain commented out.

fbsql -s "Select * From EMPLOYEE" -u SYSDBA -p masterkey employee

will write out the contents of the EMPLOYEE table in the local employee database to stdout (assuming
default password).

fbsql -b -e ../scriptengine/tests/CreateCountriesTable.sql -u SYSDBA -p masterkey employee

will run the script CreateCountriesTable.sql from the script engine test suite and apply
it to the local employee database. Each statement will be echoed to stdout and
processing will stop on the first error.

Note that on Linux, to run a program from the command line that is not on the PATH,
you need to:

cd to the example directory "ibx/examples/fbsql"
run the program as "./fbsql" e.g.

./fbsql -a -u SYSDBA -p masterkey employee


