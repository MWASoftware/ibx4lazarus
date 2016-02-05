Example: fbsql
==============

fbsql is more than just a simple example and is an ISQL replacement console mode program for
non-interactive use. fbsql uses TIBXScript as its SQL Script Engine and TIBExtract to
extract metadata from the database. Select queries are handled by by outputing the
query results to stdout in CSV format suitable for loading into a spreadsheet.

Usage: fbsql <options> <database name>
Options:
-a            write database metadata to stdout
-b            stop on first error
-e            echo sql statements to stdout
-f <filename> execute SQL script from file
-h            show this information
-p <password> provide password on command line (insecure)
-r <rolename> open database with this rolename
-s <sql>      Execute SQL text
-u <username> open database with this username (defaults to SYSDBA)

Environment Variables:
ISC_USER      Login user Name
ISC_PASSWORD  Login password

Saving the username and/or password as environment variables avoids having to enter
them on the command line and is a more secure means of provding the password.

To use, compile the program in the Lazarus IDE and run it from the command line. The
above gives the command line parameters. For example:

fbsql -a -u SYSDBA -p masterkey employee

will write out the metadata for the local employee database to stdout (assuming
default password).

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


