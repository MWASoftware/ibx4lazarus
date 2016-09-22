This example demonstrates IBX in console mode. Before opening this project you should tell the Lazarus
IDE about the ibexpressconsolemode package. All you need to do in the IDE is to select "Packages->Open Package File"
and open ibexpressconsolemode.lpk which you can find in the ibx root directory. You should then close it again 
immediately afterwards. There is no need to install or compile it. Opening the package is sufficient for Lazarus to
remember it.

Now you can open the project, compile and run it. The program uses the default Firebird "employee" database. If
this is available on the localhost and you have not changed the default SYSDBA password from "masterkey" then
the program should run "out of the box". If the Firebird server is on another host, or you have change the SYSDBA password
then goto to line 117 of the project1.lpr file and update the databasename, user_name or password as appropriate. 
Note that with a console mode program, there is no dialog that can be used to enter these and either these
parameters have to be set from the command line (left as an exercise for the student) or given explicitly in the
source code.

Under Linux: to see the output select View->Debug Windows->Terminal Output to open the stdout viewer.

Under Windows: the stdout view opens automatically but will also close automatically as soon as  the program
finishes. Set a break point to keep this window open.
