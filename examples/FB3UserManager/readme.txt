Firebird 3 User Manager
=======================

This example is intended to illustrate the use of IBX for managing users with both a single and multiple security databases.

The example uses queries on the MON$DATABASE, SEC$USERS, MON$ATTACHMENTS, RDB$ROLES and RDB$PRIVILEGE tables to display the list of current users, their login status and assigned roles. If the current database uses the global security database (default) then the SEC$USERS table is sourced from the global Security Database. If the database uses an alternative security database, then the  the SEC$USERS table is sourced from that security database.

The TIBUpdate component is used to "post" changes to the user details by generating and executing CREATE/ALTER/DROP user statements.

The TIBUpdate component is also used to "post" changes to the roles granted to users by generating and executing GRANT/REVOKE statements.

When running the example, you can enter any database connect string to local or remote databases together with suitable login credentials.

Use the right click popup menu to:

* Add a new user
* Change a user password
* Delete a user.
* Drop a database attachment (other than that used for user management)
