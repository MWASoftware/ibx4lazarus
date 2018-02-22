This example application is provided to demonstrate the use of IBX for a range of Server Management tasks. These are:

1. Database backup and restore.
2. Viewing the Server Log
3. Viewing Server Statistics
4. User Management
5. Database Validation
6. Limbo Transaction Recovery.
7. Working with Alternative Security Databases in Firebird 3.

The purpose of the example is to demonstrate use of the IBX Services API rather than to provide a practical database administration tool. As regards database administration, using the Services API alone, has not kept pace with the way that Firebird has been developing – for example in the use of virtual tables to access system information. The Database Administration tool presented in ibx/examples/DBAdmin uses virtual database tables wherever possible and the Services API only when it has too. The result is arguably a much “slicker” tool. 

The example is discussed in detail in section 3 of the User Guide "Firebird Server Management using IBX" in the ibx/docs directory.
