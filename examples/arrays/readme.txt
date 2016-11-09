For a more detail version of this text see docs/readme.firebirdarrays.html

Example applications are provided for both one and two dimensional arrays. In each case, 
the example applications create their own database and populate it with test data when 
first run. Note that you will typically need to run the application before accessing 
database properties in the IDE. This is in order to create the database referenced by the IDE.

Database Creation
-----------------

The IBDatabase property “CreateIfNotExists” is set to true in both examples. This 
means that if the database does not exist when an attempt is made to connect to 
it then the database is created. After it is created, the “OnCreateDatabase” 
event handler is used to add a table to the newly created database and to 
populate it with test data. The application then continues as if the database already existed.

By default, the database is created in the defined temporary directory. This 
behaviour can be overridden by editing the example's “unit1” unit to remove 
the “{$DEFINE LOCALDATABASE}” directive and setting the const “sDatabaseName” 
to the required path e.g.

const
  sDatabaseName = 'myserver:/databases/test.fdb';

1D Array Example
----------------

In this case, the test data table is defined as 

Create Table TestData (
  RowID Integer not null,
  Title VarChar(32) Character Set UTF8,
  MyArray Double Precision [1:12],
  Primary Key(RowID)
);

Each row includes a floating point array with twelve elements. In the 
example application, the table is displayed and edited using a DBControlGrid. 
The title field is interpreted as a “Department” and displayed using a 
TDBEdit control. The array field is interpreted as sales by month and 
displayed as a one dimensional TIBArrayGrid with column labels. The 
example allows both the Departname Name and monthly sales values 
to be updated and changes saved. New rows can be inserted and 
existing rows deleted.

Note: there is an LCL bug (http://bugs.freepascal.org/view.php?id=30892) which 
will cause the 1D array example to render incorrectly. That is only the 
focused row will show the array. The bug report includes an LCL 
patch to fix this problem.

2D Array Example
----------------

In this case, the test data table is defined as 

Create Table TestData (
  RowID Integer not null,
  Title VarChar(32) Character Set UTF8,
  MyArray VarChar(16) [0:16, -1:7] Character Set UTF8,
  Primary Key(RowID)
);

Each row includes a two dimensional string array with indices 0..16 and -1 to 7. 
The grid interprets the first index as a column index and the second as a row 
index (i.e.  x,y Cartesian co-ordinates).

The example program displays a row at a time with a navigation bar 
providing the means to scroll through the dataset, as well as saving 
or cancelling changes, inserting and deleting rows.

This example illustrates the use of both column and row labels.


