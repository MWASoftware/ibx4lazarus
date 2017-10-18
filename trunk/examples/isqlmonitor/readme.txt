There are two simple example applications used here to show the power of TISQLMontor.

IntegratedMonitoring
====================

This is a minor change to the Employee example and adds a second "Monitor Form" 
to record a selected set of SQL events in a TMemo journal. The events monitored can 
be changed by changing the trace options in the TIBDatabase.


Remote Monitoring
=================

This example show how TISQLMonitor can be used to monitor another application. 
This application comprises one form containing a single TMemo used to record the 
SQL event journal. Run it at the same time as IntegratedMonitoring and you will 
see the SQL event journal here as well. Note that what is monitored is controlled 
from the IntegratedMonitoring application which sets the monitored events 
in its trace flags and must call "EnableMonitoring" for any monitoring to 
take place.
