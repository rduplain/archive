DataVault Lite -- simple web access to NRAO archive data.

Importing new GBT data

Put catalogue, runimport.sh, runimport.sql in the same directory on your PATH.
runimport.sh will look for all ScanLogs nested from current working directory.

Optionally, pass in a path glob argument to look only for ScanLogs beyond the
given path(s).  e.g. runimport.sh AGBT07A*
No arguments means look for all projects.

This work has been funded in part by the National Radio Astronomy
Observatory (NRAO). The National Radio Astronomy Observatory is a
facility of the National Science Foundation operated under cooperative
agreement by Associated Universities, Inc.
