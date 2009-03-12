#!/bin/bash
# Run Data Vault importer for GBT data.
# This script will look for all ScanLogs nested from current working directory.
#
# Optionally, pass a path glob argument to look only for ScanLogs beyond the
# given path(s).  e.g. runimport.sh AGBT07A*
# No arguments means look for all projects.

execdir=`dirname $0`
sqlbase="runimport.sql"
sql_default="$execdir/../lib/catalogue/$sqlbase"

catalogue="Catalogue"
scanlog_base="ScanLog.fits"

# Grab target from the command line.
targets="$@"

if [ -z "$targets" ]; then
    targets=`ls -A`
fi

sqlpath="./$sqlbase"
if [ ! -e $sqlpath ]; then
    sqlpath=$execdir/../lib/catalogue/$sqlbase
fi  

echo "using $sqlbase at $sqlpath"

for target in $targets; do
    for scanlog_found in `find $target -name $scanlog_base`; do 
        oldpwd=`pwd`
        project_dir=`dirname $scanlog_found`
        newpwd=`dirname $project_dir`
        project_dir=`basename $project_dir`
        if [ "$project_dir" = "." ]; then
            newpwd=".."
        fi
        cd $newpwd

        scanlog_path="$project_dir/$scanlog_base"
        echo "running $catalogue on $scanlog_path"
        $catalogue $scanlog_path;
        cd $oldpwd
    done
done

echo "running mysql with $sqlpath"
mysql -u powerdave -p'Green$Bank' -D vault < $sqlpath
