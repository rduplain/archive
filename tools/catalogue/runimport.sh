#!/bin/bash
# Run Data Vault importer for GBT data.
# This script will look for all ScanLogs nested from current working directory.
#
# Optionally, pass a path glob argument to look only for ScanLogs beyond the
# given path(s).  e.g. runimport.sh AGBT07A*
# No arguments means look for all projects.

execdir=`dirname $0`
whichdir=`which $0`; whichdir=`dirname $whichdir`
sqlbase="runimport.sql"
sql_default="$execdir/$sqlbase"

catalogue_base="Catalogue"
scanlog_base="ScanLog.fits"

# Grab target from the command line.
targets="$@"

if [ -z "$targets" ]; then
    echo "assuming you everything in current directory..."
    targets=`ls -A`
fi

sqlpath="./$sqlbase"
if [ ! -e $sqlpath ]; then
    sqlpath=$execdir/$sqlbase
fi  

if [ ! -e $sqlpath ]; then
    sqlpath=$execdir/../lib/catalogue/$sqlbase
fi  

if [ ! -e $sqlpath ]; then
    sqlpath=$whichdir/$sqlbase
fi  


catalogue=$execdir/$catalogue_base
if [ ! -x $catalogue ]; then
    catalogue=$whichdir/$catalogue_base
fi  

if [ ! -x $catalogue ]; then
    catalogue=`which $catalogue_base`
fi  

echo "using $sqlbase at $sqlpath to run after $catalogue"

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

psql=`which psql`
if [ -z "$psql" ]; then
    psql=/usr/local/pgsql/bin/psql
fi

dbcmd="$psql -U dave vault"
if [ -e $sqlpath ]; then
    echo "$dbcmd < $sqlpath"
    $dbcmd < $sqlpath
else
    echo "manually run:"
    echo "$dbcmd < $sqlbase"
fi
