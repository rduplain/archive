#!/bin/bash

# certainly we can do this without awk. :-)
count=`wc -l hosts.txt | awk '{ print $1 }'`

cat hosts.txt | xargs -L 1 -P $count ./rcommands.sh
