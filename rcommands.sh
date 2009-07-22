#!/bin/bash

target_host=$1

[[ -n "$target_host" ]] || exit 1

cat commands.sh | ssh $target_host > $target_host.log
