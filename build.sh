#!/usr/bin/env bash
set -e
set -o xtrace
PWD=`pwd`
$(cd builder/build && ./builder ~/blog/README.md $PWD)
