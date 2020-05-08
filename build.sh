#!/usr/bin/env bash
set -e

make -C ~/blog/builder/build;
LSAN_OPTIONS=detect_leaks=0 ./builder/build/builder ~/blog/README.md ~/blog/index.html --latex2ascii
# LSAN_OPTIONS=detect_leaks=0  ./builder/build/builder ~/blog/fiction.md ~/blog/fiction.html --latex2ascii
