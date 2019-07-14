#!/usr/bin/env bash

set -euo pipefail

# drop includes
sed -i '/^#include/d' "$1"

# drop CPP
sed -i '/^{-# LANGUAGE CPP/d' "$1"

# deal with CPP in a fairly straightforward way
cpphs "$1" --noline > "$1-nocpp" 2> /dev/null

# annoyingly, cpphs cannot modify files in place
mv "$1-nocpp" "$1"

# preserve the original
cp "$1" "$1-original"

# run ormolu
ormolu -m inplace "$1" || ormolu -u -m inplace "$1"
