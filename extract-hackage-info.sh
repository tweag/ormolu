#!/usr/bin/env bash

set -e

WDIR=$(mktemp -d)
HOOGLE_DATABASE="$WDIR/hoogle-database/"
OUTPUT="$WDIR/hackage-info.bin"

trap cleanup 0 1 2 3 15

cleanup()
{
    rm -rf "$WDIR"; exit
}

mkdir "$HOOGLE_DATABASE"
curl "https://hackage.haskell.org/packages/hoogle.tar.gz" | tar -xz -C "$HOOGLE_DATABASE"

nix run .#extract-hackage-info -- generate "$HOOGLE_DATABASE" -o "$OUTPUT"

cp "$OUTPUT" "extract-hackage-info/hackage-info.bin"

cleanup
