#!/usr/bin/env bash

set -e

WDIR=$(mktemp -d)
HOOGLE_DATABASE="$WDIR/hoogle-database/"
HACKAGE_DATABASE="$WDIR/hackage-database.html"
OUTPUT="$WDIR/hackage-info.json"

trap cleanup 0 1 2 3 15

cleanup()
{
    rm -rf "$WDIR"; exit
}

EXTRACTION_APP="$(nix-build -A extractHackageInfo --no-out-link)/bin/extract-hackage-info"

mkdir "$HOOGLE_DATABASE"
curl "https://hackage.haskell.org/packages/hoogle.tar.gz" | tar -xz -C "$HOOGLE_DATABASE"
curl "https://hackage.haskell.org/packages/top" -o "$HACKAGE_DATABASE"

"$EXTRACTION_APP" "$HOOGLE_DATABASE" "$HACKAGE_DATABASE" -o "$OUTPUT"

cp "$OUTPUT" "extract-hackage-info/hackage-info.json"

cleanup
