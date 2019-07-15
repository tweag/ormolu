set -euo pipefail

# drop includes
sed -i '/^#include/d' "$1"

# deal with CPP in a fairly straightforward way
cpphs "$1" --noline > "$1-nocpp" 2> /dev/null

# annoyingly, cpphs cannot modify files in place
mv "$1-nocpp" "$1"

# preserve the original
cp "$1" "$1-original"

# run ormolu
ormolu --tolerate-cpp --mode inplace "$1" || ormolu --tolerate-cpp --unsafe --mode inplace "$1"
