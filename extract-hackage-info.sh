#!/usr/bin/env bash

set -e

# The "important packages" whose latest sources are downloaded and parsed
# authoritatively (fixities + re-exports), complementing the Hoogle database.
# These are popular, widely-depended-on packages that also tend to re-export
# operators (through umbrella modules or by explicit name) in a way the Hoogle
# database no longer captures. Edit this list to change which packages get the
# treatment; extract-hackage-info parses whatever sources it is given.
IMPORTANT_PACKAGES=(
    # lens / optics families (heavy operator re-export via umbrella modules)
    lens
    microlens
    microlens-platform
    microlens-ghc
    microlens-mtl
    generic-lens
    optics
    optics-core
    optics-extra
    generic-optics
    # servant family (Servant / Servant.API facades re-export :> :<|> etc.)
    servant
    servant-server
    servant-client
    servant-client-core
    servant-auth
    servant-auth-server
    # prelude replacements / preludes (re-export enormous surfaces)
    relude
    rio
    protolude
    classy-prelude
    basic-prelude
    foundation
    universum
    base-prelude
    numeric-prelude
    base-compat
    base-compat-batteries
    # effect systems (operators + umbrella re-exports)
    polysemy
    polysemy-plugin
    fused-effects
    effectful
    effectful-core
    freer-simple
    # streaming
    conduit
    conduit-extra
    pipes
    streaming
    streamly
    streamly-core
    machines
    io-streams
    # parsing
    megaparsec
    attoparsec
    parsers
    trifecta
    parser-combinators
    replace-megaparsec
    Earley
    # web / servers / html
    yesod
    yesod-core
    yesod-form
    wai
    wai-extra
    warp
    scotty
    Spock
    lucid
    blaze-html
    blaze-markup
    shakespeare
    # databases / persistence
    persistent
    esqueleto
    beam-core
    opaleye
    hasql
    postgresql-simple
    selda
    # JSON / serialization / config
    aeson
    aeson-pretty
    yaml
    cassava
    binary
    cereal
    store
    # testing
    hspec
    hspec-expectations
    tasty
    hedgehog
    QuickCheck
    hspec-megaparsec
    hspec-wai
    # pretty printing / terminal
    prettyprinter
    ansi-wl-pprint
    pretty-simple
    brick
    vty
    # numeric / linear algebra / dimensions
    vector
    massiv
    hmatrix
    linear
    vector-space
    units
    diagrams-lib
    diagrams-core
    # concurrency / time / misc widely used
    stm
    async
    unliftio
    time
    text
    containers
    unordered-containers
    mtl
    transformers
    these
    semialign
    witherable
    profunctors
    bifunctors
    comonad
    free
    recursion-schemes
    algebraic-graphs
    generic-deriving
)

WDIR=$(mktemp -d)
HOOGLE_DATABASE="$WDIR/hoogle-database/"
PACKAGES_SOURCE="$WDIR/packages-source/"
OUTPUT="$WDIR/hackage-info.bin"

trap cleanup 0 1 2 3 15

cleanup()
{
    rm -rf "$WDIR"; exit
}

mkdir "$HOOGLE_DATABASE"
curl "https://hackage.haskell.org/packages/hoogle.tar.gz" | tar -xz -C "$HOOGLE_DATABASE"

# Download the latest source of each important package. We first resolve the
# preferred (latest normal) version, then fetch that versioned tarball. A
# package that cannot be resolved or downloaded is skipped (the parser is
# best-effort), so one bad entry does not abort the whole run.
mkdir "$PACKAGES_SOURCE"
for pkg in "${IMPORTANT_PACKAGES[@]}"; do
    version=$(curl -s -H "Accept: application/json" \
        "https://hackage.haskell.org/package/$pkg/preferred" \
        | python3 -c 'import sys, json; print(json.load(sys.stdin)["normal-version"][0])' 2>/dev/null)
    if [ -z "$version" ]; then
        echo "Skipping $pkg (could not resolve version)"
        continue
    fi
    echo "Downloading source of $pkg-$version..."
    curl -sL "https://hackage.haskell.org/package/$pkg-$version/$pkg-$version.tar.gz" \
        | tar -xz -C "$PACKAGES_SOURCE" || echo "Skipping $pkg (download/unpack failed)"
done

nix run .#extract-hackage-info -- generate "$HOOGLE_DATABASE" \
    --packages-source-path "$PACKAGES_SOURCE" \
    -o "$OUTPUT"

cp "$OUTPUT" "extract-hackage-info/hackage-info.bin"

cleanup
