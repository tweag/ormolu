#!/usr/bin/env bash
set -eo pipefail
[[ $BUILDKITE_BRANCH == "master" ]] && FLAGS="--prod" || true
netlify deploy $FLAGS \
  -d $(nix-build -A ormoluLive.website) \
  --alias=$(git rev-parse HEAD)
