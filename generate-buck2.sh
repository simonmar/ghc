#!/bin/sh
# Generate the buck2 build of GHC with `cabal buck2`, after ./configure: stage0
# (cabal.project) is built by the GHC that configure was given (--with-ghc),
# and stage1 (cabal.project.stage1) by the GHC that the build makes.
#
# Further arguments are passed to `cabal buck2` (--keep-going, say). To use a
# cabal-buck2 that isn't `cabal buck2` (a development build), set CABAL_BUCK2
# to its executable.

set -eu

cd "$(dirname "$0")"

config=hadrian/cfg/system.config
if [ ! -f "$config" ] || [ ! -f VERSION ]; then
    echo "$0: run ./configure first" >&2
    exit 1
fi

ghc=$(sed -n 's/^system-ghc *= *//p' "$config")
if [ -z "$ghc" ]; then
    echo "$0: $config has no system-ghc: re-run ./configure" >&2
    exit 1
fi

if [ -n "${CABAL_BUCK2:-}" ]; then
    set -- "$CABAL_BUCK2" buck2 "$@"
else
    set -- cabal buck2 "$@"
fi

exec "$@" \
    --with-compiler="$ghc" \
    --enable-custom-setup \
    --later-stage "stage1:$(cat VERSION):cabal.project.stage1"
