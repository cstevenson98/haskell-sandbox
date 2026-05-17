#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

cabal build exe:haskell-test
ln -sf "$(cabal list-bin haskell-test)" ./haskell-test
echo "./haskell-test -> $(readlink -f ./haskell-test)"
