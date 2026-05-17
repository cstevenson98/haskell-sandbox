#!/usr/bin/env bash
set -euo pipefail

cabal clean -v0
cabal build 2>&1 | grep -v "^Build profile\|^In order\|^Configuring\|^Preprocessing\|^Building\|^\[.*Compiling\|^\[.*Linking\|^Warning: The package"
