#!/usr/bin/env bash
#
# Format source using:
#   - Ormolu for Haskell,
#   - cabal-fmt for cabal files.

ormolu --mode inplace $(find . -name '*.hs')
cabal-fmt --inplace $(find . -name '*.cabal')