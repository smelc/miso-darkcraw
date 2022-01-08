#!/usr/bin/env bash

GHC_VERSION=$(nix-shell --run 'ghc --numeric-version')

EXEC="dist-newstyle/build/x86_64-linux/ghc-$GHC_VERSION/app-0.1.0.0/x/judge/build/judge/judge"

nix-shell --run 'cabal build judge' || { echo "nix-shell --run 'cabal build judge' failed. Cannot proceed."; exit 1; }

[[ -e "$EXEC" ]] | { echo "$EXEC should exist by now. Exiting."; exit 1; }

./"$EXEC"
