#!/usr/bin/env bash

EXEC="dist-newstyle/build/x86_64-linux/ghc-8.6.5/app-0.1.0.0/x/judge/build/judge/judge"

[[ -e "$EXEC" ]] || { echo "$EXEC doesn't exist. Please run 'cabal build judge'."; }

./$EXEC
