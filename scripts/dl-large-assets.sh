#!/usr/bin/env bash
#
# Use that if you don't have the sources of the assets below
# (the ones inf FILES) and wanna test the game like a developer

set -eux

declare -r SCHPLAF_ADDRESS="https://schplaf.org/hgames/darkcraw/assets"

declare -r LOCAL_DIR="app/assets"
declare -r FILES="forest.png forest-hand.png"

for FILE in $FILES
do
  rm -Rf "$LOCAL_DIR/$FILE"
  wget "$SCHPLAF_ADDRESS/$FILE" -O "$LOCAL_DIR/$FILE"
done