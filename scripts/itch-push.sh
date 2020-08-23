#!/usr/bin/env bash
#
# Script to publish the game to https://hgames.itch.io/pixel-card-wars

rm -Rf "$TMP_PATH"  # Erase previous run if any

# Compile
export PCW_LOCATION="itch"
(cd app && rm -Rf .ghc.environment.* -Rf && nix-build) || { echo "compilation failed"; exit 1; }
unset PCW_LOCATION

[[ "$1" != "--no-push" ]] || { echo "--no-push spcecified: exiting"; exit 0; }

if [[ ! $(command -v butler) ]]; then
  echo "You need butler installed: https://itch.io/docs/butler/"
  exit 1
fi

# Copy nix's result to /tmp
declare -r ARCHIVE_NAME="pixel-card-wars.zip"
declare -r TMP_PATH="/tmp/app.jsexe"
declare -r ARCHIVE_PATH="${TMP_PATH}/${ARCHIVE_NAME}"
cp -R "app/result-2/bin/app.jsexe" "$TMP_PATH"

# Fix permissions, because everything is readonly in the nix store
chmod +w "$TMP_PATH"
find "$TMP_PATH" -name \* -exec chmod +w {} \;

# Record version
git rev-parse HEAD > "$TMP_PATH/VERSION" || { echo "Cannot record version"; exit 1; }

# Create zip archive
(cd "$TMP_PATH" && zip -r "${ARCHIVE_NAME}" .) || { echo "Cannot zip"; exit 1; }

# Send to itch
butler push "$ARCHIVE_PATH" hgames/pixel-card-wars:html || { echo "butler failed"; exit 1; }

# Cleanup
rm -Rf "$TMP_PATH"
