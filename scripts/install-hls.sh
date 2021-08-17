#!/usr/bin/env bash
#
# Install https://github.com/haskell/haskell-language-server to hls/

declare -r HLS_DIR="hls"
declare -r GHC_VERSION="8.6.5"
declare -r HLS_BIN="$HLS_DIR/haskell-language-server-$GHC_VERSION"
declare -r HLS_WRAPPER="$HLS_DIR/haskell-language-server-wrapper"

[[ -n "$1" ]] || { echo "install-hls.sh requires the version to install, like '1.3.0'"; exit 1; }

run() {
  echo "$@"
  $@
}

[[ -e "$HLS_DIR" ]] || run mkdir "$HLS_DIR" || exit 1

if [[ -e "$HLS_BIN" ]]; then
  VERSION=$(./$HLS_BIN --version | awk '{print $3}')
  if [[ "$VERSION" == "$1" ]]; then
    echo "$VERSION is already installed. Exiting."
    exit 0
  fi
  SAVE_DIR="$HLS_DIR/hls-$VERSION"
  if [[ ! -e "$SAVE_DIR" ]]; then
    echo "Saving hls $VERSION to $SAVE_DIR"
    run mkdir -p "$SAVE_DIR"
    for f in $HLS_BIN $HLS_WRAPPER; do
      if [[ -e "$f" ]]; then
        run mv $f "$SAVE_DIR/."
      fi
    done
  fi
fi

HLS_BIN_DL="haskell-language-server-Linux-8.6.5"
HLS_WRAPPER_DL="haskell-language-server-wrapper-Linux"

wget "https://github.com/haskell/haskell-language-server/releases/download/$1/$HLS_BIN_DL.gz" || exit 1
wget "https://github.com/haskell/haskell-language-server/releases/download/$1/$HLS_WRAPPER_DL.gz" || exit 1

cleanup() {
  run rm -Rf "$HLS_BIN_DL"
  run rm -Rf "$HLS_WRAPPER_DL"
}

trap cleanup EXIT

for f in $HLS_BIN_DL $HLS_WRAPPER_DL; do
  run gunzip "$f.gz" || exit 1
  run chmod +x "$f" || exit 1
done

run mv "$HLS_BIN_DL" "$HLS_DIR/haskell-language-server-$GHC_VERSION" || exit 1
run mv "$HLS_WRAPPER_DL" "$HLS_DIR/haskell-language-server-wrapper" || exit 1
