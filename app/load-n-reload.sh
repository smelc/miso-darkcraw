#!/bin/bash
#
# This script uses midori (https://www.midori-browser.org/)
# because it can be reloaded without giving focus to it
#
# Refreshing chrome requires giving it temporarily focus:
# https://unix.stackexchange.com/questions/87831/how-to-send-keystrokes-f5-from-terminal-to-a-gui-program
# which sucks because it makes the coding editor flickers
# at every save. For the record, here's how it would go for chrome:
# 
# xdotool search --name JSaddle windowactivate\
#   --sync %1 key F5 windowactivate\
#   $(xdotool getactivewindow)

set -ex

CABAL_ROOT="dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/app-0.1.0.0/x/app/build/app/app.jsexe"
CABAL_ASSETS="$CABAL_ROOT/assets"
CABAL_INDEX="$CABAL_ROOT/index.html"

# $1 is the command to check for
# $2 is the command to install it if missing
function install() {
  set +e
  which "$1" &> /dev/null
  if [[ "$?" != "0" ]];
  then
    $2
    which "$1" &> /dev/null
    [[ "$?" == "0" ]] || { echo "$1 could not be installed; exiting"; exit 1; }
  fi
  set -e
}

# http://eradman.com/entrproject/
install entr "sudo apt install entr"

function midori_listen() {
  git ls-files "*.hs" | entr -s "midori -e tab-reload"
}

function cabal_listen() {
  local -r ASSETS_TO_COPY="$(find assets -iname '*.png' -print0 | xargs)"
  git ls-files "*.hs" | entr -s "cabal --project-file=cabal.config build all && mkdir -p $CABAL_ASSETS && cp $ASSETS_TO_COPY $CABAL_ASSETS/."
}

if [[ -z "$1" ]]
then
  # jsaddle case, use midori (historical)

  # https://www.midori-browser.org/
  install midori "sudo snap install midori"

  # Start midori if not yet there:
  [[ $(pgrep midori) ]] || (midori -p "http://localhost:8080" &)

  # Regenerate js upon .hs saving:
  nix-shell --run reload

  # Auto refreshing of midori's tab upon saving a .hs file:
  midori_listen &
elif [[ "$1" == "release" ]]
then
  # release case, uses sensible-browser, i.e. your default browser
  # google-chrome (better dev tools and
  # since a while, midori needs manual refreshing; some better use chrome)

  if [[ -z "$IN_NIX_SHELL" ]]
  then
    # I prefer the nix-shell to be entered already, to avoid entering
    # it for every invocation of cabal
    echo "You should be in a nix-shell to execute 'load-n-reload.sh release'"
    echo "Please run: nix-shell -A release.env default.nix"
    echo "and execute me again in the resulting shell"
    exit 1
  fi

  if [[ -e "$CABAL_INDEX" ]]
  then
    sensible-browser "$CABAL_INDEX" &
  else
    echo "After your first compilation, issue this command to open the released game:"
    echo "sensible-browser $CABAL_INDEX"
  fi

  cabal_listen  # Not executing in the background, so that ctrl-c
  # this script exits 'entr'
else
  echo "Unrecognized argument: $1. Expecting no argument or 'release'"
  exit 1
fi
