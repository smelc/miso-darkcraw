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

set -eux

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
# https://www.midori-browser.org/
install midori "sudo snap install midori"

function midori_listen() {
  git ls-files "*.hs" | entr -s "midori -p -e tab-reload"
}

# Auto refreshing of midori's tab upon saving a .hs file:
midori_listen &

# Start midori if not yet there:
[[ $(pgrep midori) == "0" ]] || (midori "http://localhost:8080" &)

# Regenerate js upon .hs saving:
nix-shell --run reload
