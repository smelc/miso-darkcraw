#!/bin/bash
#
# Execute --help for help

SERVER_PID_FILE="/tmp/darkcraw_server_pid"

if [[ $(command -v ghcjs) ]];  # Give precedence to ghcjs, intended for release case
then
  COMPILER="ghcjs"
elif [[ $(command -v ghc) ]];
then
  COMPILER="ghc"
else
  echo "Neither ghc nor ghcjs available. Are you in a nix-shell?"
  exit 1
fi

GHC_VERSION="$($COMPILER --numeric-version)" || { echo "$COMPILER version cannot be found"; exit 1; }
case $GHC_VERSION in
  "8.10.7")
          CABAL_ROOT="dist-newstyle/build/js-ghcjs/ghcjs-$GHC_VERSION/app-0.1.0.0/x/app/build/app/app.jsexe"
          ;;
  "8.6.4"|"8.6.0.1")
          CABAL_ROOT="dist-newstyle/build/x86_64-linux/ghcjs-$GHC_VERSION/app-0.1.0.0/x/app/build/app/app.jsexe"
          ;;
  *)
          echo "Unsupported GHC version: $GHC_VERSION. Please adapt load-n-reload.sh"
          exit 1
          ;;
esac
CABAL_ASSETS="$CABAL_ROOT/assets"
CABAL_INDEX="$CABAL_ROOT/index.html"
CABAL_SERVER_EXEC="dist-newstyle/build/x86_64-linux/ghc-$GHC_VERSION/app-0.1.0.0/x/server/build/server/server"

# $1 is the command to check for
# $2 is the command to install it if missing
function install() {
  set +e
  command -v "$1" &> /dev/null
  if [[ "$?" != "0" ]];
  then
    echo "$2"
    $2
    command -v "$1" &> /dev/null
    [[ "$?" == "0" ]] || { echo "$1 could not be installed; exiting"; exit 1; }
  fi
  set -e
}

function run() {
  echo "$@"
  "$@"
}

# http://eradman.com/entrproject/
install entr "sudo apt install entr"

function cabal_listen() {
  local -r ASSETS_TO_COPY="$(find assets -iname '*.png' -print0 | xargs -0)"
  git ls-files "*.hs" | entr -s "cabal --project-file=cabal.config build app && mkdir -p $CABAL_ASSETS && cp $ASSETS_TO_COPY $CABAL_ASSETS/."
}

function cabal_server_listen() {
  git ls-files client server | entr -s "cabal build server && kill_launch_server"
}

function cabal_test_listen() {
  echo "test/Test.hs" | entr -s "cabal test --test-show-details=streaming"
}

function check_in_nix_shell() {
  if [[ -z "$IN_NIX_SHELL" ]]
  then
    # I prefer the nix-shell to be entered already, to avoid entering
    # it for every invocation of cabal
    echo "You should be in a nix-shell to execute 'load-n-reload.sh [release|server|test]'"
    echo "Please run:"
    echo "  nix-shell -A release.env default.nix (release and server cases)"
    echo "  nix-shell (test case)"
    echo "and execute me again in the resulting shell"
    exit 1
  fi
}

function display_help() {
  echo "Usage: ./load-n-reload [release|server|test]?"
  echo "  Without argument, launches a jsaddle webserver at localhost:8080 that is updated when code changes"
  echo "  With argument 'release', regenerates client $CABAL_ROOT/index.html when its code changes"
  echo "  With argument 'server', regenerates server at $CABAL_SERVER_EXEC when its code changes and restart it"
  echo "  With argument 'test', reexecute 'Test.hs' when it changes"
}

function kill_server() {
  if [[ ! -e "$SERVER_PID_FILE" ]]; then return 0; fi
  local -r SERVER_PID=$(cat $SERVER_PID_FILE)
  run kill -9 "$SERVER_PID"
  run rm -Rf "$SERVER_PID_FILE"
  return 0
}

function launch_server() {
  if [[ ! -e "$CABAL_SERVER_EXEC" ]]; then return 0; fi
  echo "$CABAL_SERVER_EXEC &"
  $CABAL_SERVER_EXEC &
  echo $! > "$SERVER_PID_FILE"
}

function kill_launch_server() {
  kill_server
  launch_server
}

export CABAL_SERVER_EXEC
export SERVER_PID
export SERVER_PID_FILE
export -f kill_server
export -f kill_launch_server
export -f launch_server
export -f run

function on_exit() {
  rm -Rf "$SERVER_PID_FILE"
}

trap on_exit EXIT

if [[ -z "$1" ]]
then
  # jsaddle case

  # Regenerate js upon .hs saving:
  nix-shell --run reload

  declare -r CMD="sensible-browser \"http://localhost:8080\""
  echo "Execute $CMD to open the webserver"
elif [[ "$1" == *"help" ]]
then
  display_help
  exit 0
elif [[ "$1" == "server" ]]
then
  check_in_nix_shell
  kill_launch_server
  cabal_server_listen  # Not executing in the background, so that ctrl-c
  # this script exits 'entr'
elif [[ "$1" == "release" ]]
then
  # release case, uses sensible-browser, i.e. your default browser

  check_in_nix_shell

  command -v ghcjs &> /dev/null || { echo "ghcjs unavailable, is the nix-shell's flavor correct? Please enter it as follows: nix-shell -A release.env default.nix"; exit 1; }

  declare -r CMD="sensible-browser \"$CABAL_INDEX\""
  if [[ -e "$CABAL_INDEX" ]]
  then
    echo "Execute $CMD to open the webserver"
  else
    echo "After your first compilation, issue this command to open the released game:"
    echo "$CMD"
  fi

  cabal_listen  # Not executing in the background, so that ctrl-c
  # this script exits 'entr'
elif [[ "$1" == "test" ]]
then
  check_in_nix_shell

  cabal_test_listen  # Not executing in the background, so that ctrl-c
  # this script exits 'entr'
else
  echo "Unrecognized argument: $1. Expecting no argument or one of: 'server', 'release', or 'test'"
  exit 1
fi
