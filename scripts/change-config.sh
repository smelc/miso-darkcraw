#!/usr/bin/env bash
#
# Changes the value returned by app/client/Configuration.hs
# 
# This script expects two or three arguments: the Edition, the Location,
# and optionally the head's hash (see Configuration.hs)

[[ -n "$1" ]] || { echo "Edition to set is missing"; exit 1; }
[[ -n "$2" ]] || { echo "Location to set is missing"; exit 1; }

function remove_last_line() {
  [[ -e "$1" ]] || { echo "$1 is not a file"; return 1; }
  local -r nb_lines=$(wc -l "$1" | awk '{print $1}')
  local -r nb_lines_mm=$((nb_lines - 1))
  local -r tmp_file="/tmp/$(basename "$1")"
  head -n "$nb_lines_mm" "$1" > "$tmp_file"
  mv "$tmp_file" "$1"
}

declare -r CONFIG_FILE="app/client/Configuration.hs"
remove_last_line "$CONFIG_FILE" || { echo "Could not remove last line of $CONFIG_FILE"; exit 1; }

if [[ -z "$3" ]]; then
  declare -r HASH="Nothing"
else
  declare -r HASH="(Just \"$3\")"
fi
echo "configuration = Configuration $1 $2 $HASH" >> "$CONFIG_FILE"
echo "Set configuration to:"
tail -n 1 "$CONFIG_FILE"
