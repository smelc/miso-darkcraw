#!/usr/bin/env bash
#
# Changes the value returned by app/client/Configuration.hs
#
# This script expects one argument: what to put after "get = " in the
# last line of Configuration.hs

[[ -n "$1" ]] || { echo "Configuration to set is missing"; exit 1; }

function remove_last_line() {
  [[ -e "$1" ]] || { echo "$1 is not a file"; return 1; }
  local -r nb_lines=$(wc -l "$1" | awk '{print $1}')
  local -r nb_lines_mm=$((nb_lines - 1))
  local -r tmp_file="/tmp/$(basename "$1")"
  head -n "$nb_lines_mm" "$1" > "$tmp_file"
  mv "$tmp_file" "$1"
}

declare -r CONFIG_FILE="app/shared/Configuration.hs"
remove_last_line "$CONFIG_FILE" || { echo "Could not remove last line of $CONFIG_FILE"; exit 1; }

echo "get = $1" >> "$CONFIG_FILE"
echo "Set configuration to:"
tail -n 1 "$CONFIG_FILE"
