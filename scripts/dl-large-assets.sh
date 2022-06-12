#!/usr/bin/env bash
#
# Use that if you don't have the sources of the assets below
# (the ones in FILES) and wanna test the game like a developer

set -eux

declare -r SCHPLAF_ADDRESS="https://schplaf.org/smelc3/darkcraw/assets"

declare -r LOCAL_DIR="app/assets"
declare -r FILES="blue_slot.png card-bg-beastmen.png card-bg-evil.png card-bg-human.png card-bg-sylvan.png card-bg-undead.png card-blue-fat-border.png card-green-fat-border.png card-yellow-fat-border.png dark-forest.png dark-forest-hand.png deck.png errbox.png forest.png forest-hand.png moss-dungeon-board.png moss-dungeon-hand.png moss-dungeon-turn.png loot-bot-forest.png loot-top-sand.png red_slot.png round-trees-forest-spell.png singleplayer.png torchs.png turn.png turn-dark-forest.png welcome.png world.png"

[[ -e "app/assets" ]] || mkdir -p app/assets  # In case the script executes not from the root of the repo

for FILE in $FILES
do
  rm -Rf "$LOCAL_DIR/$FILE"
  wget "$SCHPLAF_ADDRESS/$FILE" -O "$LOCAL_DIR/$FILE"
done
