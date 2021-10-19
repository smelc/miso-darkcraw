#!/usr/bin/env bash
#
# Check that some modules are always imported qualified

declare -r MODULES="Tile"

# shellcheck disable=SC2155
declare -r FILES=$(git ls-files "app/*.hs" | xargs)

for MODULE in $MODULES
do
   SEARCHED="import $MODULE\$"
   echo "grep \"$SEARCHED\" $FILES"
   # We want word splitting
   # shellcheck disable=SC2086
   grep "$SEARCHED" $FILES
   # shellcheck disable=SC2181
   if [[ "$?" == "0" ]]; then
     echo "$SEARCHED found 💣 This is wrong! $MODULE should only be used qualified (except for \"import $MODULE ($MODULE)\")"
     exit 1
   fi
   echo "$SEARCHED not found ✅ $MODULE is only used qualified 👍"
done

echo "All good, exiting"
