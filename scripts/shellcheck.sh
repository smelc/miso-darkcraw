#!/usr/bin/env bash
#
# Some files that are not shellcheck-compliant are skipped.
# Importantly, new scripts are checked by default (as this is an exclude list),
# allowing to catch newly introduced errors.

for f in $(git ls-files "*.sh" | grep -v 'load-n-reload\|dl-large-assets\|install-hls')
do
  shellcheck "$f"
done
