#!/usr/bin/env sh

set -o nounset
set -o errexit

find . -maxdepth 1 -type d | sort -f | while read -r dir; do
    if [ -d "$dir/.git" ]; then
        echo "${dir%/}"
        (cd "$dir" && git $@)
        echo
    fi
done
