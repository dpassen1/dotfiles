#!/usr/bin/env sh

set -o nounset
set -o errexit

ls -d * | sort -f | while read dir; do
    if [ -d "$dir/.git" ]; then
        echo "${dir%/}"
        (cd "$dir" && git "$@")
        echo
    fi
done
