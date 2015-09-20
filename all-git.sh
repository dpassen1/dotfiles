#!/usr/bin/env sh

set -o nounset
set -o errexit

find . -type d -d 1 | while read dir; do
    if [ -d "$dir/.git" ]; then
        echo "Git: ${dir%/}"
        (cd "$dir" && git "$@")
        echo
    fi
done
