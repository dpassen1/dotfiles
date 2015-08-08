#!/usr/bin/env sh

for dir in $(ls -d */ | sort -f); do
    if [ -d "$dir/.git" ]; then
        echo "Git: ${dir%/}"
        (cd $dir && git $@)
        echo
    fi
done
