#!/usr/bin/env sh

for dir in `ls -d */ | sort -f`; do
    cd $dir
    if [ -d ".git" ]; then
        echo "Git: ${dir%/}"
        git $@
        echo
    fi
    cd ..
done
