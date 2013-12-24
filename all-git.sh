#!/usr/bin/env sh

clear
for dir in `ls -d */ | sort -f`; do
    cd $dir
    if [ -d ".git" ]; then
        echo "Git: ${dir%/}"
        git $@
        echo
    fi
    cd ..
done
