#!/usr/bin/env sh

clear
for dir in `ls -d */ | sort -f`; do
    cd $dir
    if [ -d ".hg" ]; then
        echo "Mercurial: $dir"
        if [ -z "`hg status -admrn`" ]; then
            echo "Clean. Pulling ..."
            hg pull --rebase 1>/dev/null
        fi
        hg lg | head -n 1
        echo
    elif [ -d ".git" ]; then
        echo "Git: $dir"
        if [ -z "`git status --short`" ]; then
            echo "Clean. Pulling ..."
            git pull --rebase 1>/dev/null
        fi
        git lg -n 1
        echo
    fi
    cd ..
done
