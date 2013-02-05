#!/usr/bin/env sh

clear
for dir in `ls -d */`; do
    cd $dir
        if [ -d ".hg" ]; then
            echo "Mercurial: $dir"
            hg pull -u 1>/dev/null
            hg lg | head -n 2
            echo
        elif [ -d ".git" ]; then
            echo "Git: $dir"
            git pull --rebase 1>/dev/null
            git lg | head -n 1
            echo
        fi
    cd ..
done
