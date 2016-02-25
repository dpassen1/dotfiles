#!/usr/bin/env bash

set -o nounset
set -o errexit

function ensure_destination_exists {
    local dest="$1"
    if [[ $dest != "." ]]; then
        dest="~/.$dest"
        echo "  Creating destination directory: $dest"
        mkdir -p "$dest"
    fi
}

function create_symbolic_link {
    local path="$1"
    local src="$PWD/$path"
    local dest_path=${path/%\.symlink/}
    local dest="$HOME/.$dest_path"
    ln -fs "$src" "$dest"
}

function link_dotfiles {
    find . -iname "*.symlink" | while read link; do
        echo "Linking "$link""

        local relpath=${link/#\.\//}
        local dir=$(dirname "$relpath")

        ensure_destination_exists $dir
        create_symbolic_link $relpath
    done
}

link_dotfiles
