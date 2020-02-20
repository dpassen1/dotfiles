#!/usr/bin/env bash

set -o nounset
set -o errexit

function ensure_brew_is_installed {
    command -v brew >/dev/null 2>&1 || { echo "Please install homebrew first" >&2; exit 1; }
}

function add_taps {
    echo "Adding taps ..."
    while read -r tap; do
        brew tap "$tap"
    done < ./brew/brew-tap-list.txt
}

function install_formulas {
    echo "Installing formulas ..."
    < ./brew/brew-list.txt xargs brew install
}

function install_casks {
    echo "Installing casks ..."
    < ./brew/brew-cask-list.txt xargs brew cask install
}

function setup_brew {
    echo 'Setting up homebrew'
    ensure_brew_is_installed
    add_taps
    install_formulas
    install_casks
}

setup_brew
