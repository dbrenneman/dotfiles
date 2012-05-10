#!/bin/bash
cd "$(dirname "$0")"
git pull
function doIt() {
    rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README.md" -av . ~
    cd ~/.emacs.d/plugins
    git clone https://github.com/capitaomorte/yasnippet
    cd ~/.emacs.d/themes
    git clone https://github.com/sellout/emacs-color-theme-solarized
}
doIt
unset doIt
source ~/.bash_profile
