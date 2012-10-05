#!/bin/bash
cd "$(dirname "$0")"
git pull
function doIt() {
	rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README.md" -av . ~
    cd ~/.emacs.d/plugins
    git clone https://github.com/fgallina/python.el.git python
    cd ~/.emacs.d/plugins/python
    git pull
    cd ~/.emacs.d/plugins
    git clone https://github.com/capitaomorte/yasnippet
    cd ~/.emacs.d/plugins/yasnippet
    git pull
    cd ~/.emacs.d/plugins
    git clone https://github.com/magit/magit
    cd ~/.emacs.d/plugins/magit
    git pull
    cd ~/.emacs.d/themes
    git clone https://github.com/sellout/emacs-color-theme-solarized
    cd ~/.emacs.d/themes/emacs-color-theme-solarized
    git pull
    cd ~/.emacs.d/themes
    git clone https://github.com/bbatsov/zenburn-emacs.git
    cd ~/.emacs.d/themes/zenburn-emacs
    git pull
    # nXhtml mode for web development
    cd ~/.emacs.d/plugins
    rm -rf ./nxhtml
    wget http://ourcomments.org/Emacs/DL/elisp/nxhtml/zip/nxhtml-2.08-100425.zip
    unzip nxhtml-2.08-100425.zip
    rm nxhtml-2.08-100425.zip
    mv nxhtml-2.08-100425 nxhtml
}
if [ "$1" == "--force" -o "$1" == "-f" ]; then
	doIt
else
	read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
	echo
	if [[ $REPLY =~ ^[Yy]$ ]]; then
		doIt
	fi
fi
unset doIt
source ~/.bash_profile
