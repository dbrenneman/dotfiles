#!/bin/bash
cd "$(dirname "$0")"
git pull
function doIt() {
	rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README.md" -av . ~
    # nXhtml mode for web development
    cd ~/.emacs.d/plugins
    rm -rf ./nxhtml
    wget http://ourcomments.org/Emacs/DL/elisp/nxhtml/zip/nxhtml-2.08-100425.zip
    unzip nxhtml-2.08-100425.zip
    rm nxhtml-2.08-100425.zip
    # js2 mode for JavaScript development
    cd ~/.emacs.d/plugins
    wget http://js2-mode.googlecode.com/files/js2-20090723b.el
    # python mode
    cd ~/.emacs.d/plugins
    git clone https://github.com/fgallina/python.el.git python
    cd ~/.emacs.d/plugins/python
    git pull
    # pymacs
    cd ~/.emacs.d/plugins
    git clone https://github.com/pinard/Pymacs.git
    cd ~/.emacs.d/plugins/Pymacs
    git pull
    make install
    emacs -batch -eval ’(byte-compile-file "~/.emacs.d/plugins/Pymacs/pymacs.el")’
    # rope
    rm -rf ./rope
    hg clone https://bitbucket.org/agr/rope
    cd rope
    hg pull
    python setup.py install
    # ropemacs
    rm -rf ./ropemacs
    hg clone https://bitbucket.org/agr/ropemacs
    cd ropemacs
    hg pull
    python setup.py install
    # ropemode
    rm -rf ./ropemode
    wget http://pypi.python.org/packages/source/r/ropemode/ropemode-0.2.tar.gz
    gunzip ropemode-0.2.tar.gz
    rm ropemode-0.2.tar.gz
    cd ropemode
    python setup.py install
    # yasnippet
    cd ~/.emacs.d/plugins
    git clone https://github.com/capitaomorte/yasnippet
    cd ~/.emacs.d/plugins/yasnippet
    git pull
    # magit
    cd ~/.emacs.d/plugins
    git clone https://github.com/magit/magit
    cd ~/.emacs.d/plugins/magit
    git pull
    # solarized theme
    cd ~/.emacs.d/themes
    git clone https://github.com/sellout/emacs-color-theme-solarized
    cd ~/.emacs.d/themes/emacs-color-theme-solarized
    git pull
    # zenburn theme
    cd ~/.emacs.d/themes
    git clone https://github.com/bbatsov/zenburn-emacs.git
    cd ~/.emacs.d/themes/zenburn-emacs
    git pull
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
