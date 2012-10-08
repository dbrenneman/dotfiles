#!/bin/bash
cd "$(dirname "$0")"
git pull
function doIt() {
	rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README.md" -av . ~
    # nXhtml mode for web development
    cd ~/.emacs.d/plugins
    git clone https://github.com/emacsmirror/nxhtml
    cd ~/.emacs.d/plugins/nxhtml
    git pull
    # js2 mode for JavaScript development
    cd ~/.emacs.d/plugins
    rm js2-20090723b.el
    wget http://js2-mode.googlecode.com/files/js2-20090723b.el
    # lintnode for flymake js linting
    cd ~/.emacs.d/plugins
    git clone https://github.com/davidmiller/lintnode.git
    cd ~/.emacs.d/plugins/lintnode
    git pull
    npm install express connect-form haml underscore
    # js-comint JavaScript REPL
    cd ~/.emacs.d/plugins
    rm js-comint.el
    wget http://voxel.dl.sourceforge.net/project/js-comint-el/js-comint-el/0.0.1/js-comint.el
    # flymake cursor error mode
    cd ~/.emacs.d/plugins
    rm flymake-cursor.el
    wget http://www.emacswiki.org/emacs/download/flymake-cursor.el
    # python mode
    cd ~/.emacs.d/plugins
    git clone https://github.com/fgallina/python.el.git python
    cd ~/.emacs.d/plugins/python
    git pull
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
