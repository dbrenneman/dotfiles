#!/bin/bash

# Brew install git go wireshark docker virtualbox vagrant emacs
# git clone powerline
# install dotfile links

cd "$(dirname "$0")"
git pull
function doIt() {
	rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README.md" -av . ~
    # Install zsh
    cd
    git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
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
