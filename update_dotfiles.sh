#!/bin/bash
# 1) Back up existing configuration files
# 2) Remove existing configuration files
# 3) Update dotfiles to latest production branch HEAD
# 4) Link dotfiles into place
# 5) Display warning
export USER=`whoami`
export HOMEDIR=/home/$USER
# Check to see if we are already set up
# Check for a link from .dotfiles/.bashrc to .bashrc
# Then set the existing variable accordingly
# 1) Back up existing configuration files
if $HOMEDIR/.bashrc:
# Check to see if this is a link
mkdir $HOMEDIR/.dotfiles_previous
cp $HOMEDIR/.bashrc $HOMEDIR/.dotfiles_previous/bashrc
# 2) Remove existing configuration files
rm $HOMEDIR/.bashrc
# 3) Update dotfiles to latest production branch HEAD

# 4) Link dotfiles into place
# 5) Display warning
ln -s /home/$USER/.dotfiles/.bashrc