ZSH=$HOME/.oh-my-zsh

ZSH_THEME="af-magic"

plugins=(brew, command-not-found, django, git, github, gnu-utils, history-substring-search, node, npm, osx, pip, python)

source $ZSH/oh-my-zsh.sh

# Use Pacific Time
TZ='America/Los_Angeles'

# Prefer US English and use UTF-8
export LC_ALL="en_US.UTF-8"
export LANG="en_US"

# Path.
export PATH=/usr/local/bin:$PATH
