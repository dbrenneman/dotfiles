ZSH=$HOME/.oh-my-zsh

ZSH_THEME="af-magic"

plugins=(brew, django, git, github, heroku, node, npm, osx, supervisor, pip, python, virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

# Use Pacific Time
TZ='America/Los_Angeles'

# Prefer US English and use UTF-8
export LC_ALL="en_US.UTF-8"
export LANG="en_US"

# Path.
export PATH=/usr/local/bin:$PATH
