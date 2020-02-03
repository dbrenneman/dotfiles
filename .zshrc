# Prefer US English and use UTF-8
export LC_ALL="en_US.UTF-8"
export LANG="en_US"

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt appendhistory beep extendedglob nomatch notify
bindkey -e

zstyle :compinstall filename '/Users/dbrenneman/.zshrc'

autoload -Uz compinit
compinit

# Appends every command to the history file once it is executed
setopt inc_append_history
# Reloads the history whenever you use it
setopt share_history

#
# Only load Liquid Prompt in interactive shells, not from a script or from scp
[[ $- = *i* ]] && source ~/liquidprompt/liquidprompt

alias editor="GOPRIVATE=*.apple.com GO111MODULE=on /Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias edit="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
