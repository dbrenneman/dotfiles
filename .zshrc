# Prefer US English and use UTF-8
export LC_ALL="en_US.UTF-8"
export LANG="en_US"

setopt appendhistory beep extendedglob nomatch notify
bindkey -e

zstyle :compinstall filename '/Users/dbrenneman/.zshrc'

autoload -Uz compinit
compinit

#############################################################################
# History Configuration
##############################################################################
HISTSIZE=500000               #How many lines of history to keep in memory
HISTFILE=~/.zsh_history     #Where to save history to disk
SAVEHIST=1000000               #Number of history entries to save to disk
HISTDUP=erase               #Erase duplicates in the history file
setopt    appendhistory     #Append history to the history file (no overwriting)
setopt    sharehistory      #Share history across terminals
setopt    incappendhistory  #Immediately append to the history file, not just when a term is killed

#
# Only load Liquid Prompt in interactive shells, not from a script or from scp
[[ $- = *i* ]] && source ~/liquidprompt/liquidprompt

# kubectl autocomplete
[[ $commands[kubectl] ]] && source <(kubectl completion zsh)

alias editor="GOPRIVATE=*.apple.com GO111MODULE=on /Applications/Emacs.app/Contents/MacOS/Emacs -nw"
alias edit="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"


cvrgo () {
	t=$(mktemp)
	go test -coverprofile=$t $@ && go tool cover -func=$t && unlink $t
}
cvrgohtml () {
	t=$(mktemp)
	go test -covermode=count -coverprofile=$t $@ && go tool cover -func=$t && go tool cover -html=$t && unlink $t
}
qq() {
    clear

    logpath="$TMPDIR/q"
    if [[ -z "$TMPDIR" ]]; then
        logpath="/tmp/q"
    fi

    if [[ ! -f "$logpath" ]]; then
        echo 'Q LOG' > "$logpath"
    fi

    tail -100f -- "$logpath"
}

rmqq() {
    logpath="$TMPDIR/q"
    if [[ -z "$TMPDIR" ]]; then
        logpath="/tmp/q"
    fi
    if [[ -f "$logpath" ]]; then
        rm "$logpath"
    fi
    qq
}
