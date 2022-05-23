# Prefer US English and use UTF-8
export LC_ALL="en_US.UTF-8"
export LANG="en_US"

setopt appendhistory beep extendedglob nomatch notify
bindkey -e

zstyle :compinstall filename '/Users/dbrenneman/.zshrc'

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

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

# kubectl autocomplete
[[ $commands[kubectl] ]] && source <(kubectl completion zsh)

alias editor="/Applications/Emacs.app/Contents/MacOS/bin/emacs"
alias edit="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"


aws-profile() {
  if [[ "$1" == "clear" ]]; then
    unset AWS_PROFILE
    unset AWS_SDK_LOAD_CONFIG
  else
    export AWS_PROFILE="$1"
    export AWS_SDK_LOAD_CONFIG=1
  fi
}

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

export GOPRIVATE=*.apple.com
export GOPATH=$HOME/go
export PATH="/usr/local/opt/curl/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
export PATH=$HOME/.cargo/bin:$GOPATH/bin:/usr/local/bin:/usr/local/opt/python@3.9/libexec/bin:/usr/local/opt/openssl@1.1/bin:$PATH
. "$HOME/.cargo/env"

[[ :$PATH: == *:$HOME/bin:* ]] || PATH=$HOME/bin:$PATH

eval "$(starship init zsh)"
