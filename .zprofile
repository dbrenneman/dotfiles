export EDITOR="/opt/homebrew/bin/emacsclient"
export PAGER="bat"
export GOPRIVATE=*.apple.com
export GOPATH=$HOME/go
export PATH="/opt/homebrew/bin:$PATH"
export PATH="/usr/local/opt/curl/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/go/bin:$PATH"
export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
export PATH=$HOME/.cargo/bin:$GOPATH/bin:/usr/local/bin:$PATH

. "$HOME/.cargo/env"

[[ :$PATH: == *:$HOME/bin:* ]] || PATH=$HOME/bin:$PATH
