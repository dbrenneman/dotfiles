export EDITOR="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n"
export PAGER="bat"
export GOPRIVATE=*.apple.com
export GOPATH=$HOME/go
export PATH=$HOME/.cargo/bin:$GOPATH/bin:/usr/local/bin:/usr/local/opt/python@3.9/libexec/bin:/usr/local/opt/openssl@1.1/bin:$PATH
export PATH=$PATH:/usr/local/go/bin
. "$HOME/.cargo/env"
