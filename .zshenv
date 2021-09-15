aws-profile() {
  if [[ "$1" == "clear" ]]; then
    unset AWS_PROFILE
    unset AWS_SDK_LOAD_CONFIG
  else
    export AWS_PROFILE="$1"
    export AWS_SDK_LOAD_CONFIG=1
  fi
}

export GOPATH=$HOME/go
export PATH=$GOPATH/bin:/usr/local/bin:/usr/local/opt/python@3.9/libexec/bin:/usr/local/opt/openssl@1.1/bin:$PATH
source "$HOME/.cargo/env"
