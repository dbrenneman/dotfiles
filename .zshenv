aws-profile() {
  if [[ "$1" == "clear" ]]; then
    unset AWS_PROFILE
  else
    export AWS_PROFILE="$1"
  fi
}

export GOPATH=$HOME/go
export PATH=$GOPATH/bin:/usr/local/bin:$PATH
