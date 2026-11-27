# Read by every zsh, including `ssh host cmd`, which reads nothing else. Keep it to PATH and cheap exports.
# .zshrc sources this again in login shells, because /etc/zprofile's path_helper runs after it and moves
# /usr/local/bin and the system directories ahead of these.

typeset -U path fpath

# Homebrew (the static part of `brew shellenv`, without the subprocess)
if [[ -d /opt/homebrew ]]; then
  export HOMEBREW_PREFIX=/opt/homebrew HOMEBREW_CELLAR=/opt/homebrew/Cellar HOMEBREW_REPOSITORY=/opt/homebrew
  export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}"
  path=(/opt/homebrew/bin /opt/homebrew/sbin $path)
  fpath=(/opt/homebrew/share/zsh/site-functions $fpath)
fi

# User tools (uv, claude)
path=($HOME/.local/bin $path)

# Machine-specific additions (not tracked in this repo)
[[ -f $HOME/.zshenv.local ]] && source $HOME/.zshenv.local
