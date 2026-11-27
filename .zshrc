# Locale
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

export EDITOR="hx"

# MOTD: rainbow Apple logo (static file, no subprocess). Interactive terminal only.
[[ -o interactive && -t 1 && -z "$CLAUDECODE" && $SHLVL -le 1 && -r ~/.config/motd/apple.ansi ]] \
  && print -r -- "$(<~/.config/motd/apple.ansi)"

# PATH lives in ~/.zshenv. In a login shell, /etc/zprofile's path_helper has since reordered it, so apply it again.
[[ -o login ]] && source ~/.zshenv

# _cached_source <name> <dependency> <command...>
# Sources the output of <command>, kept in ~/.cache/zsh/<name>.zsh, instead of running it at every startup.
# Regenerates when <dependency> changes: its resolved path (a brew upgrade changes the Cellar path) or a newer mtime.
_cached_source() {
  local name=$1 dep=$2; shift 2
  [[ -n $dep && -e $dep ]] || return 0
  local cache=${XDG_CACHE_HOME:-$HOME/.cache}/zsh/$name.zsh key="# ${dep:A}" first
  [[ -e $cache ]] && read -r first < $cache
  if [[ $first != "$key" || $dep -nt $cache ]]; then
    mkdir -p ${cache:h}
    { print -r -- "$key"; "$@" 2>/dev/null } >| $cache
  fi
  source $cache
}

# Completions. The dump is rebuilt once a day; otherwise compinit -C trusts it.
fpath=(${HOMEBREW_PREFIX:+$HOMEBREW_PREFIX/share/zsh-completions} $fpath)
autoload -Uz compinit
() {
  setopt local_options extended_glob   # (#q) needs it; globally it breaks HEAD^ and similar
  if [[ -n ~/.zcompdump(#qN.mh+24) ]]; then
    compinit && touch ~/.zcompdump
  else
    compinit -C
  fi
}
zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ''

# fzf (Catppuccin Macchiato). Before atuin, so atuin keeps Ctrl-R.
export FZF_DEFAULT_OPTS=" \
--color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796 \
--color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6 \
--color=marker:#b7bdf8,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796 \
--color=selected-bg:#494d64 \
--border=\"rounded\""
_cached_source fzf "$commands[fzf]" fzf --zsh

# Shell integrations (zoxide after compinit, or z gets no completion)
_cached_source atuin "$commands[atuin]" atuin init zsh
_cached_source zoxide "$commands[zoxide]" zoxide init zsh
_cached_source starship "$commands[starship]" starship init zsh --print-full-init

# History (minimal fallback; atuin is primary)
HISTSIZE=10000
HISTFILE=~/.zsh_history
SAVEHIST=10000
setopt appendhistory incappendhistory extendedhistory

# Shell options
setopt auto_cd
setopt correct

# Java (macOS). Cached until a JDK is added or removed.
_cached_source java_home /Library/Java/JavaVirtualMachines \
  zsh -c 'h=$(/usr/libexec/java_home 2>/dev/null) && print -r -- "export JAVA_HOME=${(q)h}; path=(\$JAVA_HOME/bin \$path)"'

# PATH
[[ -n "$HOMEBREW_PREFIX" && -d "${HOMEBREW_PREFIX}/opt/libpq/bin" ]] && path=("${HOMEBREW_PREFIX}/opt/libpq/bin" $path)
[[ -n "$HOMEBREW_PREFIX" && -d "${HOMEBREW_PREFIX}/opt/gnu-tar/libexec/gnubin" ]] && path=("${HOMEBREW_PREFIX}/opt/gnu-tar/libexec/gnubin" $path)
path=($HOME/bin $path)

# Man pages through bat
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export MANROFFOPT="-c"

# Modern aliases
alias ls="eza"
alias ll="eza -la --git --icons"
alias tree="eza --tree"
alias cat="bat"
alias dotfiles='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'

# Machine-specific overrides (not tracked in dotfiles repo)
[[ -f "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"
path=($path)   # re-apply typeset -U: the sourced files prepend through the PATH string, which skips it

# Zsh plugins (must be sourced last)
[[ -n "$HOMEBREW_PREFIX" ]] && source "${HOMEBREW_PREFIX}/share/zsh-autosuggestions/zsh-autosuggestions.zsh" 2>/dev/null
[[ -n "$HOMEBREW_PREFIX" ]] && source "${HOMEBREW_PREFIX}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" 2>/dev/null

# Daily notes (dn / jot / eod)
[ -f ~/.config/zsh/daily-notes.zsh ] && source ~/.config/zsh/daily-notes.zsh
