# Locale
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

CASE_SENSITIVE="true"
export EDITOR="hx"

# Homebrew
if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -f /usr/local/bin/brew ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
fi

# fzf (Catppuccin Macchiato)
export FZF_DEFAULT_OPTS=" \
--color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796 \
--color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6 \
--color=marker:#b7bdf8,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796 \
--color=selected-bg:#494d64 \
--border=\"rounded\""
source <(fzf --zsh)

# Shell integrations
eval "$(atuin init zsh)"
eval "$(zoxide init zsh)"
eval "$(direnv hook zsh)"
eval "$(starship init zsh)"

# Completions
if [[ -n "$HOMEBREW_PREFIX" ]]; then
  FPATH="${HOMEBREW_PREFIX}/share/zsh-completions:$FPATH"
fi
autoload -Uz compinit && compinit

# History
HISTSIZE=1000000000
HISTFILE=~/.zsh_history
SAVEHIST=1000000000
HISTDUP=erase
setopt appendhistory sharehistory incappendhistory extendedhistory

# Java (macOS)
if [[ -x /usr/libexec/java_home ]]; then
  export JAVA_HOME=$(/usr/libexec/java_home 2>/dev/null)
  [[ -n "$JAVA_HOME" ]] && export PATH="$JAVA_HOME/bin:$PATH"
fi

# PATH
[[ -n "$HOMEBREW_PREFIX" && -d "${HOMEBREW_PREFIX}/opt/libpq/bin" ]] && export PATH="${HOMEBREW_PREFIX}/opt/libpq/bin:$PATH"
[[ -n "$HOMEBREW_PREFIX" && -d "${HOMEBREW_PREFIX}/opt/gnu-tar/libexec/gnubin" ]] && export PATH="${HOMEBREW_PREFIX}/opt/gnu-tar/libexec/gnubin:$PATH"
[[ :$PATH: == *:$HOME/bin:* ]] || PATH="$HOME/bin:$PATH"

# qq debug log viewer (Go's q package)
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

# Modern aliases
alias ls="eza"
alias ll="eza -la --git --icons"
alias tree="eza --tree"
alias dotfiles='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'

# Machine-specific overrides (not tracked in dotfiles repo)
[[ -f "$HOME/.zshrc.local" ]] && source "$HOME/.zshrc.local"

# Zsh plugins (must be sourced last)
[[ -n "$HOMEBREW_PREFIX" ]] && source "${HOMEBREW_PREFIX}/share/zsh-autosuggestions/zsh-autosuggestions.zsh" 2>/dev/null
[[ -n "$HOMEBREW_PREFIX" ]] && source "${HOMEBREW_PREFIX}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" 2>/dev/null
