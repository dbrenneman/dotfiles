# dotfiles

Terminal-first development environment: zsh, helix, ghostty, starship, with Catppuccin Macchiato theming throughout.

## Fresh Machine Setup

```bash
# 1. Install Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
eval "$(/opt/homebrew/bin/brew shellenv)"

# 2. Clone as a bare repo
git clone --bare git@github.com:dbrenneman/dotfiles.git $HOME/.dotfiles

# 3. Define the alias (this is also in .zshrc once checked out)
alias dotfiles='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'

# 4. Checkout files into $HOME
dotfiles checkout
```

If checkout fails due to existing files (e.g. a default `.zshrc`):

```bash
dotfiles checkout 2>&1 | grep "^\t" | awk '{print $1}' | \
  xargs -I{} mv {} {}.bak
dotfiles checkout
```

```bash
# 5. Hide untracked files (so dotfiles status isn't noisy)
dotfiles config --local status.showUntrackedFiles no

# 6. Install all dependencies
brew bundle --file=~/Brewfile

# 7. Install tmux plugin manager and plugins
git clone https://github.com/tmux-plugins/tpm ~/.config/tmux/plugins/tpm
# After starting tmux, press Ctrl-T then I to install plugins

# 8. Import existing shell history into atuin
atuin import zsh

# 9. Restart shell
exec zsh
```

## Machine-Specific Overrides

These files are NOT tracked in this repo. Create them locally as needed:

**`~/.zshrc.local`** — Machine-specific shell config (env vars, aliases, tool completions). Sourced at the end of `.zshrc`.

**`~/.gitconfig.local`** — Machine-specific git config (work email, signing keys, URL rewrites). Included via `[include]` in `.gitconfig`.

## What's Included

| Path | Purpose |
|------|---------|
| `Brewfile` | Declarative package manifest — `brew bundle` installs everything |
| `.zshrc` | Shell config: fzf, atuin, zoxide, direnv, starship, eza aliases |
| `.gitconfig` | Git aliases, delta pager, Catppuccin theme, auto-setup remote |
| `.config/helix/` | Editor config, language servers, snippets (Go, Swift, Python, TS, PKL, Java) |
| `.config/lazygit/config.yml` | Lazygit with Catppuccin Macchiato theme |
| `.config/starship.toml` | Prompt: Catppuccin Macchiato, language versions, k8s context |
| `.config/ghostty/config` | Terminal: Catppuccin Mocha, Source Code Pro, transparency |
| `.config/bat/config` | Catppuccin syntax highlighting theme |
| `.config/atuin/` | Shell history config and Catppuccin theme |
| `.config/tmux/tmux.conf` | Tmux: Ctrl-T prefix, catppuccin, vim-style panes |
| `.config/git/ignore` | Global gitignore patterns |

## Managing Dotfiles

```bash
# Check status
dotfiles status

# Add a new file
dotfiles add -f ~/.config/some/new/config
dotfiles commit -m "Add some config"
dotfiles push

# Pull updates on another machine
dotfiles pull
```
