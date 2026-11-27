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

# 7. Configure macOS defaults (keyboard, dock, finder, etc.)
./setup-macos.sh

# 8. Import existing shell history into atuin
atuin import zsh

# 9. Restart shell
exec zsh
```

## Machine-Specific Overrides

These files are NOT tracked in this repo. Create them locally as needed:

**`~/.zshrc.local`**: machine-specific shell config (env vars, aliases, tool completions). Sourced near the end of `.zshrc`.

**`~/.zshenv.local`**: machine-specific PATH entries. Sourced by `.zshenv`, so every zsh reads it, including `ssh host cmd`.

**`~/.gitconfig.local`**: machine-specific git config (work email, signing keys, URL rewrites). Included via `[include]` in `.gitconfig`.

## What's Included

| Path | Purpose |
|------|---------|
| `Brewfile` | Declarative package manifest: `brew bundle` installs everything |
| `setup-macos.sh` | macOS defaults: fast key repeat, auto-hide dock, Finder tweaks |
| `.zshenv` | PATH (Homebrew, `~/.local/bin`) for every zsh, including non-interactive ssh commands |
| `.zshrc` | Shell config: fzf, atuin, zoxide, starship, eza aliases; tool init output cached in `~/.cache/zsh` |
| `.gitconfig` | Git aliases, delta pager, Catppuccin theme, auto-setup remote |
| `.config/helix/` | Editor config, language servers, snippets (Go, Swift, Python, TS, PKL, Java) |
| `.config/lazygit/config.yml` | Lazygit with Catppuccin Macchiato theme |
| `.config/hunk/config.toml` | Hunk diff reviewer: Catppuccin Macchiato, wrapped lines, agent notes shown |
| `.config/starship.toml` | Prompt: Catppuccin Macchiato, git status, command duration; language modules off |
| `.config/ghostty/config` | Terminal: Catppuccin Macchiato, JetBrains Mono, blur, quick terminal, command-finished notifications |
| `.config/bat/config` | Catppuccin syntax highlighting theme |
| `.config/atuin/` | Shell history config and Catppuccin theme |
| `.config/tmux/tmux.conf` | Tmux: Ctrl-T prefix, native Macchiato theme, no plugins, tool popups |
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
