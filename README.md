# dotfiles

Terminal-first development environment: zsh, helix, ghostty, starship, with Catppuccin Macchiato theming throughout.

## Fresh Machine Setup

```bash
# 1. Clone as a bare repo
git clone --bare git@github.com:dbrenneman/dotfiles.git $HOME/.dotfiles

# 2. Define the alias (this is also in .zshrc once checked out)
alias dotfiles='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'

# 3. Checkout files into $HOME
dotfiles checkout
```

If checkout fails due to existing files (e.g. a default `.zshrc`):

```bash
# Back up conflicting files, then retry
dotfiles checkout 2>&1 | grep "^\t" | awk '{print $1}' | \
  xargs -I{} mv {} {}.bak
dotfiles checkout
```

```bash
# 4. Hide untracked files (so dotfiles status isn't noisy)
echo '*' > $HOME/.dotfiles/info/exclude

# 5. Install dependencies
brew install helix starship fzf atuin zoxide direnv eza bat \
  git-delta fd ripgrep lazygit yazi tmux \
  zsh-autosuggestions zsh-syntax-highlighting zsh-completions

# 6. Language servers and formatters (for helix)
brew install gopls ruff pkl-lsp prettier typescript-language-server
brew install swift-format        # macOS only
# brew install jdtls             # optional: Java LSP

# 7. Restart shell
exec zsh

# 8. Import existing shell history into atuin
atuin import zsh
```

## Machine-Specific Overrides

These files are NOT tracked in this repo. Create them locally as needed:

**`~/.zshrc.local`** — Machine-specific shell config (env vars, aliases, tool completions). Sourced at the end of `.zshrc`.

**`~/.gitconfig.local`** — Machine-specific git config (email, signing, URL rewrites). Included via `[include]` in `.gitconfig`.

## What's Included

| Path | Purpose |
|------|---------|
| `.zshrc` | Shell config: fzf, atuin, zoxide, direnv, starship, eza aliases |
| `.gitconfig` | Git aliases, delta pager, Catppuccin theme |
| `.config/helix/` | Editor config, language servers, snippets (Go, Swift, Python, TS, PKL) |
| `.config/starship.toml` | Prompt: Catppuccin Macchiato, language versions, k8s context |
| `.config/ghostty/config` | Terminal settings |
| `.config/bat/config` | Catppuccin syntax highlighting theme |
| `.config/atuin/` | Shell history config and Catppuccin theme |
| `.config/tmux/tmux.conf` | Tmux config (Ctrl-T prefix, catppuccin, vim-style panes) |
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
