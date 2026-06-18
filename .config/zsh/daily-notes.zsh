# Daily notes in the knowledge repo.
#   dn             open (create) today's note in $EDITOR; rolls over unchecked todos
#   jot <text>     append a timestamped bullet to today's note
#   jot -t <text>  append an unchecked todo instead
#   eod            append an end-of-day summary (commits + shell history), then open
#   todo <text>    capture a standalone todo to the vault inbox (surfaces in todo.md)
#   todo           list open todos across the vault (terminal view of todo.md)
# Note: `jot` shadows the BSD /usr/bin/jot utility; use `command jot` for the original.
: ${KNOWLEDGE:=$HOME/knowledge}

_dn_today() { date +%F; }
_dn_file()  { print -- "$KNOWLEDGE/daily/$(_dn_today).md"; }

_dn_ensure() {
  emulate -L zsh
  setopt local_options null_glob
  local f today tmpl prev carry
  f="$(_dn_file)"; today="$(_dn_today)"; tmpl="$KNOWLEDGE/templates/Daily Note.md"
  [[ -e $f ]] && return 0
  mkdir -p "$KNOWLEDGE/daily"
  if [[ -e $tmpl ]]; then
    sed "s/{{date}}/$today/g" "$tmpl" > "$f"
  else
    printf -- '---\ndate: %s\ntags: [work, journal]\n---\n# %s\n\n## Focus\n- [ ] \n\n## Log\n' "$today" "$today" > "$f"
  fi
  # roll over unchecked todos from the most recent earlier daily note
  prev=$(print -l -- "$KNOWLEDGE"/daily/*.md | grep -v "$today" | sort | tail -1)
  if [[ -n $prev ]]; then
    carry="${f}.carry"
    grep -E '^[[:space:]]*- \[ \] .+' "$prev" > "$carry"
    if [[ -s $carry ]]; then
      awk -v cf="$carry" '{print} /^## Focus/ && !d {while ((getline l < cf) > 0) print l; d=1}' "$f" > "${f}.tmp" && mv "${f}.tmp" "$f"
    fi
    rm -f "$carry"
  fi
}

dn() { _dn_ensure; ${EDITOR:-hx} "$(_dn_file)"; }

jot() {
  _dn_ensure
  local f; f="$(_dn_file)"
  if [[ $1 == -t ]]; then shift; print -r -- "- [ ] $*" >> "$f"
  else print -r -- "- $(date +%H:%M) $*" >> "$f"; fi
}

eod() {
  _dn_ensure
  local f today repo; f="$(_dn_file)"; today="$(_dn_today)"
  {
    print -r -- ""
    print -r -- "## End of day ($(date +%H:%M))"
    print -r -- ""
    print -r -- "### Commits"
    for repo in "$HOME/code/devicecompute" "$HOME/code/mobile-host-driver"; do
      [[ -d $repo/.git ]] || continue
      git -C "$repo" log --all --since="$today 00:00" --author="Brenneman" --pretty="- ${repo:t}: %s (%h)" 2>/dev/null
    done
    print -r -- ""
    print -r -- "### Shell commands today (atuin)"
    atuin search --after "$today 00:00:00" --cmd-only 2>/dev/null | awk 'NF && !seen[$0]++' | head -50 | sed 's/^/- /'
  } >> "$f"
  ${EDITOR:-hx} "$f"
}

# Capture a standalone (non-daily) todo to the vault inbox. The Tasks plugin's
# "Open — everywhere else" query in todo.md surfaces it automatically, so this
# integrates with the global list without editing the (query-only) dashboard.
todo() {
  emulate -L zsh
  setopt local_options null_glob
  local inbox="$KNOWLEDGE/inbox.md"
  if (( $# == 0 )); then
    # No args: list open todos across the vault in the terminal, since
    # todo.md's Tasks queries only render in Obsidian. Two groups, mirroring
    # the dashboard; templates excluded.
    local fmt="s#^${KNOWLEDGE}/##; s#:[[:space:]]*- \[ \] #: #; s/^/  /"
    print -- "daily notes:"
    grep -rE '^[[:space:]]*- \[ \] .+' "$KNOWLEDGE/daily" --include='*.md' 2>/dev/null \
      | sort -r | sed -E "$fmt"
    print -- ""
    print -- "inbox:"
    grep -E '^[[:space:]]*- \[ \] .+' "$inbox" 2>/dev/null | sed -E "s#[[:space:]]*- \[ \] #  #"
    return
  fi
  if [[ ! -e $inbox ]]; then
    printf -- '---\ntitle: Inbox\ntags: [inbox]\n---\n# Inbox\n\nStandalone todos captured from the shell. Open items show in [[todo]].\n\n' > "$inbox"
  fi
  print -r -- "- [ ] $*" >> "$inbox"
}
