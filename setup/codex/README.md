# Codex status line

Add the two account-limit fields to `~/.codex/config.toml`, while preserving
Codex's default status-line fields:

```toml
[tui]
status_line = ["model-with-reasoning", "current-dir", "thread-name", "five-hour-limit", "weekly-limit"]
```

The limit fields show the percentage remaining in the primary and secondary
usage windows. Codex omits a field when the account does not provide that
window. The native status line does not show reset times or percentage used.

Run `/statusline` to change the fields interactively. Codex saves the result as
`tui.status_line` in `~/.codex/config.toml`.

Run `/usage` to inspect account usage from the TUI.

Documentation:

- [Developer commands](https://learn.chatgpt.com/docs/developer-commands)
- [Configuration reference](https://learn.chatgpt.com/docs/config-file/config-reference)
- [Status-line field definitions](https://github.com/openai/codex/blob/main/codex-rs/tui/src/bottom_pane/status_line_setup.rs)

## Tool-call keyboard navigation

Replace `Ctrl+O`'s copy action with activity focus:

```toml
[tui.keymap.global]
copy = []
focus_activity = "ctrl-o"
```

With an activity focused, use `Enter` to expand or collapse it, `Up` and `Down`
to select another activity, `Left` and `Right` to collapse or expand it, and
`Esc` to leave activity focus. Codex does not currently provide one action that
toggles every tool call at once. `Ctrl+T` opens the full transcript.

Run `/keymap` to manage these bindings interactively.

Documentation:

- [Codex keymap actions](https://github.com/openai/codex/blob/main/codex-rs/tui/src/keymap_setup/actions.rs)
- [Codex activity disclosure controls](https://github.com/openai/codex/blob/main/codex-rs/tui/src/transcript_view/disclosure.rs)
