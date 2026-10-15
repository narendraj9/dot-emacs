# Pi keybindings

On a new machine, merge these into `${PI_CODING_AGENT_DIR:-$HOME/.pi/agent}/keybindings.json` (preserve other bindings), then run `/reload`.

```json
{
  "tui.editor.cursorUp": ["up", "ctrl+p"],
  "tui.editor.cursorDown": ["down", "ctrl+n"],
  "app.model.cycleForward": [],
  "tui.editor.historyPrevious": "alt+p",
  "tui.editor.historyNext": "alt+n"
}
```

`C-p/C-n`: up/down. `M-p/M-n`: prompt history. `C-p` no longer cycles models; use `/model`.

Packages for web search/fetch and code diagnostics:

```sh
pi install npm:pi-web-access
pi install npm:pi-lens
```
