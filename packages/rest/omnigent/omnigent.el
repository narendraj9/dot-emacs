;;; omnigent.el --- Work with Omnigent sessions from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Narendra Joshi

;; Author: Narendra Joshi <narendra.joshi@grammarly.com>
;; Keywords: processes, tools
;; Package-Requires: ((emacs "29.1") (ghostel "0") (request "0.3.2"))

;;; Commentary:

;; Reattach to an existing Omnigent session in a terminal inside Emacs,
;; switch between the terminals that result, and act on the session of
;; the current terminal from a `transient' menu.  New sessions are
;; started by etc/llms-coding.el, which builds on `omnigent-terminal'.
;;
;; Session state comes from the Omnigent server's HTTP API
;; (`omnigent-server-url'); terminals are `omni' processes run by
;; ghostel.
;;
;; Entry points:
;;
;;   `omnigent-mode'           global minor mode that puts every command
;;                             under `omnigent-command-prefix'
;;   `omnigent-attach'         pick a stored session and reopen it
;;   `omnigent-attach-live'    join a session that is still running
;;   `omnigent-switch-buffer'  pick one of the live Omnigent terminals
;;   `omnigent-dispatch'       the menu, on \\`C-c C-o' inside a terminal

;;; Code:

(require 'ghostel)
(require 'let-alist)
(require 'request)
(require 'seq)
(require 'transient)

(defgroup omnigent nil
  "Drive Omnigent sessions from Emacs."
  :group 'tools
  :prefix "omnigent-")

(defcustom omnigent-server-url "http://127.0.0.1:6767"
  "Base URL of the Omnigent server holding session state.
The local server that `omni start' brings up listens on port 6767."
  :type 'string)

(defcustom omnigent-program "omni"
  "Name of the Omnigent command-line program."
  :type 'string)

(defcustom omnigent-session-limit 50
  "How many recent sessions to ask the server for."
  :type 'natnum)

(defcustom omnigent-environment
  '("OMNIGENT_RUNNER_ENV_PASSTHROUGH=OMNIGENT_NATIVE_PANE_IDLE_TIMEOUT_S,OMNIGENT_HARNESS_IDLE_TIMEOUT_S"
    "OMNIGENT_NATIVE_PANE_IDLE_TIMEOUT_S=0"
    "OMNIGENT_HARNESS_IDLE_TIMEOUT_S=0")
  "Environment entries prepended to `process-environment' for terminals.
The default disables Omnigent's two idle reapers, which otherwise kill
an idle native pane or harness subprocess after one hour.  `0' disables
each reaper.

The settings reach the reapers over two hops, both of which filter the
environment through an allowlist:

- CLI to daemon: local mode allows the `OMNIGENT_' prefix, so all three
  pass (`_build_host_daemon_env' in omnigent/cli.py).
- Daemon to runner: that allowlist has no `OMNIGENT_' prefix, so the two
  timeouts only pass because `OMNIGENT_RUNNER_ENV_PASSTHROUGH' names
  them (`_build_runner_env' in omnigent/host/connect.py).

The daemon is spawned by the first `omni' command that finds no live
one and is reused afterwards, so a change here takes effect on the next
cold start, not in an already-running daemon."
  :type '(repeat string))

(defcustom omnigent-keep-buffer-on-exit t
  "Whether a terminal buffer survives its Omnigent process exiting.
Non-nil appends the sentinel event to the buffer instead of killing it,
so a session that dies while you are away leaves the reason on screen.
A failed launch is kept either way."
  :type 'boolean)


;;; Keymaps

(defvar-keymap omnigent-session-mode-map
  :doc "Keymap active in ghostel terminals running Omnigent."
  "C-c C-o" #'omnigent-dispatch)

(define-minor-mode omnigent-session-mode
  "Minor mode offering `omnigent-dispatch' in an Omnigent terminal.
Bound on the \\`C-c' prefix, which ghostel passes through to Emacs.

\\{omnigent-session-mode-map}"
  :lighter " Omni")

(defvar-keymap omnigent-command-map
  :doc "Keymap of Omnigent commands, reached via `omnigent-command-prefix'."
  "a" #'omnigent-attach
  "A" #'omnigent-attach-live
  "b" #'omnigent-switch-buffer
  "o" #'omnigent-dispatch)

(defvar-keymap omnigent-mode-map
  :doc "Keymap of `omnigent-mode'.  Holds `omnigent-command-prefix' alone.")

(defun omnigent--bind-prefix (symbol prefix)
  "Bind `omnigent-command-map' at PREFIX, and store PREFIX in SYMBOL."
  ;; `define-key', not `keymap-set': `key-valid-p' rejects the angle-bracket
  ;; spelling of a remapped key such as `<C-m>'.
  (when (boundp symbol)
    (define-key omnigent-mode-map (symbol-value symbol) nil t))
  (set-default symbol prefix)
  (define-key omnigent-mode-map prefix omnigent-command-map))

(defcustom omnigent-command-prefix (kbd "<C-m> o")
  "Key sequence `omnigent-mode' binds `omnigent-command-map' to."
  :type 'key-sequence
  :set #'omnigent--bind-prefix)

;; `defcustom' does not run :set for the initial value.
(omnigent--bind-prefix 'omnigent-command-prefix omnigent-command-prefix)

;;;###autoload
(define-minor-mode omnigent-mode
  "Global minor mode making the Omnigent commands reachable.

\\{omnigent-command-map}"
  :global t
  :keymap omnigent-mode-map)


;;; Server API

(defun omnigent--request (method path &optional body)
  "Call METHOD on PATH of the Omnigent API and return the parsed reply.
PATH is relative to the API root, e.g. \"/sessions\".  BODY, when
non-nil, is an alist sent as the JSON request body.  Returns nil for a
reply that carries no body, such as a 204."
  (let* (;; We report failures ourselves, below; request.el's own messages
         ;; would only duplicate them.
         (request-message-level -1)
         (response
          (request (concat omnigent-server-url "/v1" path)
            ;; Sync because `omnigent-read-session' runs in an `interactive'
            ;; form, which has no way to await.  The wait polls with
            ;; `accept-process-output', so C-g still works.  Five seconds
            ;; instead of request.el's implicit 30 for a local server.
            :sync t
            :timeout 5
            :type method
            :headers (and body '(("Content-Type" . "application/json")))
            :data (and body (json-serialize body))
            :parser (lambda ()
                      (json-parse-buffer :object-type 'alist :array-type 'list
                                         :null-object nil)))))
    (unless (<= 200 (or (request-response-status-code response) 0) 299)
      (user-error "Omnigent %s %s: %s" method path
                  (or (request-response-status-code response)
                      ;; No status code at all: the request never landed.
                      (format "%s unreachable" omnigent-server-url))))
    (request-response-data response)))

(defun omnigent-sessions ()
  "Return the recent unarchived sessions, most recently active first."
  (alist-get 'data
             (omnigent--request
              "GET" (format "/sessions?limit=%d&sort_by=updated_at&order=desc"
                            omnigent-session-limit))))


;;; Reading a session

(defun omnigent--ordered-table (collection)
  "Return a completion table over COLLECTION that keeps COLLECTION's order."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action collection string predicate))))

(defun omnigent--session-line (session width)
  "Format SESSION for completion, padding its title to WIDTH columns."
  (let-alist session
    (concat (string-pad (truncate-string-to-width
                         (or .title "(untitled)") width nil nil t)
                        width)
            "  " (propertize (or .status "") 'face 'shadow)
            "  " (propertize (if .workspace (abbreviate-file-name .workspace) "")
                             'face 'completions-annotations))))

(defun omnigent-read-session (prompt)
  "Read a session with PROMPT and return it as an alist."
  (let* ((sessions (omnigent-sessions))
         (width (min 60 (apply #'max 20
                               (mapcar (lambda (session)
                                         (string-width
                                          (or (alist-get 'title session) "")))
                                       sessions))))
         (table (mapcar (lambda (session)
                          (cons (omnigent--session-line session width) session))
                        sessions)))
    (unless table
      (user-error "No Omnigent sessions on %s" omnigent-server-url))
    (cdr (assoc (completing-read prompt (omnigent--ordered-table table) nil t)
                table))))


;;; Terminals

(defvar-local omnigent-session-id nil
  "Identifier of the Omnigent session running in this buffer.")

(defun omnigent-buffer-p (buffer)
  "Return non-nil if BUFFER is a live ghostel terminal running Omnigent.
Terminals started outside this package count too: rather than rely on a
buffer-local marker, look for `omnigent-program' in the command ghostel
exec'd."
  (when-let* ((process (buffer-local-value 'ghostel--process buffer)))
    (and (process-live-p process)
         (string-match-p (regexp-quote omnigent-program)
                         (string-join (process-command process) " ")))))

(defun omnigent-buffers ()
  "Return the live Omnigent terminals, most recently used first."
  (seq-filter #'omnigent-buffer-p (buffer-list)))

(defun omnigent--on-exit (buffer event)
  "Note EVENT in BUFFER, or kill BUFFER after a clean exit.
EVENT is the process sentinel string.  See `omnigent-keep-buffer-on-exit'
and `ghostel-exit-functions'."
  (cond
   ((and (not omnigent-keep-buffer-on-exit)
         (string-prefix-p "finished" event))
    (kill-buffer buffer))
   ((buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n[omnigent] process exited: %s (at %s)\n"
                        (string-trim event)
                        (format-time-string "%F %T"))))))))

(defun omnigent-terminal (name directory command &optional session-id)
  "Show a ghostel terminal NAME running COMMAND in DIRECTORY.
COMMAND is a list of a program and its arguments, exec'd directly as the
terminal's process so quitting it closes the terminal.  SESSION-ID, when
known, goes into `omnigent-session-id' so `omnigent-dispatch' need not
ask.  A live terminal called NAME is reused rather than started again.

Runs with `omnigent-environment' prepended, and hands exits to
`omnigent--on-exit'.  Also the launcher etc/llms-coding.el builds on."
  (if-let* ((live (seq-find (lambda (buffer)
                              (and (equal (buffer-name buffer) name)
                                   (omnigent-buffer-p buffer)))
                            (buffer-list))))
      (pop-to-buffer live)
    (let ((default-directory (or directory default-directory))
          (process-environment (append omnigent-environment
                                       process-environment))
          (buffer (get-buffer-create name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'ghostel-mode)
          (ghostel-mode))
        (setq omnigent-session-id session-id)
        (omnigent-session-mode)
        ;; Let `omnigent--on-exit' decide whether the buffer survives.
        (setq-local ghostel-kill-buffer-on-exit nil)
        (add-hook 'ghostel-exit-functions #'omnigent--on-exit nil t))
      (pop-to-buffer buffer)
      (ghostel-exec buffer (car command) (cdr command)))))

(defun omnigent--session-buffer-name (session)
  "Return the terminal buffer name to use for SESSION."
  (let-alist session
    (format "*omnigent: %s*"
            (truncate-string-to-width (or .title .id) 40 nil nil t))))

;;;###autoload
(defun omnigent-attach (session)
  "Reopen SESSION in a terminal with `omni resume'.
Resuming restarts the session's harness.  To join a session that is
still running, use `omnigent-attach-live'."
  (interactive (list (omnigent-read-session "Resume session: ")))
  (let-alist session
    (omnigent-terminal (omnigent--session-buffer-name session) .workspace
                       (list omnigent-program "resume" .id) .id)))

;;;###autoload
(defun omnigent-attach-live (session)
  "Join the running SESSION with `omni attach', streaming its I/O.
Fails when nothing is live for SESSION; `omnigent-attach' reopens a
stored session instead."
  (interactive (list (omnigent-read-session "Attach to live session: ")))
  (let-alist session
    (omnigent-terminal (omnigent--session-buffer-name session) .workspace
                       (list omnigent-program "attach" .id) .id)))

;;;###autoload
(defun omnigent-switch-buffer ()
  "Switch to one of the live Omnigent terminals."
  (interactive)
  (let ((buffers (omnigent-buffers)))
    (unless buffers
      (user-error "No live Omnigent terminals"))
    (pop-to-buffer
     (read-buffer "Omnigent terminal: " (car buffers) t
                  ;; `read-buffer' hands the predicate either a name or a
                  ;; (NAME . BUFFER) cons.
                  (lambda (candidate)
                    (omnigent-buffer-p
                     (get-buffer (if (consp candidate)
                                     (car candidate)
                                   candidate))))))))


;;; Acting on a session

(defun omnigent--id ()
  "Return the session id of the current terminal, prompting when unknown."
  (or omnigent-session-id
      (alist-get 'id (omnigent-read-session "Session: "))))

(defun omnigent--patch (id field value)
  "Set FIELD of session ID to VALUE and report it."
  (omnigent--request "PATCH" (format "/sessions/%s" id) (list (cons field value)))
  (message "%s: %s" field value))

(defun omnigent-rename (id title)
  "Retitle session ID to TITLE."
  (interactive (list (omnigent--id) (read-string "New title: ")))
  (omnigent--patch id 'title title))

(defun omnigent-archive (id)
  "Archive session ID so it drops out of the session list."
  (interactive (list (omnigent--id)))
  (omnigent--patch id 'archived t))

(defun omnigent-set-permission-mode (id mode)
  "Switch the running claude-native session ID to permission MODE.
Only the modes Claude Code's shift+tab cycle reaches are accepted."
  (interactive
   (list (omnigent--id)
         (completing-read "Permission mode: "
                          '("default" "acceptEdits" "plan" "auto") nil t)))
  (omnigent--patch id 'permission_mode mode))

(defun omnigent-set-reasoning-effort (id effort)
  "Set the per-session reasoning EFFORT of session ID."
  (interactive
   (list (omnigent--id)
         (completing-read "Reasoning effort: "
                          '("none" "minimal" "low" "medium" "high" "xhigh"
                            "max" "default")
                          nil t)))
  (omnigent--patch id 'reasoning_effort effort))

(defun omnigent-set-model (id model)
  "Override the model of session ID with MODEL.
\"default\" removes the override."
  (interactive (list (omnigent--id) (read-string "Model (or default): ")))
  (omnigent--patch id 'model_override model))

(defun omnigent-browse (id)
  "Open session ID in the Omnigent web UI."
  (interactive (list (omnigent--id)))
  (browse-url (format "%s/c/%s" omnigent-server-url id)))

(defun omnigent-copy-id (id)
  "Copy session ID to the kill ring."
  (interactive (list (omnigent--id)))
  (kill-new id)
  (message "%s" id))

(defun omnigent-export (id file)
  "Export the transcript of session ID to FILE as JSONL."
  (interactive (list (omnigent--id)
                     (read-file-name "Export transcript to: "
                                     nil nil nil "transcript.jsonl")))
  (let ((file (expand-file-name file)))
    (with-temp-buffer
      (unless (zerop (call-process omnigent-program nil t nil
                                   "session" "export" "--id" id
                                   "--output" file))
        (user-error "%s" (string-trim (buffer-string)))))
    (message "Exported to %s" file)))

;;;###autoload (autoload 'omnigent-dispatch "omnigent" nil t)
(transient-define-prefix omnigent-dispatch ()
  "Act on the Omnigent session of the current terminal."
  [["Session"
    ("r" "Rename" omnigent-rename)
    ("k" "Archive" omnigent-archive)
    ("e" "Export transcript" omnigent-export)
    ("w" "Copy id" omnigent-copy-id)
    ("b" "Open in browser" omnigent-browse)]
   ["Agent"
    ("p" "Permission mode" omnigent-set-permission-mode)
    ("m" "Model" omnigent-set-model)
    ("R" "Reasoning effort" omnigent-set-reasoning-effort)]
   ["Go"
    ("a" "Resume a session" omnigent-attach)
    ("A" "Attach to a live session" omnigent-attach-live)
    ("s" "Switch terminal" omnigent-switch-buffer)]])


(provide 'omnigent)
;;; omnigent.el ends here
