;;; omnigent.el --- Work with Omnigent sessions from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Narendra Joshi

;; Author: Narendra Joshi <narendra.joshi@grammarly.com>
;; Keywords: processes, tools
;; Package-Requires: ((emacs "29.1") (ghostel "0") (request "0.3.2"))

;;; Commentary:

;; Start an Omnigent session, or reattach to an existing one, in a
;; terminal inside Emacs; switch between those terminals; act on
;; the current terminal from a `transient' menu, or start a new one.
;;
;; Session state comes from the Omnigent server's HTTP API
;; (`omnigent-server-url'); terminals are `omni' processes run by
;; ghostel.
;;
;; Entry points:
;;
;;   `omnigent-mode'           global minor mode that opens the menu on
;;                             `omnigent-command-prefix'
;;   `omnigent-start'          create a session for a harness over the
;;                             API, filed under the current project,
;;                             then boot the harness onto it
;;   `omnigent-attach'         pick a session and bring it up
;;   `omnigent-switch-buffer'  pick one of the live Omnigent terminals
;;   `omnigent-dispatch'       the menu, also on \\`C-c C-o' inside a
;;                             terminal

;;; Code:

(require 'ghostel)
(require 'let-alist)
(require 'project)
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

(defvar-keymap omnigent-mode-map
  :doc "Keymap of `omnigent-mode'.  Holds `omnigent-command-prefix' alone.")

(defun omnigent--bind-prefix (symbol prefix)
  "Bind `omnigent-dispatch' at PREFIX, and store PREFIX in SYMBOL."
  ;; `define-key', not `keymap-set': `key-valid-p' rejects the angle-bracket
  ;; spelling of a remapped key such as `<C-m>'.
  (when (boundp symbol)
    (define-key omnigent-mode-map (symbol-value symbol) nil t))
  (set-default symbol prefix)
  (define-key omnigent-mode-map prefix #'omnigent-dispatch))

(defcustom omnigent-command-prefix (kbd "<C-m> o")
  "Key sequence `omnigent-mode' binds `omnigent-dispatch' to."
  :type 'key-sequence
  :set #'omnigent--bind-prefix)

;; `defcustom' does not run :set for the initial value.
(omnigent--bind-prefix 'omnigent-command-prefix omnigent-command-prefix)

;;;###autoload
(define-minor-mode omnigent-mode
  "Global minor mode binding `omnigent-dispatch' to reach the commands.

\\{omnigent-mode-map}"
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

(defun omnigent--find-named (path name)
  "Return the entry called NAME in the list the API serves at PATH."
  (seq-find (lambda (entry) (equal (alist-get 'name entry) name))
            (alist-get 'data (omnigent--request "GET" path))))

(defun omnigent-agent-id (name)
  "Return the id of the registered Omnigent agent called NAME."
  (or (alist-get 'id (omnigent--find-named "/agents?limit=100" name))
      (user-error "No Omnigent agent named %s" name)))

(defun omnigent-project-id (name)
  "Return the id of the Omnigent project called NAME, creating it if absent."
  (or (alist-get 'id (omnigent--find-named "/projects?limit=100" name))
      (alist-get 'id (omnigent--request "POST" "/projects" `((name . ,name))))))


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

(defun omnigent-terminal-live-p (buffer)
  "Return non-nil if BUFFER is a live ghostel terminal.
The ghostel plumbing shared by Omnigent terminals and the vanilla
launchers in etc/llms-coding.el: liveness is the buffer's own ghostel
process, since ghostel talks to a pty connection rather than a
subprocess, so `process-command' is nil and there is no command line to
search."
  (and (buffer-live-p buffer)
       (when-let* ((process (buffer-local-value 'ghostel--process buffer)))
         (process-live-p process))))

(defun omnigent-buffer-p (buffer)
  "Return non-nil if BUFFER is a live ghostel terminal running Omnigent.
Recognised by its `omnigent-session-id' on top of a live ghostel
process (`omnigent-terminal-live-p')."
  (and (buffer-local-value 'omnigent-session-id buffer)
       (omnigent-terminal-live-p buffer)))

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

(defun omnigent--live-terminal (name)
  "Return the live ghostel terminal called NAME, if there is one.
Matches on liveness alone (`omnigent-terminal-live-p'), so it also finds
the vanilla terminals in etc/llms-coding.el, which carry no session id."
  (when-let* ((buffer (get-buffer name)))
    (and (omnigent-terminal-live-p buffer) buffer)))

(defun omnigent-exec-terminal (name directory command &optional setup)
  "Show a ghostel terminal NAME running COMMAND in DIRECTORY.
COMMAND is a list of a program and its arguments, exec'd directly as the
terminal's process so quitting it closes the terminal.  A live terminal
called NAME is reused rather than started again.  SETUP, when non-nil, is
called with no arguments in the new terminal buffer before the command is
exec'd, for whatever buffer-local state the caller wants to attach.

Runs with `omnigent-environment' prepended, and hands exits to
`omnigent--on-exit'.  The shared ghostel launcher that `omnigent-terminal'
and the vanilla launchers in etc/llms-coding.el both build on."
  (if-let* ((live (omnigent--live-terminal name)))
      (pop-to-buffer live)
    (let* ((directory (file-name-as-directory
                       (expand-file-name (or directory default-directory))))
           (default-directory directory)
           (process-environment (append omnigent-environment
                                        process-environment))
           (buffer (get-buffer-create name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'ghostel-mode)
          (ghostel-mode))
        ;; Set it in the buffer, not just around its creation, so Emacs
        ;; commands run where the session does.  See
        ;; `omnigent-sync-directory' for keeping up when it moves.
        (setq default-directory directory)
        ;; Let `omnigent--on-exit' decide whether the buffer survives.
        (setq-local ghostel-kill-buffer-on-exit nil)
        (add-hook 'ghostel-exit-functions #'omnigent--on-exit nil t)
        (when setup (funcall setup)))
      (pop-to-buffer buffer)
      (ghostel-exec buffer (car command) (cdr command)))))

(defun omnigent-terminal (name directory command &optional session-id)
  "Show a ghostel terminal NAME running COMMAND for an Omnigent SESSION-ID.
Like `omnigent-exec-terminal', but also puts the terminal in
`omnigent-session-mode' and records SESSION-ID in `omnigent-session-id',
so `omnigent-dispatch' can act on the session without asking."
  (omnigent-exec-terminal
   name directory command
   (lambda ()
     (setq omnigent-session-id session-id)
     (omnigent-session-mode))))

(defun omnigent--session-buffer-name (session)
  "Return the terminal buffer name to use for SESSION."
  (let-alist session
    (format "*omnigent: %s*"
            (truncate-string-to-width (or .title .id) 40 nil nil t))))

;;;###autoload
(defun omnigent-attach (session)
  "Bring SESSION up in a terminal with `omni resume'.
Resume, not `omni attach': attaching only joins a session whose runner
is still live, while resuming hands the session to its harness either
way."
  (interactive (list (omnigent-read-session "Attach to session: ")))
  (let-alist session
    (omnigent-terminal (omnigent--session-buffer-name session) .workspace
                       (list omnigent-program "resume" .id) .id)))

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


;;; Starting a session

(defcustom omnigent-harnesses
  '(("claude" . "claude-native-ui")
    ("codex" . "codex-native-ui")
    ("pi" . "pi-native-ui"))
  "Alist of `omni' subcommand to the registered agent it launches.
`omnigent-start' creates a session bound to the agent, then hands the id
to that subcommand's `--resume'."
  :type '(alist :key-type string :value-type string))

(defun omnigent--directory (arg)
  "Return the directory to start a session in.
With prefix ARG, prompt for a directory; otherwise use the current
project root, falling back to `default-directory'."
  (let ((default (or (when-let* ((project (project-current)))
                       (project-root project))
                     default-directory)))
    (if arg
        (read-directory-name "Session directory: " default)
      default)))

(defun omnigent--project-name (directory)
  "Return the project name to file a session under for DIRECTORY.
The enclosing project's root name when DIRECTORY is inside a project,
else DIRECTORY's own name."
  (let ((root (or (when-let* ((project (project-current nil directory)))
                    (project-root project))
                  directory)))
    (file-name-nondirectory (directory-file-name root))))

;;;###autoload
(defun omnigent-start (harness &optional arg)
  "Start a session for HARNESS in a terminal, and switch to it.
HARNESS names an `omni' subcommand in `omnigent-harnesses'.  With a
prefix ARG, prompt for the directory instead of using the project root.

The session is created over the API first, with its workspace and
Omnigent project taken from the Emacs project, and only then handed to
`omni HARNESS --resume'.  So the session is filed correctly before the
harness boots, and the terminal knows its `omnigent-session-id' straight
away, which is what lets `omnigent-dispatch' act without asking.  No
title is set: Omnigent titles a session from its first message, and that
is what makes `omnigent-attach' readable."
  (interactive (list (completing-read "Harness: " omnigent-harnesses nil t)
                     current-prefix-arg))
  (let* ((agent (or (cdr (assoc harness omnigent-harnesses))
                    (user-error "No agent configured for harness %s" harness)))
         (directory (omnigent--directory arg))
         (name (omnigent--project-name directory))
         (buffer-name (format "*omni-%s[%s]*" harness name)))
    ;; Switching has to come first: creating the session further down is not
    ;; free, and a session created for a terminal we then decline to start
    ;; would sit on the server unused.
    (if-let* ((live (omnigent--live-terminal buffer-name)))
        (pop-to-buffer live)
      (let ((id (alist-get
                 'id (omnigent--request
                      "POST" "/sessions"
                      `((agent_id . ,(omnigent-agent-id agent))
                        ;; Slashless, the way `omni' itself stores a workspace.
                        (workspace . ,(directory-file-name
                                       (expand-file-name directory)))
                        (project_id . ,(omnigent-project-id name)))))))
        (omnigent-terminal buffer-name directory
                           (list omnigent-program harness "--resume" id) id)))))

(defmacro omnigent-define-start (harness)
  "Define `omnigent-HARNESS', which starts a session for HARNESS.
HARNESS is a string naming an `omni' subcommand in `omnigent-harnesses'."
  (let ((command (intern (format "omnigent-%s" harness))))
    `(progn
       ;;;###autoload
       (defun ,command (&optional arg)
         ,(format "Start or switch to an Omnigent %s session for this project.
With a prefix ARG, prompt for the directory to use instead.
See `omnigent-start', which does the work." harness)
         (interactive "P")
         (omnigent-start ,harness arg)))))

(omnigent-define-start "claude")
(omnigent-define-start "codex")
(omnigent-define-start "pi")

;;;###autoload
(defun omnigent-run (&optional arg)
  "Start or switch to an `omni run' session for this project.
With a prefix ARG, prompt for the directory to use instead.

Unlike `omnigent-start', nothing is created up front: `omni run' picks
the agent itself, so there is no id to create a session for, and the
terminal has to ask which session it is on."
  (interactive "P")
  (let ((directory (omnigent--directory arg)))
    (omnigent-terminal (format "*omni[%s]*"
                               (omnigent--project-name directory))
                       directory (list omnigent-program "run"))))


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

(defun omnigent-browse (id)
  "Open session ID in the Omnigent web UI."
  (interactive (list (omnigent--id)))
  (browse-url (format "%s/c/%s" omnigent-server-url id)))

(defun omnigent-workspace (id)
  "Return the directory session ID is working in, as the server has it."
  (when-let* ((workspace
               (alist-get 'workspace
                          (omnigent--request
                           "GET" (format "/sessions/%s?include_items=false" id)))))
    (file-name-as-directory workspace)))

(defun omnigent-sync-directory ()
  "Point this terminal's `default-directory' at its session's directory.
A session can be moved to another directory after it starts -- `omni'
offers that on attach -- so ask the server where it is now instead of
trusting the directory the terminal was launched in."
  (interactive)
  (unless omnigent-session-id
    (user-error "Not in an Omnigent terminal"))
  (let ((workspace (or (omnigent-workspace omnigent-session-id)
                       (user-error "Session %s has no directory"
                                   omnigent-session-id))))
    (setq default-directory workspace)
    (message "%s" workspace)))

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
  "Bring up an Omnigent session, or act on the current terminal's one.
Bound at `omnigent-command-prefix', and at \`C-c C-o' inside a session
terminal.  The \"Session\" commands need a session, which they take from
the current terminal or else ask for."
  [["Session"
    ("r" "Rename" omnigent-rename)
    ("k" "Archive" omnigent-archive)
    ("e" "Export transcript" omnigent-export)
    ("w" "Copy id" omnigent-copy-id)
    ("B" "Open in browser" omnigent-browse)]
   ["Go"
    ("d" "Sync directory" omnigent-sync-directory)
    ("a" "Attach to a session" omnigent-attach)
    ("b" "Switch terminal" omnigent-switch-buffer)]])


(provide 'omnigent)
;;; omnigent.el ends here
