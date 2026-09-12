;;; omnigent.el --- Omnigent sessions from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Narendra Joshi

;; Author: Narendra Joshi <narendra.joshi@grammarly.com>
;; Keywords: processes, tools
;; Package-Requires: ((emacs "29.1") (ghostel "0") (request "0.3.2"))

;;; Commentary:

;; Start an Omnigent session, or reattach to one, in a terminal inside
;; Emacs, and act on it from a `transient' menu.
;;
;; Session state comes from the Omnigent server's HTTP API
;; (`omnigent-server-url').  Terminals are `omni' processes run by
;; ghostel.
;;
;; Entry points:
;;
;;   `omnigent-mode'           global minor mode; opens the menu on
;;                             `omnigent-command-prefix'
;;   `omnigent-start'          create a session for a harness, filed
;;                             under the current project, and boot the
;;                             harness onto it
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

(defcustom omnigent-data-directory "~/.omnigent"
  "Directory Omnigent keeps its state in on this machine.
`omnigent-list-sessions' reads the server's and the daemon's own records
of themselves from here."
  :type 'directory)

(defcustom omnigent-environment
  '("OMNIGENT_RUNNER_ENV_PASSTHROUGH=OMNIGENT_NATIVE_PANE_IDLE_TIMEOUT_S,OMNIGENT_HARNESS_IDLE_TIMEOUT_S"
    "OMNIGENT_NATIVE_PANE_IDLE_TIMEOUT_S=172800"
    "OMNIGENT_HARNESS_IDLE_TIMEOUT_S=0")
  "Environment entries prepended to `process-environment' for terminals.
They set Omnigent's two idle reapers, which otherwise kill an idle
native pane or harness subprocess after an hour.  Values are seconds;
`0' disables a reaper.

The pane here keeps for two days.  That reaper spares a pane with an
attached tmux client, an in-flight turn, or a working CLI, and it tears
down the pane alone -- the session and its transcript stay, and
`omnigent-attach' resumes them.

Each setting crosses two allowlists on its way down:

- CLI to daemon allows the `OMNIGENT_' prefix, so all three pass
  (`_build_host_daemon_env' in omnigent/cli.py).
- Daemon to runner does not, so the two timeouts pass only because
  `OMNIGENT_RUNNER_ENV_PASSTHROUGH' names them (`_build_runner_env' in
  omnigent/host/connect.py).

A change takes effect on the next cold daemon start.  The first `omni'
command that finds no live daemon spawns one; the rest reuse it."
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
  "Set SYMBOL to PREFIX, and bind `omnigent-dispatch' at PREFIX."
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
  (let* (;; Failures are reported below; request.el's own messages would
         ;; only duplicate them.
         (request-message-level -1)
         (response
          (request (concat omnigent-server-url "/v1" path)
            ;; Sync: `omnigent-read-session' runs in an `interactive'
            ;; form, which cannot await.  The wait polls with
            ;; `accept-process-output', so C-g still works, and five
            ;; seconds is long for a local server.
            :sync t
            :timeout 5
            :type method
            :headers (and body '(("Content-Type" . "application/json")))
            :data (and body (json-serialize body))
            :parser (lambda ()
                      (json-parse-buffer :object-type 'alist :array-type 'list
                                         :null-object nil)))))
    (unless (<= 200 (or (request-response-status-code response) 0) 299)
      (user-error "Omnigent[%s] %s %s: %s" omnigent-server-url method path
                  (or (request-response-status-code response)
                      ;; No status code at all: the request never landed.
                      (format "%s unreachable" omnigent-server-url))))
    (request-response-data response)))

(defun omnigent-sessions (&optional limit)
  "Return the recent unarchived sessions, most recently active first.
LIMIT caps how many the server returns, defaulting to
`omnigent-session-limit'."
  (alist-get 'data
             (omnigent--request
              "GET" (format "/sessions?limit=%d&sort_by=updated_at&order=desc"
                            (or limit omnigent-session-limit)))))

(defun omnigent--find-named (path name)
  "Return the entry in the list the API serves at PATH whose name is NAME."
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
Liveness is the buffer's own ghostel process: ghostel talks to a pty
connection rather than a subprocess, so `process-command' is nil and
there is no command line to match on.  Shared with the vanilla launchers
in etc/llms-coding.el."
  (and (buffer-live-p buffer)
       (when-let* ((process (buffer-local-value 'ghostel--process buffer)))
         (process-live-p process))))

(defun omnigent-buffer-p (buffer)
  "Return non-nil if BUFFER is a live ghostel terminal running Omnigent.
That is an `omnigent-session-id' on top of a live ghostel process."
  (and (buffer-local-value 'omnigent-session-id buffer)
       (omnigent-terminal-live-p buffer)))

(defun omnigent-buffers ()
  "Return the live Omnigent terminals, most recently used first."
  (seq-filter #'omnigent-buffer-p (buffer-list)))

(defun omnigent--exec (buffer command)
  "In BUFFER, run COMMAND as its terminal process.
COMMAND is a list of a program and its arguments.  `omnigent-environment'
is bound here, not around the buffer's creation, so a relaunch gets it
too."
  (with-current-buffer buffer
    (let ((process-environment (append omnigent-environment
                                       process-environment)))
      (ghostel-exec buffer (car command) (cdr command)))))


;;; Surviving a stale runner binding

(defconst omnigent--stale-runner-error " is offline for conversation "
  "Fragment of the server error that says a session's runner is gone.
`omni' matches on the same fragment (`_is_stale_runner_message' in its
codex_native.py).")

(defcustom omnigent-stale-runner-retry-delay 2
  "Seconds to wait before relaunching a terminal that hit a stale runner.
Nil never retries, leaving the error on screen.

Resuming a session, `omni' looks that session's terminal up first.  The
lookup fails with a 400 when the runner is gone while this machine still
counts as online -- a tunnel that dropped without the host record
catching up, after sleep or a daemon being replaced.  The Claude wrapper
recovers from the 404, 409, 502 and 503 the same lookup can return, but
not from the 400.

The condition clears as soon as the tunnel re-registers or the host
record goes offline, so the relaunch takes the ordinary cold-resume
path.  The web UI never trips on it: it skips the lookup and leaves
rebinding to the server, which does it on the next message."
  :type '(choice (const :tag "Do not retry" nil) number))

(defvar-local omnigent--command nil
  "The command this terminal ran, for relaunching it after a failure.")

(defvar-local omnigent--retried nil
  "Non-nil once this terminal has been relaunched after a stale runner.
One retry only: a second failure is the real thing, and belongs on
screen.")

(defun omnigent--stale-runner-failure-p (buffer)
  "Return non-nil if BUFFER's terminal died on a stale runner binding.
Reads the buffer's tail, where `omni' leaves the error before exiting."
  ;; ponytail: ghostel renders into a buffer only while it has a window,
  ;; which a terminal you just launched has.  One hidden for its whole
  ;; short life renders nothing and gets no retry.  Ask the server
  ;; instead -- `GET /sessions/ID/resources' returns the same 400 -- if
  ;; that stops holding.
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (and (search-backward omnigent--stale-runner-error
                            (max (point-min) (- (point-max) 4000)) t)
           t))))

(defun omnigent--retry-stale-runner (buffer)
  "Relaunch BUFFER's command once `omnigent-stale-runner-retry-delay' is up.
The wait is the point, and re-execing from `ghostel-exit-functions'
would race ghostel's teardown of the old process."
  (with-current-buffer buffer
    (setq omnigent--retried t)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n[omnigent] runner was offline; relaunching...\n")))
  ;; The relaunch erases the buffer, so leave the reason where it lasts.
  (message "omnigent: %s hit an offline runner; relaunching" (buffer-name buffer))
  (run-at-time
   omnigent-stale-runner-retry-delay nil
   (lambda ()
     (when (and (buffer-live-p buffer)
                (not (omnigent-terminal-live-p buffer)))
       (omnigent--exec buffer (buffer-local-value 'omnigent--command buffer))))))

(defun omnigent--on-exit (buffer event)
  "Hand BUFFER's exit to `omnigent--handle-exit', once it is rendered.
EVENT is the process sentinel string.  See `ghostel-exit-functions'.
Deferred because the decision reads the terminal's last output, and the
sentinel can beat ghostel's final redraw."
  (run-at-time 0.3 nil #'omnigent--handle-exit buffer event))

(defun omnigent--handle-exit (buffer event)
  "Retry BUFFER, note EVENT in it, or kill it after a clean exit.
EVENT is the process sentinel string.  See `omnigent-keep-buffer-on-exit'
and `omnigent-stale-runner-retry-delay'."
  (cond
   ((not (buffer-live-p buffer)))
   ((and omnigent-stale-runner-retry-delay
         (not (buffer-local-value 'omnigent--retried buffer))
         (buffer-local-value 'omnigent--command buffer)
         (omnigent--stale-runner-failure-p buffer))
    (omnigent--retry-stale-runner buffer))
   ((and (not omnigent-keep-buffer-on-exit)
         (string-prefix-p "finished" event))
    (kill-buffer buffer))
   (t
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n[omnigent] process exited: %s (at %s)\n"
                        (string-trim event)
                        (format-time-string "%F %T"))))))))

(defun omnigent--live-terminal (name)
  "Return the live ghostel terminal called NAME, if there is one.
Matches on liveness alone, so it finds the vanilla terminals in
etc/llms-coding.el too, which carry no session id."
  (when-let* ((buffer (get-buffer name)))
    (and (omnigent-terminal-live-p buffer) buffer)))

(defun omnigent-exec-terminal (name directory command &optional setup)
  "Show a ghostel terminal NAME in DIRECTORY, running COMMAND.
COMMAND is a list of a program and its arguments, exec'd directly as the
terminal's process, so quitting it closes the terminal.  A live terminal
called NAME is reused rather than started again.  SETUP, when non-nil,
runs in the new buffer before the exec, for whatever buffer-local state
the caller wants to attach.

Exits go to `omnigent--on-exit'.  The shared launcher that
`omnigent-terminal' and the vanilla ones in etc/llms-coding.el build on."
  (if-let* ((live (omnigent--live-terminal name)))
      (pop-to-buffer live)
    (let* ((directory (file-name-as-directory
                       (expand-file-name (or directory default-directory))))
           (default-directory directory)
           (buffer (get-buffer-create name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'ghostel-mode)
          (ghostel-mode))
        ;; In the buffer, not just around its creation, so Emacs
        ;; commands run where the session does.
        ;; `omnigent-sync-directory' keeps up when it moves.
        (setq default-directory directory)
        ;; Let `omnigent--on-exit' decide whether the buffer survives.
        (setq-local ghostel-kill-buffer-on-exit nil)
        (setq omnigent--command command
              omnigent--retried nil)
        (add-hook 'ghostel-exit-functions #'omnigent--on-exit nil t)
        (when setup (funcall setup)))
      (pop-to-buffer buffer)
      (omnigent--exec buffer command))))

(defun omnigent-terminal (name directory command &optional session-id)
  "Show a ghostel terminal NAME in DIRECTORY running COMMAND, for SESSION-ID.
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
is still live, while resuming hands it to its harness either way."
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
    ("pi" . "pi-native-ui")
    ("agy" . "antigravity-native-ui"))
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

The session is created over the API first -- workspace and Omnigent
project taken from the Emacs project -- and only then handed to
`omni HARNESS --resume'.  Two things follow: it is filed correctly
before the harness boots, and the terminal knows its
`omnigent-session-id' straight away, which is what lets
`omnigent-dispatch' act without asking.  No title is set, because
Omnigent titles a session from its first message."
  (interactive (list (completing-read "Harness: " omnigent-harnesses nil t)
                     current-prefix-arg))
  (let* ((agent (or (cdr (assoc harness omnigent-harnesses))
                    (user-error "No agent configured for harness %s" harness)))
         (directory (omnigent--directory arg))
         (name (omnigent--project-name directory))
         (buffer-name (format "*omni-%s[%s]*" harness name)))
    ;; Switch first: a session created for a terminal we then decline to
    ;; start would sit on the server unused.
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
(omnigent-define-start "agy")

;;;###autoload
(defun omnigent-run (&optional arg)
  "Start or switch to an `omni run' session for this project.
With a prefix ARG, prompt for the directory to use instead.

Unlike `omnigent-start', nothing is created up front: `omni run' picks
the agent itself, so there is no id to bind, and the terminal has to ask
which session it is on."
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
  "Set session ID's FIELD to VALUE, and report it."
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
A session can move after it starts -- `omni' offers that on attach -- so
ask the server where it is now rather than trust the launch directory."
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

;; Loaded on demand, so its requires cost nothing until asked for.
;; Spelled out as well as cookied: this package is loaded from a
;; directory, with no generated autoloads.
;;;###autoload (autoload 'omnigent-list-sessions "omnigent-list" nil t)
(autoload 'omnigent-list-sessions "omnigent-list" nil t)

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
    ("b" "Switch terminal" omnigent-switch-buffer)
    ("l" "List running sessions" omnigent-list-sessions)]])


(provide 'omnigent)
;;; omnigent.el ends here
