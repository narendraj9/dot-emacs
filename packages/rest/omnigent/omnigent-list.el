;;; omnigent-list.el --- Running Omnigent sessions  -*- lexical-binding: t; -*-

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A `list-processes' for Omnigent: one line per session this machine is
;; running, with the background server and the host daemon on the header
;; line.
;;
;; A stored session costs nothing.  A running one holds a runner process,
;; a tmux pane, a vendor CLI and its MCP fleet.  The two are told apart by
;; joining `/sessions' against `/runners': a session is running when its
;; runner is one the server reports online.  The stored rest, usually most
;; of them, stays out of the way.
;;
;; The data all comes from the API.  The CLI reports the server and the
;; daemon in prose meant for a human (`omni server status', `omni host
;; status') and says nothing about runners or terminals, so there is
;; nothing here to parse it for.  The one CLI call left is what `k' runs:
;; `omni host stop-session' frees a session's resources, where the API's
;; DELETE would take the transcript with it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'omnigent)

(defconst omnigent-list--session-limit 1000
  "How many stored sessions to scan for running ones.
The API maximum.  A running session can have been quiet for days, so
looking only at the `omnigent-session-limit' most recent ones would miss
it -- and missing one defeats the point of the list.")


;;; Reading the state

(defun omnigent-list--true-p (value)
  "Return non-nil when VALUE is JSON true.
`json-parse-buffer' leaves JSON false as `:false', which is non-nil in
Emacs Lisp, so a plain `alist-get' test reads every flag as true."
  (eq value t))

(defun omnigent-list--online-runners (runners)
  "Return the ids of the online runners among RUNNERS.
RUNNERS is the list under `data' in the `/runners' reply."
  (thread-last runners
               (seq-filter (lambda (runner)
                             (omnigent-list--true-p (alist-get 'online runner))))
               (mapcar (lambda (runner) (alist-get 'runner_id runner)))))

(defun omnigent-list--running-sessions (sessions runners)
  "Return the SESSIONS whose runner is online in RUNNERS.
SESSIONS and RUNNERS are the parsed `/sessions' and `/runners' lists.
A session keeps its last `runner_id' after that runner is gone, so it is
the online set, not the field's presence, that marks one as running."
  (let ((online (omnigent-list--online-runners runners)))
    (seq-filter (lambda (session)
                  (member (alist-get 'runner_id session) online))
                sessions)))

(defun omnigent-list--terminals (id)
  "Return the terminal resources of session ID."
  (alist-get 'data (omnigent--request
                    "GET" (format "/sessions/%s/resources/terminals" id))))

(defun omnigent-list--session-buffer (id)
  "Return the live Omnigent terminal running session ID, if there is one."
  (seq-find (lambda (buffer)
              (equal (buffer-local-value 'omnigent-session-id buffer) id))
            (omnigent-buffers)))


;;; The header line

(defun omnigent-list--daemon ()
  "Return the local host daemon's own record of itself, or nil.
The daemon writes one JSON file per server it serves under
`omnigent-data-directory'; the one naming our server is ours."
  (let ((files (file-expand-wildcards
                (expand-file-name "daemons/*.json" omnigent-data-directory))))
    (thread-last files
                 (mapcar (lambda (file)
                           (ignore-errors
                             (with-temp-buffer
                               (insert-file-contents file)
                               (json-parse-buffer :object-type 'alist
                                                  :array-type 'list
                                                  :null-object nil)))))
                 (delq nil)
                 (seq-find (lambda (daemon)
                             (equal (alist-get 'resolved_server_url daemon)
                                    omnigent-server-url))))))

(defun omnigent-list--server-pid ()
  "Return the background server's pid as it recorded it, or nil.
Its pid file holds the pid on the first line and the port on the second.
No liveness check: the table could not have been built at all if the
server were not answering."
  (let ((file (expand-file-name "local_server.pid" omnigent-data-directory)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (car (split-string (buffer-string) "\n" t))))))

(defun omnigent-list--host (host-id)
  "Return the `/hosts' entry for HOST-ID, or nil.
The daemon records its host as \"host_ID\" while the API keys on the
bare ID, so the prefix comes off before matching."
  (let ((id (string-remove-prefix "host_" (or host-id ""))))
    (seq-find (lambda (host) (equal (alist-get 'host_id host) id))
              (alist-get 'hosts (omnigent--request "GET" "/hosts")))))

(defun omnigent-list--header (sessions)
  "Return the header line describing what is behind SESSIONS.
A part whose source is missing is dropped, not guessed at."
  (let* ((daemon (omnigent-list--daemon))
         (host (omnigent-list--host (alist-get 'host_id daemon)))
         (server-pid (omnigent-list--server-pid)))
    (string-join
     (delq nil
           (list (concat "server " omnigent-server-url
                         (and server-pid (format " (pid %s)" server-pid)))
                 (when daemon
                   (let-alist daemon
                     (format "host %s %s (pid %s, %s)"
                             (or (alist-get 'name host) .host_id)
                             (or (alist-get 'status host) "?")
                             .pid .mode)))
                 (format "%d running" (length sessions))))
     "  |  ")))


;;; The list

(defun omnigent-list--status (status)
  "Return STATUS coloured by what it says about the session's agent."
  (propertize (or status "?") 'face
              (pcase status
                ("running" 'success)
                ("failed" 'error)
                (_ 'shadow))))

(defun omnigent-list--terminal-names (terminals)
  "Return the names of TERMINALS, dimming the ones no longer running."
  (if (null terminals)
      (propertize "-" 'face 'shadow)
    (mapconcat (lambda (terminal)
                 (let-alist terminal
                   (if (omnigent-list--true-p .metadata.running)
                       .name
                     (propertize .name 'face 'shadow))))
               terminals ",")))

(defun omnigent-list--columns (session terminals)
  "Return the table columns for SESSION and its TERMINALS."
  (let-alist session
    (vector (omnigent-list--status .status)
            (or .agent_name "")
            (omnigent-list--terminal-names terminals)
            (if (omnigent-list--session-buffer .id) "*" "")
            (if .workspace (abbreviate-file-name .workspace) "")
            (or .title "(untitled)"))))

(defun omnigent-list--entries ()
  "Return `tabulated-list-entries' for the sessions running here.
An entry's id is a cons of the session and its terminals, so a command
acting on a line needs no request of its own."
  ;; ponytail: one request per running session for its terminals.  Fine
  ;; at the handful a machine runs; batch it if that grows.
  (mapcar (lambda (session)
            (let ((terminals (omnigent-list--terminals (alist-get 'id session))))
              (list (cons session terminals)
                    (omnigent-list--columns session terminals))))
          (omnigent-list--running-sessions
           (omnigent-sessions omnigent-list--session-limit)
           (alist-get 'data (omnigent--request "GET" "/runners")))))

(defun omnigent-list--refresh ()
  "Fill this buffer's entries and header line from the server."
  (setq tabulated-list-entries (omnigent-list--entries))
  (setq header-line-format
        (omnigent-list--header (mapcar #'car tabulated-list-entries))))


;;; Acting on a line

(defun omnigent-list--entry ()
  "Return the (SESSION . TERMINALS) of the line at point."
  (or (tabulated-list-get-id) (user-error "No session on this line")))

(defun omnigent-list--session ()
  "Return the session of the line at point."
  (car (omnigent-list--entry)))

(defun omnigent-list--read-terminal (terminals)
  "Read one of TERMINALS by name."
  (let ((table (mapcar (lambda (terminal)
                         (cons (alist-get 'name terminal) terminal))
                       terminals)))
    (cdr (assoc (completing-read "Terminal: " table nil t) table))))

(defun omnigent-list-visit (session)
  "Switch to SESSION's terminal, attaching it when none is live."
  (interactive (list (omnigent-list--session)) omnigent-list-mode)
  (if-let* ((buffer (omnigent-list--session-buffer (alist-get 'id session))))
      (pop-to-buffer buffer)
    (omnigent-attach session)))

(defun omnigent-list-stop (session)
  "Stop SESSION, freeing its runner, terminals and vendor CLI.
Runs `omni host stop-session', the supported teardown: the API's DELETE
would drop the conversation and its transcript too.  The session stays
stored, and `omnigent-attach' resumes it."
  (interactive (list (omnigent-list--session)) omnigent-list-mode)
  (let-alist session
    (when (yes-or-no-p (format "Stop session %s? " (or .title .id)))
      (with-temp-buffer
        (unless (zerop (call-process omnigent-program nil t nil
                                     "host" "stop-session" .id))
          (user-error "%s" (string-trim (buffer-string)))))
      (message "Stopped %s" (or .title .id))
      (revert-buffer))))

(defun omnigent-list-close-terminal (session terminal)
  "Close SESSION's TERMINAL, killing its tmux pane and what runs in it.
The session and its transcript stay.  Omnigent recreates the pane on the
next message, and the vendor CLI resumes itself."
  (interactive
   (let ((terminals (cdr (omnigent-list--entry))))
     (list (omnigent-list--session)
           (pcase terminals
             ('nil (user-error "No terminals on this session"))
             (`(,only) only)
             (_ (omnigent-list--read-terminal terminals)))))
   omnigent-list-mode)
  (let ((id (alist-get 'id session))
        (name (alist-get 'name terminal)))
    (when (yes-or-no-p (format "Close terminal %s? " name))
      (omnigent--request "DELETE" (format "/sessions/%s/resources/terminals/%s"
                                          id (alist-get 'id terminal)))
      (message "Closed %s" name)
      (revert-buffer))))

(defun omnigent-list-browse ()
  "Open the session at point in the Omnigent web UI."
  (interactive nil omnigent-list-mode)
  (omnigent-browse (alist-get 'id (omnigent-list--session))))

(defun omnigent-list-copy-id ()
  "Copy the id of the session at point to the kill ring."
  (interactive nil omnigent-list-mode)
  (omnigent-copy-id (alist-get 'id (omnigent-list--session))))


;;; The mode

(defvar-keymap omnigent-list-mode-map
  :doc "Keymap for `omnigent-list-mode'."
  :parent tabulated-list-mode-map
  "RET" #'omnigent-list-visit
  "k" #'omnigent-list-stop
  "K" #'omnigent-list-close-terminal
  "w" #'omnigent-list-browse
  "c" #'omnigent-list-copy-id)

(define-derived-mode omnigent-list-mode tabulated-list-mode "Omnigent"
  "Major mode for the list of Omnigent sessions running on this machine.

\\{omnigent-list-mode-map}"
  (setq tabulated-list-format
        [("Status" 8 t)
         ("Agent" 17 t)
         ("Terminal" 14 t)
         ;; A `*' here means an Emacs terminal is attached to the session.
         ("E" 1 t)
         ("Workspace" 30 t)
         ("Title" 0 t)]
        tabulated-list-padding 2
        ;; No sort key: the server hands sessions back most recently
        ;; active first, which is the order to see them in.
        tabulated-list-sort-key nil
        ;; The column names go in the buffer's first line, leaving the
        ;; header line for `omnigent-list--header'.
        tabulated-list-use-header-line nil)
  (add-hook 'tabulated-list-revert-hook #'omnigent-list--refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun omnigent-list-sessions ()
  "List the Omnigent sessions running on this machine.
One line per session holding a runner, with the server and host daemon
on the header line.  See `omnigent-list-mode' for what the keys do."
  (interactive)
  (let ((buffer (get-buffer-create "*Omnigent sessions*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'omnigent-list-mode)
        (omnigent-list-mode))
      (omnigent-list--refresh)
      (tabulated-list-print t t))
    (pop-to-buffer buffer)))


;;; Checks

(defun omnigent-list--self-check ()
  "Check this file's own logic against fixtures, signalling on failure.
The runner join and the column formatting are the only logic here that
is not a straight read of the API."
  (interactive)
  (let ((sessions '(((id . "running") (runner_id . "r1"))
                    ((id . "stale") (runner_id . "r2"))
                    ((id . "never") (runner_id . nil))))
        (runners '(((runner_id . "r1") (online . t))
                   ((runner_id . "r2") (online . :false)))))
    (cl-assert (equal '("running")
                      (mapcar (lambda (session) (alist-get 'id session))
                              (omnigent-list--running-sessions sessions runners)))
               t "only a session on an online runner is running"))
  (cl-assert (omnigent-list--true-p t) t)
  (cl-assert (not (omnigent-list--true-p :false)) t "JSON false is not true")
  (let* ((terminals '(((name . "claude:main") (metadata (running . t)))
                      ((name . "shell:1") (metadata (running . :false)))))
         (names (omnigent-list--terminal-names terminals)))
    (cl-assert (equal "claude:main,shell:1" (substring-no-properties names)) t)
    (cl-assert (null (get-text-property 0 'face names))
               t "a running terminal is not dimmed")
    (cl-assert (eq 'shadow (get-text-property (string-search "shell" names)
                                              'face names))
               t "a stopped terminal is dimmed"))
  (let ((columns (omnigent-list--columns '((id . "x")) nil)))
    (cl-assert (equal "?" (substring-no-properties (aref columns 0)))
               t "a session with no status shows one anyway")
    (cl-assert (equal "(untitled)" (aref columns 5))
               t "a session with no title shows one anyway"))
  (message "omnigent-list: checks passed"))

(provide 'omnigent-list)
;;; omnigent-list.el ends here
