;;; llms-coding.el --- Bind coding-agent keys to a launcher  -*- lexical-binding: t; -*-

;;; Commentary:

;; One indirection between my key bindings and whatever starts my coding
;; agents.  The bindings in init.el name `llms-coding-<agent>'; the
;; commands here decide what that actually runs.
;;
;; Two families, both riding on the same ghostel plumbing in omnigent.el
;; (`omnigent-exec-terminal' and the directory/project helpers):
;;
;; - `llms-coding-<agent>' runs the plain CLI in a terminal.
;; - `llms-coding-omni-<agent>' wraps that agent in an Omnigent session:
;;   `omnigent-start' creates it over the API, filed under the current
;;   project, then boots the harness onto it.  `llms-coding-omni' is the
;;   exception, since `omni run' picks its own agent.

;;; Code:

(require 'omnigent)
(require 'subr-x)

;;; Omni server selection

;; `llms-coding-omni-server', when bound (e.g. in a private init file), is
;; the URL of a remote Omnigent server to connect to.  Left unbound,
;; llms-coding starts a local Omni server and talks to it on the default
;; localhost URL (`omnigent-server-url').
(defvar llms-coding-omni-server)

(defcustom llms-coding-omni-server-start-timeout 15
  "Seconds to wait for a freshly started local Omni server to accept requests."
  :type 'natnum
  :group 'llms-coding)

(defvar llms-coding-omni--server-ensured nil
  "Non-nil once the local Omni server was started or confirmed this session.")

(defun llms-coding-omni--server-running-p ()
  "Return non-nil if the local Omni background server is up."
  (with-temp-buffer
    (and (zerop (call-process omnigent-program nil t nil
                              "server" "status" "--json"))
         (progn
           (goto-char (point-min))
           (eq t (ignore-errors
                   (alist-get 'running
                              (json-parse-buffer :object-type 'alist
                                                 :null-object nil))))))))

(defun llms-coding-omni--start-local-server ()
  "Start the local Omni background server and wait until it accepts requests."
  (message "Starting local Omni server...")
  (with-temp-buffer
    (unless (zerop (call-process omnigent-program nil t nil
                                 "server" "--background"))
      (user-error "Could not start local Omni server: %s"
                  (string-trim (buffer-string)))))
  (let ((deadline (+ (float-time) llms-coding-omni-server-start-timeout)))
    (while (and (not (llms-coding-omni--server-running-p))
                (< (float-time) deadline))
      (sleep-for 0.2))
    (unless (llms-coding-omni--server-running-p)
      (user-error "Local Omni server did not come up within %ss"
                  llms-coding-omni-server-start-timeout))
    (message "Local Omni server is up at %s" omnigent-server-url)))

(defun llms-coding-omni-ensure-server (&rest _)
  "Point Omnigent at the server to use, starting a local one when needed.
When `llms-coding-omni-server' is bound, connect to that URL and start
nothing.  Otherwise start a local Omni server once and keep the default
localhost `omnigent-server-url'.  Wired onto `omnigent--request', so it
runs before every Omnigent API call."
  (if (boundp 'llms-coding-omni-server)
      (setq omnigent-server-url llms-coding-omni-server)
    (unless llms-coding-omni--server-ensured
      (unless (llms-coding-omni--server-running-p)
        (llms-coding-omni--start-local-server))
      (setq llms-coding-omni--server-ensured t))))

(advice-add 'omnigent--request :before #'llms-coding-omni-ensure-server)

;;; Omni-wrapped agents

;;;###autoload (defalias 'llms-coding-omni-claude #'omnigent-claude)
(defalias 'llms-coding-omni-claude #'omnigent-claude)
;;;###autoload (defalias 'llms-coding-omni-codex #'omnigent-codex)
(defalias 'llms-coding-omni-codex #'omnigent-codex)
;;;###autoload (defalias 'llms-coding-omni-pi #'omnigent-pi)
(defalias 'llms-coding-omni-pi #'omnigent-pi)
;;;###autoload (defalias 'llms-coding-omni #'omnigent-run)
(defalias 'llms-coding-omni #'omnigent-run)

;;; Plain agents

(defgroup llms-coding nil
  "Launch coding-agent CLIs from Emacs."
  :group 'tools
  :prefix "llms-coding-")

(defcustom llms-coding-agents
  '(("claude" . ("claude"))
    ("codex" . ("codex"))
    ("pi" . ("pi")))
  "Alist of plain coding-agent names to argv lists.
The car is the agent name used for completion and buffer naming; the cdr
is exec'd directly as a ghostel terminal's PROGRAM plus ARGS."
  :type '(alist :key-type string :value-type (repeat string)))

;;;###autoload
(defun llms-coding (agent &optional arg)
  "Start or switch to a plain coding AGENT for this project.
With a prefix ARG, prompt for the directory to use instead.

Reuses omnigent.el's ghostel plumbing: the session-less launcher
`omnigent-exec-terminal', with the same project-root/prefix-arg
resolution as the Omnigent wrappers (`omnigent--directory' and
`omnigent--project-name')."
  (interactive (list (completing-read "Agent: " llms-coding-agents nil t)
                     current-prefix-arg))
  (let* ((command (or (cdr (assoc agent llms-coding-agents))
                      (user-error "No agent configured for %s" agent)))
         (directory (omnigent--directory arg))
         (name (omnigent--project-name directory))
         (buffer-name (format "*%s[%s]*" agent name)))
    (omnigent-exec-terminal buffer-name directory command)))

(defmacro llms-coding-define (agent)
  "Define `llms-coding-AGENT', which starts the plain AGENT CLI."
  (let ((command (intern (format "llms-coding-%s" agent))))
    `(progn
       ;;;###autoload
       (defun ,command (&optional arg)
         ,(format "Start or switch to a plain %s session for this project.
With a prefix ARG, prompt for the directory to use instead.
See `llms-coding', which does the work." agent)
         (interactive "P")
         (llms-coding ,agent arg)))))

(llms-coding-define "claude")
(llms-coding-define "codex")
(llms-coding-define "pi")

(provide 'llms-coding)
;;; llms-coding.el ends here
