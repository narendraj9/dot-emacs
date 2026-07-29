;;; llms-coding.el --- Launch coding agents in ghostel terminals  -*- lexical-binding: t; -*-

;;; Commentary:

;; Per-agent, per-project launchers for coding-agent CLIs (claude, pi,
;; omni, ...) running in ghostel terminals.  Each agent gets its own
;; interactive command `llms-coding-<name>' that starts a new session,
;; or switches to the existing one, for the current project.  With a
;; prefix argument the command prompts for a directory to use instead;
;; the chosen directory becomes the session key.

;;; Code:

(require 'project)
(require 'ghostel)

(defun llms-coding--session-directory (arg)
  "Return the directory to key a coding session on.
With prefix ARG, prompt for a directory; otherwise use the current
project root, falling back to `default-directory'."
  (let ((default (or (when-let* ((proj (project-current)))
                       (project-root proj))
                     default-directory)))
    (if arg
        (read-directory-name "Coding session directory: " default)
      default)))

(defun llms-coding--buffer-name (name-prefix directory)
  "Derive a ghostel buffer name for NAME-PREFIX rooted at DIRECTORY.
Uses the enclosing project's root name when DIRECTORY is inside a
project, else DIRECTORY's own name."
  (let* ((root (or (when-let* ((proj (project-current nil directory)))
                     (project-root proj))
                   directory))
         (label (file-name-nondirectory (directory-file-name root))))
    (format "*%s[%s]*" name-prefix label)))

(defun llms-coding--live-buffer (buffer-name)
  "Return the buffer named BUFFER-NAME if it hosts a live ghostel process."
  (when-let* ((buffer (get-buffer buffer-name))
              (proc (buffer-local-value 'ghostel--process buffer)))
    (and (process-live-p proc) buffer)))

(defun llms-coding--on-exit (buffer event)
  "Kill BUFFER on a clean agent exit; keep it if the agent failed.
EVENT is the process sentinel string.  Keeping the buffer on failure
leaves any startup error or traceback on screen instead of the buffer
vanishing (see `ghostel-exit-functions')."
  (when (string-prefix-p "finished" event)
    (kill-buffer buffer)))

(defun llms-coding--launch (command name-prefix arg)
  "Start or switch to a ghostel session running COMMAND.
NAME-PREFIX names the agent; ARG is the raw prefix argument.  Reuses a
live session for the resolved directory.  COMMAND is exec'd directly as
the terminal's process (no wrapping shell), so quitting the agent closes
the terminal.  A clean exit kills the buffer; a failed launch leaves it
visible so the error can be read."
  (let* ((directory (llms-coding--session-directory arg))
         (buffer-name (llms-coding--buffer-name name-prefix directory))
         (existing (llms-coding--live-buffer buffer-name)))
    (if existing
        (pop-to-buffer existing)
      (let* ((default-directory directory)
             (buffer (get-buffer-create buffer-name))
             (words (split-string-shell-command command)))
        (with-current-buffer buffer
          (unless (derived-mode-p 'ghostel-mode)
            (ghostel-mode))
          ;; Let our exit function decide whether to kill the buffer.
          (setq-local ghostel-kill-buffer-on-exit nil)
          (add-hook 'ghostel-exit-functions #'llms-coding--on-exit nil t))
        (pop-to-buffer buffer)
        (ghostel-exec buffer (car words) (cdr words))))))

(defmacro llms-coding-define (name command)
  "Define an interactive command `llms-coding-NAME' running COMMAND.
NAME is an unquoted symbol; COMMAND is a shell command string."
  (let ((fn (intern (format "llms-coding-%s" name))))
    `(progn
       ;;;###autoload
       (defun ,fn (&optional arg)
         ,(format "Start or switch to a %s coding session in ghostel.
With a prefix ARG, prompt for the directory to use." name)
         (interactive "P")
         (llms-coding--launch ,command ,(symbol-name name) arg)))))

(llms-coding-define claude "omni claude")
(llms-coding-define codex "omni codex")
(llms-coding-define pi "omni pi")
(llms-coding-define omni "omni run")

(provide 'llms-coding)
;;; llms-coding.el ends here
