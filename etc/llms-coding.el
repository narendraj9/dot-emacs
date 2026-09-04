;;; llms-coding.el --- Launch coding agents in ghostel terminals  -*- lexical-binding: t; -*-

;;; Commentary:

;; Per-agent, per-project launchers for coding-agent CLIs.  Each agent
;; gets its own interactive command `llms-coding-<name>' that starts a
;; new session, or switches to the existing one, for the current
;; project.  With a prefix argument the command prompts for a directory
;; to use instead; the chosen directory becomes the session key.
;;
;; Every agent currently runs through omnigent (`omni claude', ...).
;; That mapping lives in one place, at the bottom of this file, so
;; pointing an agent at a different CLI is a one-line change.

;;; Code:

(require 'omnigent)
(require 'project)

(defun llms-coding--directory (arg)
  "Return the directory to key a coding session on.
With prefix ARG, prompt for a directory; otherwise use the current
project root, falling back to `default-directory'."
  (let ((default (or (when-let* ((project (project-current)))
                       (project-root project))
                     default-directory)))
    (if arg
        (read-directory-name "Coding session directory: " default)
      default)))

(defun llms-coding--buffer-name (name directory)
  "Return the terminal buffer name for agent NAME rooted at DIRECTORY.
Uses the enclosing project's root name when DIRECTORY is inside a
project, else DIRECTORY's own name."
  (let ((root (or (when-let* ((project (project-current nil directory)))
                    (project-root project))
                  directory)))
    (format "*%s[%s]*" name
            (file-name-nondirectory (directory-file-name root)))))

(defmacro llms-coding-define (name command)
  "Define an interactive command `llms-coding-NAME' running COMMAND.
NAME is an unquoted symbol; COMMAND is a shell command string.  The
terminal itself comes from `omnigent-terminal', which reuses a live
session for the same agent and directory."
  (let ((function (intern (format "llms-coding-%s" name))))
    `(progn
       ;;;###autoload
       (defun ,function (&optional arg)
         ,(format "Start or switch to a %s coding session in ghostel.
Runs `%s'.  With a prefix ARG, prompt for the directory to use instead."
                  name command)
         (interactive "P")
         (let ((directory (llms-coding--directory arg)))
           (omnigent-terminal
            (llms-coding--buffer-name ,(symbol-name name) directory)
            directory
            (split-string-shell-command ,command)))))))

(llms-coding-define claude "omni claude")
(llms-coding-define codex "omni codex")
(llms-coding-define pi "omni pi")
(llms-coding-define omni "omni run")

(provide 'llms-coding)
;;; llms-coding.el ends here
