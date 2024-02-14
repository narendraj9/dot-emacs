;;; connected-repl.el --- Launch and interact with a REPL connected to a buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defcustom connected-repl-commands
  '(
    ;; For a Maven project, check this out:
    ;; https://github.com/johnpoth/jshell-maven-plugin
    (java-mode . ("jshell> " "jshell" "-q"))

    (rust-ts-mode . (">> "  "evcxr"))
    (rust-mode . (">> "  "evcxr"))

    (python-ts-mode . (">>> " "python3")) )
  "Configuration of commands to start REPLs, value is an association list.

  Each item of the association list follows the following format:

  (major-mode . (\"prompt-regexp\" \"command\" \"arg1\" \"arg2\"...))

  where arg1, arg2, ... argN represent arguments to command to
  start a REPL session.")

(defvar connected-repl--buffer)
(make-local-variable 'connected-repl--buffer)

(defvar connected-repl--last-output "")
(make-local-variable 'connected-repl--last-output)

(defvar connected-repl-prompt-regexp)

(defun connected-repl-collect-output (s)
  (setq connected-repl--last-output
        (concat connected-repl--last-output s)))


(defun connected-repl-add-local-bindings ()
  (let ((m (current-local-map)))
    (define-key m (kbd "C-c C-k") #'connected-repl-send-region)
    (define-key m (kbd "C-x C-e") #'connected-repl-send-statement)
    (define-key m (kbd "C-c C-z") #'connected-repl-display-buffer)))


(define-derived-mode connected-repl-mode comint-mode "connected-repl"
  "A major-mode for running a REPL for `'major-mode' using
  `comint-mode'."
  (setq comint-prompt-regexp connected-repl-prompt-regexp)
  (setq comint-process-echoes t)
  (add-to-list 'comint-output-filter-functions
               #'connected-repl-collect-output))


(defun connected-repl--send-string (s)
  (with-current-buffer (get-buffer connected-repl--buffer)
    (setq connected-repl--last-output "")
    (comint-send-string (get-buffer-process (current-buffer))
                        (if (string-suffix-p "\n" s) s (concat s "\n")))))


(defun connected-repl-display-buffer ()
  (interactive)
  (save-excursion
    (set-window-buffer (split-window-right)
                       connected-repl--buffer)))


;;;###autoload
(defun connected-repl-run (&optional prefix)
  "Start an inferior process running command defined for
   `major-mode' in `connected-repl-commands' and show it in the
   same frame. With prefix argument, asks for flags to pass
   instead of the ones defined in `connected-repl-commands'."
  (interactive "P")
  (let* ((repl-entry-for-mode (assoc-default major-mode connected-repl-commands))
         (connected-repl-prompt-regexp (car repl-entry-for-mode))
         (connected-repl-command (cadr repl-entry-for-mode))
         (switches (cddr repl-entry-for-mode)))
    (unless connected-repl-command
      (user-error "Add configuration for major-mode: %s in `connected-repl-commands'."
                  major-mode))
    (setq-local connected-repl--buffer
                (apply #'make-comint
                       (generate-new-buffer-name (format "%s [Connected REPL]" major-mode))
                       connected-repl-command
                       nil
                       (if prefix (read-string "Flags: ") switches)))
    (connected-repl-add-local-bindings)
    (with-current-buffer connected-repl--buffer
      (connected-repl-mode))
    (connected-repl-display-buffer)))


;;;###autoload
(defun connected-repl-run-on-project (&optional prefix)
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (connected-repl-run prefix)))


(defun connected-repl-send-region (begin end)
  "Send selected region to `connected-repl--buffer' for evaluation
   as a snippet."
  (interactive "r")
  (unless (and (buffer-live-p connected-repl--buffer)
               (eq 'run (process-status (get-buffer-process connected-repl--buffer))))
    (user-error "Connected REPL buffer doesn't exist. Use `connected-repl-run'."))
  (connected-repl--send-string (buffer-substring begin end)))


(defun connected-repl-send-statement ()
  (interactive)
  (unless (and (buffer-live-p connected-repl--buffer)
               (eq 'run (process-status (get-buffer-process connected-repl--buffer))))
    (user-error "Connected REPL buffer doesn't exist. Use `connected-repl-run'."))
  (connected-repl--send-string (buffer-substring (line-beginning-position) (line-end-position))))


(defun connected-repl-interrupt ()
  "Send an interrupt signal to the REPL process connected to the
   current buffer."
  (interactive)
  (if-let ((p (get-buffer-process connected-repl--buffer)))
      (progn (interrupt-process p)
             (kill-buffer connected-repl--buffer))
    (message "No process to interrupt.")))


(provide 'connected-repl)
;;; connected-repl.el ends here
