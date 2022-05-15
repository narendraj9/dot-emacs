;;; inferior-jshell.el --- Launch and interact with Jshell  -*- lexical-binding: t; -*-

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


(defvar inferior-jshell--buffer)
(defvar inferior-jshell--last-output)


(defun inferior-jshell-collect-output (s)
  (setq inferior-jshell--last-output
        (concat inferior-jshell--last-output s)))


(define-derived-mode inferior-jshell-mode comint-mode "inf-jshell"
  "A major-mode for running `jshell' in `comint-mode'."
  (setq comint-prompt-regexp "jshell> ")
  (setq comint-process-echoes t)
  (add-to-list 'comint-output-filter-functions
               #'inferior-jshell-collect-output))


(defun inferior-lisp--send-string (s)
  (setq inferior-jshell--last-output "")
  (comint-send-string (get-buffer-process inferior-jshell--buffer)
                      (if (string-suffix-p "\n" s) s (concat s "\n"))))


(defun inferior-jshell-display-buffer ()
  (interactive)
  (save-excursion
    (set-window-buffer (split-window-right)
                       inferior-jshell--buffer)))


(defun inferior-jshell-run (&optional prefix)
  "Start an inferior `jshell' process and show it in the same
   frame. With a prefix argument, asks for the flags to pass to
   `jshell'."
  (interactive "P")
  (setq inferior-jshell--buffer
        (make-comint "jshell"
                     "jshell"
                     nil
                     (or (when prefix (read-string "Flags: ")) "-q")))
  (with-current-buffer inferior-jshell--buffer
    (inferior-jshell-mode))
  (inferior-jshell-display-buffer))


(defun inferior-jshell-send-region (begin end)
  "Send selected region to `inferior-jshell--buffer' for evaluation
   as a snippet."
  (interactive "r")
  (unless (and (buffer-live-p inferior-jshell--buffer)
               (eq 'run (process-status (get-buffer-process inferior-jshell--buffer))))
    (user-error "`*jshell*' buffer doesn't exist. Use `inferior-jshell-run'."))
  (inferior-lisp--send-string (buffer-substring begin end)))


(provide 'inferior-jshell)
;;; inferior-jshell.el ends here
