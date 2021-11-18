;;; pomodoro.el --- Simple pomodoro built with org-timer.el fns  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Version: 0.1
;; Keywords: tools, data
;; Package-Requires: org

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

;; This package provides a simple interface to start and record a Pomodoro
;; (https://todoist.com/productivity-methods/pomodoro-technique).

;;; Code:

(require 'org-timer)

(defgroup pomodoro ()
  "A simple Pomodoro system."
  :group 'emacs)

(defcustom pomodoro-file
  (expand-file-name "pomodoro/pomodoro-records.el"
                    emacs-assets-directory)
  "File for persisting Pomodoro records."
  :type 'file
  :group 'pomodoro)

(defcustom pomodoro-notification-file
  (expand-file-name "audio/quite-impressed.wav"
                    emacs-assets-directory)
  "Audio file for notification when a Pomodoro is completed."
  :type 'file
  :group 'pomodoro)

(defvar pomodoro-default-fringe-style nil)
(defvar pomodoro-list (list))
(defvar pomodoro-start-time nil)
(defvar pomodoro-last-title nil)

(defun pomodoro-persist (p)
  (with-temp-buffer
    (prin1 p (current-buffer))
    (terpri (current-buffer))
    (let ((save-silently t))
      (append-to-file (point-min) (point-max) pomodoro-file))))

(defun pomodoro-load-file ()
  (with-temp-buffer
    (insert-file-contents pomodoro-file)
    (goto-char (point-min))
    (condition-case _end-of-file-error
        (while t
          (push (read (current-buffer)) pomodoro-list))
      (end-of-file))))

(defun pomodoro-record ()
  (let ((this-pomodoro (list pomodoro-start-time
                             (current-time)
                             pomodoro-last-title)))
    (push this-pomodoro pomodoro-list)
    (pomodoro-persist this-pomodoro))
  (remove-hook 'org-timer-done-hook #'pomodoro-record)
  (setq pomodoro-start-time nil))

(defun pomodoro-start-without-prompt (minutes)
  (interactive)
  (let ((org-timer-default-timer minutes))
    (org-timer-set-timer '(4))))

(defun pomodoro-start (&optional arg)
  (interactive "P")
  (when (not pomodoro-list)
    (pomodoro-load-file))
  (setq pomodoro-start-time (current-time))
  (when (not pomodoro-last-title)
    (setq pomodoro-last-title
          (completing-read "Title: " (mapcar #'caddr pomodoro-list))))
  (pomodoro-start-without-prompt (if arg (read-number "Duration: ") 25))
  (message ">> Start: %s" pomodoro-last-title)
  (add-hook 'org-timer-done-hook #'pomodoro-record)
  (add-hook 'org-timer-done-hook #'pomodoro-notify))

(defun pomodoro-start-break (&optional prefix)
  (interactive "P")
  (pomodoro-remove-notifications)
  (pomodoro-start-without-prompt (if prefix (read-number "Duration (min): ") 5)))

(defun pomodoro-edit-title ()
  "Changes the title of the next pomodoro that will be recorded."
  (interactive)
  (setq pomodoro-last-title
        (completing-read "Pomodoro: " (mapcar #'caddr pomodoro-list))))

(defun pomodoro-summarize ()
  (interactive)
  (when (not pomodoro-list)
    (pomodoro-load-file))
  (let* ((pomodoro-buffer-name " *POMODORO*")
         (day-ps-alist (-group-by (lambda (p)
                                    (format-time-string "%F (%a)" (car p)))
                                  pomodoro-list))
         (format-ps
          (lambda (ps)
            (->> (nconc (-interleave ps
                                     (-zip-with (lambda (p1 p2)
                                                  (time-to-seconds (time-subtract (cadr p1)
                                                                                  (car p2))))
                                                ps
                                                (cdr ps)))
                        (last ps))
                 (-map (lambda (p)
                         (if (consp p)
                             (format "\t%s %s: %s\n"
                                     (format-time-string "%H:%M" (car p))
                                     (format-time-string "%H:%M" (cadr p))
                                     (caddr p))
                           (format "%8d mins\n"
                                   (floor (/ p 60))))))
                 (apply #'concat))))
         (summary
          (->> day-ps-alist
               (-mapcat (lambda (day-ps)
                          (format "%s [%d]:\n%s"
                                  (propertize (car day-ps) 'face 'highlight)
                                  (length (cdr day-ps))
                                  (funcall format-ps (cdr day-ps))))))))
    (with-output-to-temp-buffer pomodoro-buffer-name
      (with-current-buffer pomodoro-buffer-name
        (when pomodoro-start-time
          (insert (format "Current (%s): %s\n\n"
                          (string-trim (org-timer-value-string))
                          pomodoro-last-title)))
        (insert (or summary "No Pomodoros"))))))

(defun pomodoro-notify ()
  (setq pomodoro-default-fringe-style fringe-mode)
  (fringe-mode (cons 4 0))
  (set-face-attribute 'fringe nil :background "sandy brown")
  (make-thread (lambda () (play-sound-file pomodoro-notification-file 30)))
  (cl-do () ((not (sit-for 1)) :done)
    (play-sound-file pomodoro-notification-file 40))
  (remove-hook 'org-timer-done-hook #'pomodoro-notify))

(defun pomodoro-remove-notifications ()
  (interactive)
  (fringe-mode pomodoro-default-fringe-style))

(provide 'pomodoro)
;;; pomodoro.el ends here
