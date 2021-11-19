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
(require 'seq)
(require 'dash)

(defgroup pomodoro ()
  "A simple Pomodoro system."
  :group 'emacs)

(defcustom pomodoro-file
  (expand-file-name "pomodoro/pomodoro-records.el"
                    emacs-assets-directory)
  "File for persisting Pomodoro records."
  :type 'file
  :group 'pomodoro)

(defcustom pomodoro-max-notification-count 2
  "Numbers of times to play audio notification."
  :type 'number
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
  "Start a new Pomodoro.

   If the default prefix argument is supplied supplied once, asks
   for the title of the pomodoro. If supplied twice, asks for
   both the title and the duration of the Pomodoro."
  (interactive "P")
  (pomodoro-remove-notifications)
  (when (not pomodoro-list)
    (pomodoro-load-file))
  (setq pomodoro-start-time (current-time))
  (when (or arg (not pomodoro-last-title))
    (setq pomodoro-last-title
          (completing-read "Title: "
                           (seq-uniq (mapcar #'caddr pomodoro-list)))))
  (pomodoro-start-without-prompt (if (equal arg '(16))
                                     (read-number "Duration: ")
                                   25))
  (message ">> Start: %s" pomodoro-last-title)
  (add-hook 'org-timer-done-hook #'pomodoro-record)
  (add-hook 'org-timer-done-hook #'pomodoro-notify))

(defun pomodoro-start-break (&optional prefix)
  (interactive "P")
  (pomodoro-remove-notifications)
  (pomodoro-start-without-prompt (if prefix (read-number "Duration (min): ") 5))
  (add-hook 'org-timer-done-hook #'pomodoro-notify))

(defun pomodoro-edit-title ()
  "Changes the title of the next pomodoro that will be recorded."
  (interactive)
  (setq pomodoro-last-title
        (completing-read "Pomodoro: "
                         (seq-uniq (mapcar #'caddr pomodoro-list)))))

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
                 (mapconcat (lambda (p)
                              (if (consp p)
                                  (format "\t%s %s: %s\n"
                                          (format-time-string "%H:%M" (car p))
                                          (format-time-string "%H:%M" (cadr p))
                                          (caddr p))
                                (format "%8d mins\n"
                                        (floor (/ p 60)))))))))
         (summary (mapconcat (lambda (day-ps)
                               (format "%s [%d]:\n%s"
                                       (propertize (car day-ps) 'face 'highlight)
                                       (length (cdr day-ps))
                                       (funcall format-ps (cdr day-ps))))
                             day-ps-alist
                             "\n")))
    (with-output-to-temp-buffer pomodoro-buffer-name
      (with-current-buffer pomodoro-buffer-name
        (when pomodoro-start-time
          (insert (format "Current (%s): %s\n\n"
                          (string-trim (org-timer-value-string))
                          pomodoro-last-title)))
        (insert (or summary "No Pomodoros"))))))

(defun pomodoro-audio-notification ()
  (when (file-exists-p pomodoro-notification-file)
    (play-sound-file pomodoro-notification-file 40)))

(defun pomodoro-notify ()
  (setq pomodoro-default-fringe-style (cons fringe-mode
                                            (face-attribute 'fringe :background)))
  (fringe-mode (cons 2 0))
  (set-face-attribute 'fringe nil :background "sandy brown")
  (pomodoro-audio-notification)
  (cl-do ((count 1 (1+ count))) ((or (not (sit-for 1))
                                     (<= pomodoro-max-notification-count count)))
    (pomodoro-audio-notification))
  (remove-hook 'org-timer-done-hook #'pomodoro-notify))

(defun pomodoro-remove-notifications ()
  (interactive)
  (fringe-mode (car pomodoro-default-fringe-style))
  (set-face-attribute 'fringe nil :background (cdr pomodoro-default-fringe-style)))

(provide 'pomodoro)
;;; pomodoro.el ends here
