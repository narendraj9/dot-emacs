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

(defcustom pomodoro-notification-volume 40
  "Volume (%) to be used for audio notifications."
  :type 'number
  :group 'pomodoro)

(defcustom pomodoro-default-duration 25
  "Default duration (in minutes) for a Pomodoro."
  :type 'number
  :group 'pomodoro)

(defcustom pomodoro-default-break 5
  "Default duration (in minutes) for a break."
  :type 'number
  :group 'pomodoro)

(defcustom pomodoro-default-long-break 30
  "Default duration (in minutes) for a longer break."
  :type 'number
  :group 'pomodoro)

(defcustom pomodoro-format-list-function
  #'pomodoro-format-with-icon-char
  "Function to be used for formatting Pomodoros completed within a day.
   Provided functions:
        `pomodoro-format-with-icon-char'
        `pomodoro-format-textual'"
  :type 'function
  :group 'pomodoro)

(defcustom pomodoro-icon-char ?\N{TOMATO}
  "Character to use for displaying one unit of Pomodoro."
  :type 'character
  :group 'pomodoro)

(defface pomodoro-standard-face
  '((t . ((:weight semibold :foreground "sandy brown"))))
  "Face for pomodoro lines.")

(defface pomodoro-short-face
  '((t . ((:slant italic :foreground "rosy brown"))))
  "Face for Pomodoro entries that are shorted than the default
duration.")

(defvar pomodoro-default-fringe-style
  (cons fringe-mode
        (face-attribute 'fringe :background)))

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
                                   pomodoro-default-duration))
  (message ">> Start: %s" pomodoro-last-title)
  (add-hook 'org-timer-done-hook #'pomodoro-record)
  (add-hook 'org-timer-done-hook #'pomodoro-notify))

(defun pomodoro-start-break (&optional prefix)
  (interactive "P")
  (pomodoro-remove-notifications)
  (pomodoro-start-without-prompt (if prefix
                                     (read-number "Duration (min): ")
                                   pomodoro-default-break))
  (add-hook 'org-timer-done-hook #'pomodoro-notify))


(defun pomodoro-start-long-break ()
  (interactive)
  (pomodoro-remove-notifications)
  (pomodoro-start-without-prompt pomodoro-default-long-break)
  (add-hook 'org-timer-done-hook #'pomodoro-notify))

(defun pomodoro-edit-title ()
  "Changes the title of the next pomodoro that will be recorded."
  (interactive)
  (setq pomodoro-last-title
        (completing-read "Pomodoro: "
                         (seq-uniq (mapcar #'caddr pomodoro-list)))))

;; Notifications
;; -------------

(defun pomodoro-audio-notification ()
  (when (file-exists-p pomodoro-notification-file)
    (play-sound-file pomodoro-notification-file pomodoro-notification-volume)))

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

;; Summarize my Pomodoros
;; ----------------------

(defun pomodoro--format-pomodoro (p)
  (let* ((start (car p))
         (end (cadr p))
         (duration (floor (/ (float-time (time-subtract start end)) 60))))
    (format (propertize "%s %s: %s"
                        'face
                        (if (<= duration pomodoro-default-duration)
                            'pomodoro-standard-face
                          'pomodoro-short-face))
            (format-time-string "%H:%M" start)
            (format-time-string "%H:%M" end)
            (caddr p))))

(defun pomodoro--duration-mins (t1 t2)
  "Returns (t1 - t2) in minutes."
  (floor (/ (time-to-seconds (time-subtract t1 t2)) 60)))

(defun pomodoro--format-duration (mins)
  "Return a human readable duration."
  (let ((days (/ mins 1440))
        (hours (/ (% mins 1440) 60))
        (mins (% (% mins 1440) 60))
        (result (list)))
    (unless (zerop mins)
      (push (format "%s min%s" mins (if (= mins 1) "" "s")) result))
    (unless (zerop hours)
      (push (format "%s hr%s" hours (if (= hours 1) "" "s")) result))
    (unless (zerop days)
      (push (format "%s day%s" days (if (= days 1) "" "s")) result))
    (mapconcat #'identity result " ")))

(defun pomodoro-format-textual (pomodoros)
  "Summarize a list of Pomodoros (which belong to the same day).
   Consecutive Pomodoros with the same title are shown separately
   on a new line."
  (->> (nconc
        (-interleave pomodoros
                     (-zip-with (lambda (p1 p2)
                                  (pomodoro--duration-mins (car p1) (cadr p2)))
                                pomodoros
                                (cdr pomodoros)))
        (last pomodoros))
       (mapconcat (lambda (p)
                    (if (consp p)
                        (format "\t%-55s\n" (pomodoro--format-pomodoro p))
                      ;; This is the break in between (show it only when it's
                      ;; more than 2 * default break minutes).
                      (when (< (* 2 pomodoro-default-break) p)
                        (format "\t\t%s\n" (pomodoro--format-duration p))))))))


(defun pomodoro-format-with-icon-char (pomodoros)
  "Summarize a list of Pomodoros (which belong to the same day)
   using 🍅 for displaying Pomodoros."
  (->> (nconc (-interleave pomodoros
                           (-zip-with (lambda (p1 p2)
                                        (pomodoro--duration-mins (car p1) (cadr p2)))
                                      pomodoros
                                      (cdr pomodoros)))
              (last pomodoros))
       ;; Filter out breaks smaller than (* 2 pomodoro-default-break)
       (-filter (lambda (p-or-break-mins)
                  (or (consp p-or-break-mins)
                      (< (* 2 pomodoro-default-break) p-or-break-mins))))
       (-partition-by (lambda (p-or-break-mins)
                        (when (consp p-or-break-mins)
                          (caddr p-or-break-mins))))
       (mapconcat (lambda (p-or-break-mins-partition)
                    (if (consp (car p-or-break-mins-partition))
                        (let ((covering-p (list (cadar (last p-or-break-mins-partition))
                                                (caar p-or-break-mins-partition)
                                                (caddar p-or-break-mins-partition))))
                          (format "\t%-55s %s\n"
                                  (pomodoro--format-pomodoro covering-p)
                                  (make-string (length p-or-break-mins-partition)
                                               pomodoro-icon-char)))
                      (format "\t\t%s\n" (pomodoro--format-duration (car p-or-break-mins-partition))))))))

;;;###autoload
(defun pomodoro-summary ()
  (when (not pomodoro-list)
    (pomodoro-load-file))
  (let* ((day-ps-alist (-group-by (lambda (p)
                                    (format-time-string "%F (%a)" (car p)))
                                  pomodoro-list))
         (summary (mapconcat (lambda (day-ps)
                               (format "%s [%d]:\n%s"
                                       (propertize (car day-ps) 'face 'highlight)
                                       (length (cdr day-ps))
                                       (funcall pomodoro-format-list-function (cdr day-ps))))
                             day-ps-alist
                             "\n")))
    (concat (if pomodoro-start-time
                (format "Current (%s): %s\n\n"
                        (string-trim (org-timer-value-string))
                        pomodoro-last-title)
              (format "Last Pomodoro: %s ago\n\n"
                      (pomodoro--format-duration
                       (pomodoro--duration-mins (current-time)
                                                (cadar pomodoro-list)))))
            (or summary "No Pomodoros"))))

(defun pomodoro-summarize ()
  (interactive)
  (let ((pomodoro-buffer-name " *POMODORO*"))
    (with-output-to-temp-buffer pomodoro-buffer-name
      (with-current-buffer pomodoro-buffer-name
        (insert (pomodoro-summary))))))


(provide 'pomodoro)
;;; pomodoro.el ends here
