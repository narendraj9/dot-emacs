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

(require 'org)
(require 'org-timer)
(require 'seq)
(require 'cl-lib)

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

(defcustom pomodoro-max-notification-count 3
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

(defcustom pomodoro-mode-line-format
  '(:eval (when (not pomodoro-timer)
            " 🔴 "))
  "Format for mode line spec displayed when there is no active Pomodoro."
  :type '(sexp)
  :group 'pomodoro)

(put 'pomodoro-mode-line-format 'risky-local-variable t)

(defface pomodoro-standard-face
  '( (((background light))
      (:weight semibold :foreground "tomato4"))
     (t
      (:weight semibold :foreground "sandy brown")) )
  "Face for pomodoro lines.")

(defface pomodoro-short-face
  '( (((background light))
      (:weight semibold :foreground "rosy brown"))
     (t (:slant italic :foreground "rosy brown")) )
  "Face for Pomodoro entries that are shorted than the default
   duration.")


(defcustom pomodoro-use-fringe-notifications nil
  "If set to true, fringe is changed to notify about a
   pomodoro/break not being active."
  :group 'pomodoro
  :type 'boolean)

(defvar pomodoro-default-fringe-style
  (cons fringe-mode
        (face-attribute 'fringe :background)))

(cl-defstruct (pomodoro-entry (:constructor pomodoro-new)
                              (:conc-name pomodoro-))
  "Struct representing a single pomodoro entry."
  (start (current-time) :read-only t :documentation "Start ts for the pomodoro.")
  (end nil :documentation "Time for the end of the pomodoro.")
  (title nil :documentation "Title of the pomodoro.")
  (org-id nil :documentation "Org ID for an associated Org heading."))

(defvar pomodoro-list (list))
(defvar pomodoro-start-time nil)
(defvar pomodoro-title nil)
(defvar pomodoro-timer nil)

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
                             pomodoro-title)))
    (push this-pomodoro pomodoro-list)
    (pomodoro-persist this-pomodoro))
  (remove-hook 'org-timer-done-hook #'pomodoro-record)
  (setq pomodoro-start-time nil))

(defun pomodoro-start-timer (minutes)
  (interactive)
  (let ((org-timer-default-timer minutes))
    (org-timer-set-timer '(4))
    (setq pomodoro-timer org-timer-countdown-timer)
    (add-hook 'org-timer-done-hook #'pomodoro-notify)
    (add-hook 'org-timer-done-hook #'pomodoro-clear-timer)
    (add-hook 'org-timer-stop-hook #'pomodoro-clear-timer)))

(defun pomodoro-clear-timer ()
  (setq pomodoro-timer nil)
  (remove-hook 'org-timer-done-hook #'pomodoro-clear-timer)
  (remove-hook 'org-timer-stop-hook #'pomodoro-clear-timer)
  ;; Remove all other hooks that might have been added by other functions.
  (remove-hook 'org-timer-done-hook #'pomodoro-record)
  (remove-hook 'org-timer-done-hook #'pomodoro-notify))

(defun pomodoro-start (&optional arg)
  "Start a new Pomodoro.

   If the default prefix argument is supplied supplied once, asks
   for the title of the pomodoro. If supplied twice, asks for
   both the title and the duration of the Pomodoro."
  (interactive "P")
  (when pomodoro-timer
    (user-error "Pomodoro active: %s" pomodoro-title))
  (pomodoro-remove-notifications)
  (when (not pomodoro-list)
    (pomodoro-load-file))
  (setq pomodoro-start-time (current-time))
  (when (or arg (not pomodoro-title))
    (setq pomodoro-title
          (completing-read "Title: "
                           (seq-uniq (mapcar #'caddr pomodoro-list)))))
  (pomodoro-start-timer (if (equal arg '(16))
                            (read-number "Duration: ")
                          pomodoro-default-duration))
  (message ">> Start: %s [Prediction: %s]"
           pomodoro-title
           (caar (pomodoro--org-agenda-matching-headings pomodoro-title)))
  (add-hook 'org-timer-done-hook #'pomodoro-record))

(defun pomodoro-start-break (&optional prefix)
  (interactive "P")
  (pomodoro-remove-notifications)
  (pomodoro-start-timer (if prefix
                            (read-number "Duration (min): ")
                          pomodoro-default-break))
  (setq pomodoro-timer nil))


(defun pomodoro-start-long-break ()
  (interactive)
  (pomodoro-remove-notifications)
  (pomodoro-start-timer pomodoro-default-long-break)
  (setq pomodoro-timer nil))

(defun pomodoro-edit-title ()
  "Changes the title of the next pomodoro that will be recorded."
  (interactive)
  (setq pomodoro-title
        (completing-read "Pomodoro: "
                         (seq-uniq (mapcar #'caddr pomodoro-list)))))

;; Notifications
;; -------------

(defun pomodoro-audio-notification ()
  (when (file-exists-p pomodoro-notification-file)
    (play-sound-file pomodoro-notification-file pomodoro-notification-volume)))

(defun pomodoro-notify ()
  (when pomodoro-use-fringe-notifications
    (fringe-mode (cons 2 0))
    (set-face-attribute 'fringe nil :background "sandy brown"))
  (pomodoro-audio-notification)
  (cl-do ((count 1 (1+ count))) ((or (not (sit-for 1))
                                     (<= pomodoro-max-notification-count count)))
    (pomodoro-audio-notification))
  (remove-hook 'org-timer-done-hook #'pomodoro-notify))

(defun pomodoro-remove-notifications ()
  (interactive)
  (when pomodoro-use-fringe-notifications
    (fringe-mode (car pomodoro-default-fringe-style))
    (set-face-attribute 'fringe nil :background (cdr pomodoro-default-fringe-style))))

;; Summarize my Pomodoros
;; ----------------------

(defun pomodoro--format-pomodoro (p)
  (let* ((start (car p))
         (end (cadr p))
         (title (caddr p) )
         (duration (floor (/ (float-time (time-subtract end start)) 60))))
    (format (propertize "%s %s: %s"
                        'face
                        (if (<= pomodoro-default-duration duration)
                            'pomodoro-standard-face
                          'pomodoro-short-face))
            (format-time-string "%H:%M" start)
            (format-time-string "%H:%M" end)
            (propertize title
                        'button t
                        'category 'pomodoro-title-text
                        'pomodoro-title-text title))))

(defun pomodoro--duration-mins (t1 t2)
  "Returns (t1 - t2) in minutes."
  (floor (/ (time-to-seconds (time-subtract t1 t2)) 60)))

(defun pomodoro--format-duration (input-mins)
  "Return a human readable duration."
  (let ((days (/ input-mins 1440))
        (hours (/ (% input-mins 1440) 60))
        (mins (% (% input-mins 1440) 60))
        (result (list)))
    (when (zerop input-mins)
      (push "<1 min" result))
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
   using `pomodoro-icon-char' for displaying Pomodoros."
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
                        (let ((covering-p (list (caar (last p-or-break-mins-partition))
                                                (cadar p-or-break-mins-partition)
                                                (caddar p-or-break-mins-partition))))
                          (format "\t%-55s %s\n"
                                  (pomodoro--format-pomodoro covering-p)
                                  (make-string (length p-or-break-mins-partition)
                                               pomodoro-icon-char)))
                      (format "\t\t%s\n" (pomodoro--format-duration (car p-or-break-mins-partition))))))))


;;;###autoload
(defun pomodoro-status ()
  (if pomodoro-timer
      (format "Current (%s): %s\n\n"
              (string-trim (org-timer-value-string))
              pomodoro-title)
    (format "Last Pomodoro: %s ago\n\n"
            (pomodoro--format-duration
             (pomodoro--duration-mins (current-time)
                                      (cadar pomodoro-list))))))

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
    (concat (pomodoro-status) (or summary "No Pomodoros"))))

(defun pomodoro-summarize ()
  (interactive)
  (let ((pomodoro-buffer-name " *POMODORO*"))
    (with-output-to-temp-buffer pomodoro-buffer-name
      (with-current-buffer pomodoro-buffer-name
        (let ((m (make-sparse-keymap)))
          (set-keymap-parent m (current-local-map))
          (use-local-map m)
          (setq revert-buffer-function
                (lambda (&rest _args) (pomodoro-summarize)))
          (define-key m [return] #'pomodoro-jump-to-org-heading)
          (insert (pomodoro-summary)))))
    (select-window (get-buffer-window pomodoro-buffer-name))))

;; Integration with org-agenda
;; ---------------------------

(defun pomodoro-jump-to-org-heading (&optional arg)
  "Jump to org-heading for the next pomodoro entry in the summary buffer.
Optional argument ARG controls whether we jump directly to the first heading
 or select one from the options."
  (interactive "P")
  (save-excursion
    (if-let ((p (text-property-search-forward 'pomodoro-title-text)))
        (let* ((matching-headings
                (pomodoro--org-agenda-matching-headings (prop-match-value p)))
               (default-heading (car matching-headings))
               (selected-heading-text
                (if arg
                    (completing-read "Heading: "
                                     matching-headings
                                     nil t nil nil
                                     default-heading)
                  (car default-heading))))
          (org-id-goto (assoc-default selected-heading-text matching-headings))
          (org-narrow-to-subtree))
      (message "No pomodoros found in this buffer."))))

(defun pomodoro--org-agenda-matching-headings (title)
  (let ((org-headings (org-map-entries (lambda ()
                                         (cons (org-get-heading t t t t)
                                               (org-id-get-create)))
                                       "/+TODO"
                                       'agenda)))
    (seq-sort-by #'car
                 (lambda (org-heading1 org-heading2)
                   (< (string-distance title org-heading1)
                      (string-distance title org-heading2)))
                 org-headings)))


(defun pomodoro-select-org-heading ()
  (let ((org-headings (pomodoro--org-agenda-matching-headings pomodoro-title)))
    (assoc (completing-read "Org entry: " org-headings nil t)
           org-headings)))

(defun pomodoro-append-to-org-agenda ()
  (when (memq this-command '(org-agenda org-agenda-redo))
    (let ((pomodoro-summary-lines (s-lines (pomodoro-summary)))
          (start-date-regex "^[0-9]\\{4\\}-[0-9]\\{1,2\\}-[0-9]\\{1,2\\}")
          (dates-seen 0))
      (goto-char (point-max))
      (insert "\n")
      ;; Assumes the date is not on the first line.
      (while (< dates-seen 2)
        (insert (car pomodoro-summary-lines))
        (insert "\n")
        (setq pomodoro-summary-lines (cdr pomodoro-summary-lines))
        (when (string-match-p start-date-regex (car pomodoro-summary-lines))
          (setq dates-seen (1+ dates-seen))))

      (goto-char (point-min))
      (set-window-start (get-buffer-window) (point-min)))))

;;;###autoload
(define-minor-mode pomodoro-mode
  "Pomodoro minor-mode for displaying status in mode line."
  :global t
  :lighter pomodoro-mode-line-format)

(provide 'pomodoro)
;;; pomodoro.el ends here
