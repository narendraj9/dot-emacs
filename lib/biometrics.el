;;; biometrics.el --- Miscellaneous metrics I like to collect for myself  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: data

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

;; I am starting this file to collect metrics about my typing pattern.  The idea
;; is to build a state machine with characters as states and probabilities as
;; directed edges between them as of now.

;;; Code:

(defcustom biometrics-max-typing-delay 10
  "Maximum delay (in seconds) to consider okay between two characters."
  :type 'number
  :group 'data)

(defvar biometrics-last-char-info nil
  "An association list of information about last pressed char.")

(defvar biometrics-char->char (make-hash-table :test 'equal)
  "A representation of stats about time to move from char to char while typing.")

(defun biometrics--reset-last-char-info ()
  "Remove information about last typed char as typist as been idle for too long."
  (setq biometrics-last-char-info nil))

(defsubst biometrics--make-key (a b)
  "Make hash table key for stats for char A --> B."
  (format "%c%c" a b))

(defun biometrics-record-char ()
  "Record information about currently pressed char."
  (let ((current-char (char-before))
        (t1 (current-time)))
    (when biometrics-last-char-info
      (let ((last-char (alist-get :char biometrics-last-char-info))
            (t0 (alist-get :ts biometrics-last-char-info)))
        (puthash (biometrics--make-key last-char current-char)
                 (time-subtract t1 t0)
                 biometrics-char->char)))
    (setq biometrics-last-char-info
          (list (cons :ts t1) (cons :char current-char)))
    (run-with-idle-timer biometrics-max-typing-delay
                         nil
                         #'biometrics--reset-last-char-info)))

(define-minor-mode biometrics-mode
  "Global minor-mode for collecting personal metrics."
  :global t
  :group 'data
  (if biometrics-mode
      (add-hook 'post-self-insert-hook #'biometrics-record-char)
    (remove-hook 'post-self-insert-hook #'biometrics-record-char)))

(provide 'biometrics)
;;; biometrics.el ends here
