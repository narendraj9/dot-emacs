;;; mylife-logger.el --- For logging activites       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I intend to use this to log things like time spent doing an
;; activity on a particular day, or whether I worked out a day or not.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defcustom mylife-logger-logfile
  (expand-file-name "~/miscellany/assets/mylife-observable.log")
  "Path to the file for storing logged entries"
  :type 'file
  :group 'mylife)

(defcustom mylife-logger-timestamp-format
  "%FT%T%z"
  "Format for the timestamps."
  :type 'string
  :group 'mylife)

(defvar mylife--observable-string-prefix "⁍ "
  "Prefix before an observable string in the log.")

(defvar mylife--observables nil
  "List of all the existing observables.")

(defun mylife--get-logbuffer ()
  "Internal function for getting the buffer to add a log entry."
  (unless (file-exists-p mylife-logger-logfile)
    (write-region "" nil mylife-logger-logfile))
  (find-file-noselect mylife-logger-logfile))

(defun mylife--make-observable-string (observable)
  "Make the title of the OBSERVABLE."
  (format "\n%s%s"
          mylife--observable-string-prefix
          observable))

(defun mylife--get-all-observables ()
  "Return all observables recorded so far."
  (unless mylife--observables
    (with-current-buffer (mylife--get-logbuffer)
      (goto-char (point-min))
      (setq mylife--observables
            (loop while (search-forward mylife--observable-string-prefix nil t)
                  collect (buffer-substring-no-properties (point) (line-end-position))))))
  mylife--observables)

(defun mylife-log-it (observable value)
  "Log OBSERVABLE at current time with an optional VALUE.
If VALUE isn't specified, it is assumed to be boolean and a ✔ is stored.
Both OBSERVABLE and VALUE are strings."
  (interactive
   (list (completing-read "Observable: "
                          (mylife--get-all-observables))
         (completing-read "Value (Default ✔): "
                          '("✔" "❌"))))
  (with-current-buffer (mylife--get-logbuffer)
    (goto-char (point-min))
    (unless (search-forward (mylife--make-observable-string observable) nil t)
      (goto-char (point-max))
      (insert (mylife--make-observable-string observable)))
    (insert (format "\n%s\t%s" (format-time-string "%FT%T%z")
                    (if (equal value "")
                        "✔"
                      value)))
    (save-buffer)
    (kill-buffer)))

(defun mylife-show-it (observable)
  "Show a timeline for OBSERVABLE."
  (interactive (list (completing-read "Observable: "(mylife--get-all-observables))))
  (message "To be implemented."))

(provide 'mylife-logger)
;;; mylife-logger.el ends here
