;;; appearance.el --- Appearance etc.                -*- lexical-binding: t; -*-

;; Copyright (C) 2020  narendra

;; Author: narendra(defvar quick-switch-themes <narendra@merantix>
;; Keywords:

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

(require 'midnight)
(require 'seq)
(require 'solar)

(defvar app/light-theme 'modus-operandi)
(defvar app/dark-theme 'jazz)

(defun app/switch-theme (theme)
  ;; Disable other themes.
  (dolist (this-theme custom-enabled-themes)
    (when (not (eq theme this-theme))
      (disable-theme theme)))
  ;; If `theme' isn't enabled, enable it.
  (when (not (memq theme custom-enabled-themes))
    (load-theme theme t)
    (run-with-timer 5 nil #'sunrise-sunset)))

(defun app/ft->minutes (t)
  "Given fractional time: hours since midnight, return (hour minutes)."
  (round (* t 60)))

(defun app/daytime-p ()
  "Return true if it's day time in my local timezone."
  (seq-let ((sunrise _) (sunset _) _) (solar-sunrise-sunset (calendar-current-date))
    (let ((now (decode-time (current-time))))
      (< (app/ft->minutes sunrise)
         (+ (decoded-time-minute now)
            (* 60 (decoded-time-hour now)))
         (app/ft->minutes sunset)))))

(defun app/minutes->timer-string (mins)
  (format "%02d:%02d" (/ mins 60) (% mins 60)))


(defvar app/daytime-based-theme-setup-timer)
(defvar app/daytime-offset-mins 10)
(defun app/daytime-based-theme-setup ()
  (seq-let ((sunrise _) (sunset _) _) (solar-sunrise-sunset (calendar-current-date))
    (setq app/daytime-based-theme-setup-timer
          (run-at-time (if (app/daytime-p)
                           (app/minutes->timer-string
                            ;; Add Offset so that the next time this function
                            ;; runs, the other branch is taken.
                            (+ app/daytime-offset-mins
                               (app/ft->minutes sunset)))
                         ;; Surise of the next day.
                         (+ (midnight-next)
                            (* 60 (+ app/daytime-offset-mins
                                     (app/ft->minutes sunrise)))))
                       nil
                       (lambda ()
                         (app/switch-theme (if (app/daytime-p)
                                               app/dark-theme
                                             app/light-theme))
                         ;; Reset the time again for the next switch
                         (app/daytime-based-theme-setup))))))


;;;###autoload
(defun app/init ()
  "Load the correct theme based on the current time and setup a
timer for changing the theme next."
  (if (app/daytime-p)
      (app/switch-theme app/light-theme)
    (app/switch-theme app/dark-theme))
  (app/daytime-based-theme-setup))


(provide 'appearance)
;;; appearance.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("app/" . "appearance-"))
;; End:
