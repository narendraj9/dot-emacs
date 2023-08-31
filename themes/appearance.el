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

(use-package ef-themes
  :ensure t
  :defer t
  :config
  (setq ef-dark-palette-overrides
        '((bg-main "#151515"))))

(use-package modus-themes
  :ensure t
  :pin gnu
  :init
  ;; Disable mixed fonts in modus themes
  (setq modus-themes-mixed-fonts nil)

  (setq modus-operandi-palette-overrides
        '((fringe "GhostWhite"))))

(defcustom app/enable-time-based-theme-switching nil
  "Set to t to enable switching between `app/light-theme' and
`app/dark-theme' based on time of the day.")

(defvar app/light-theme 'modus-operandi)
(defvar app/dark-theme 'jazz)

(defun app/switch-theme (theme)
  ;; Disable other themes.
  (dolist (this-theme custom-enabled-themes)
    (when (not (eq theme this-theme))
      (disable-theme this-theme)))
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

(defun app/daytime-switch-theme ()
  (app/switch-theme (if (app/daytime-p) app/light-theme app/dark-theme))
  (app/daytime-switch-theme-reschedule))

(defun app/daytime-switch-theme-reschedule ()
  (seq-let ((sunrise _) (sunset _) _) (solar-sunrise-sunset (calendar-current-date))
    (let ((next-switch-time (if (app/daytime-p)
                                (app/minutes->timer-string
                                 ;; Add Offset so that the next time this function
                                 ;; runs, the other branch is taken.
                                 (+ app/daytime-offset-mins
                                    (app/ft->minutes sunset)))
                              ;; Surise of the next day.
                              (+ (midnight-next)
                                 (* 60 (+ app/daytime-offset-mins
                                          (app/ft->minutes sunrise)))))))
      (setq app/daytime-theme-switcher-timer
            (run-at-time next-switch-time
                         nil
                         #'app/daytime-switch-theme)))))


(defun app/font-availablep (font)
  "Return true if FONT is available on system.
   This is written to avoid calling `find-font' repeatedly."
  (let ((favailablep (intern (concat font "-availablep"))))
    (if (boundp favailablep)
        (symbol-value favailablep)
      (customize-save-variable favailablep
                               (not (null (find-font (font-spec :name font))))))))


(defun app/font-setup ()
  ;; Setup my favorite fonts [if available]
  (dolist (font (list "Symbola" "Fira Code"))
    (if (app/font-availablep font)
        (set-fontset-font "fontset-default" nil
                          (font-spec :name font :size 15)
                          nil 'append)))

  ;; Font for coding.
  (set-face-attribute 'default nil
                      :family "Fira Code"
                      :height 143
                      :weight 'semibold)

  ;; Font for reading news
  (cond
   ((app/font-availablep "Carlito")
    ;; It would have been great if I could set the background to white
    ;; while reading anything other than code. Emacs doesn't support
    ;; buffer-local themes and doing this would require nasty tricks
    ;; with hooks.
    (set-face-attribute 'variable-pitch nil
                        :family "Carlito"
                        :height 130
                        :weight 'normal
                        :width 'ultraexpanded))))

;;;###autoload
(defun app/init ()
  "Load the correct theme based on the current time and setup a
timer for changing the theme next."
  (app/font-setup)

  (when app/enable-time-based-theme-switching
    (app/daytime-switch-theme))

  ;; Show long lines as continuations.
  (setq-default truncate-lines nil)

  ;; Cursor settings
  (setq-default cursor-type t)
  ;; Useful for copying text to a minibuffer.
  (setq-default cursor-in-non-selected-windows 'hollow)
  ;;
  (setq x-underline-at-descent-line t)

  ;; Maximize emacs on startup
  (when (window-system)
    (add-to-list 'default-frame-alist
                 '(fullscreen . maximized))))


(provide 'appearance)
;;; appearance.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("app/" . "appearance-"))
;; End:
