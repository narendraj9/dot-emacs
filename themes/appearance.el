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



(defvar quick-switch-themes (let ((themes-list (list 'jazz 'modus-vivendi)))
                              (nconc themes-list themes-list))
  "A circular list of themes to keep switching between.
Make sure that the currently enabled theme is at the head of this
list always.

A nil value implies no custom theme should be enabled.")

(defun quick-switch-themes* ()
  "Switch between to commonly used faces in Emacs.
One for writing code and the other for reading articles."
  (interactive)
  (if-let ((next-theme (cadr quick-switch-themes)))
      (progn (when-let ((current-theme (car quick-switch-themes)))
               (disable-theme (car quick-switch-themes)))
             (load-theme next-theme t)
             (message "Loaded theme: %s" next-theme))
    ;; Always have the dark mode-line theme
    (mapc #'disable-theme (delq 'smart-mode-line-dark custom-enabled-themes)))
  (setq quick-switch-themes (cdr quick-switch-themes)))

(provide 'appearance)
;;; appearance.el ends here
