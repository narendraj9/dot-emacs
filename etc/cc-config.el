;;; custom-c-style.el --- Custom c style             -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: c, c

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

;;

;;; Code:
(require 'cc-vars)
(require 'cc-styles)
(require 'cc-cmds)

(setq c-offsets-alist
      '((member-init-intro . ++)))

(defconst custom-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-no-newlines-before-nonblanks
        c-semi&comma-inside-parenlist))
    (c-hanging-colons-alist
     . ((member-init-intro before)
        (inher-intro)
        (case-label after)
        (label after)
        (access-label after)))
    (c-cleanup-list
     . (scope-operator
        empty-defun-braces
        defun-close-semi))
    (c-offsets-alist
     . ((arglist-close . c-lineup-arglist)
        (substatement-open . 0)
        (comment-intro     . 0)
        (case-label        . 4)
        (block-open        . 0)
        (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "Custom C Programming Style.")
(c-add-style "PERSONAL" custom-c-style)

(defun custom-c-mode-common-hook ()
  "Hook common to all CC-derived modes."
  (c-set-style "PERSONAL")
  (setq tab-width 4
        indent-tabs-mode nil)
  ;; we like auto-newline, but not hungry-delete
  (c-toggle-auto-newline 1))

(provide 'cc-config)
;;; cc-config.el ends here
