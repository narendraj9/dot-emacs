;;; saved-macros.el --- File containing useful macro definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: Emacs, Macros

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

;; I intend to macros for tasks that I tend to repeat in this file.

;;; Code:

(defvar saved-macros-end-string "🔔")
(defvar saved-macros-file-path load-file-name)

(defun saved-macros-save-macro (name)
  "Save a macro named NAME in `custom.el`."
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)
  (with-current-buffer (find-file-noselect saved-macros-file-path)
    (search-forward saved-macros-end-string)
    (forward-line -1)
    (end-of-line)
    (newline)
    (insert-kbd-macro name)
    (newline)
    (save-buffer)
    (kill-buffer)))

(fset 'org-store-article-link
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([return 3 16 115 110 115 134217848 111 114 103 32 115 104 111 tab tab 115 117 98 tab return 134217790 tab tab 21 24 113 24 98 return] 0 "%d")) arg)))


;; 🔔 | This symbol marks the end of definitions

(provide 'saved-macros)
;;; macros.el ends here
