;;; mode-line-config.el --- Code for managing mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: emacs, data, con

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

;; Credits:
;; https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline/45788

;;; Code:

(defface mode-line-delim () "Face for mode line delimiters.")
(defface mode-line-mode-name () "Face for major mode name.")
(defface mode-line-minor-mode-face () "Face for minor modes in mode line.")
(defface mode-line-size-face () "Face for minor modes in mode line.")

(defun simple-mode-line-render (left middle right)
  "Return a mode-line construct with MIDDLE centered and
available space adjust after LEFT and before RIGHT."
  (let* ((l (format-mode-line left))
         (m (format-mode-line middle))
         (r (format-mode-line right))
         (c (/ (window-width) 2))
         (l-len (length l))
         (m-len (length m))
         (r-len (length r))
         (available-width (- (window-total-width)
                             (+ l-len m-len r-len)
                             1          ; - some empty space at the end.
                             ))
         (l-space  (max 0 (- c (+ l-len (/ m-len 2)))))
         (r-space (max 0 (- available-width l-space))))
    (append left
            (list (format (format "%%%ds" l-space) ""))
            middle
            (list (format (format "%%%ds" r-space) ""))
            right)))

(setq mode-line-buffer-identification
      `((:propertize "%10b" face mode-line-buffer-id)))

(setq mode-line-position
      `((:propertize mode-line-percent-position)
        (size-indication-mode
         (8 ,(propertize " of %I" 3 10)))
        (line-number-mode ((column-number-mode
                            (10 ,(propertize " (%l,%C)"))
                            (6 ,(propertize " L%l")))))))

(setq
 mode-line-modes
 (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
   (list (propertize "%[" 'help-echo recursive-edit-help-echo)
         (propertize "( " 'face 'mode-line-delim)
         `(:propertize ("" mode-name)
                       face mode-line-mode-name
                       help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                       mouse-face mode-line-highlight
                       local-map ,mode-line-major-mode-keymap)
         '("" mode-line-process)
         `(:propertize ("" minor-mode-alist)
                       face mode-line-minor-mode-face
                       mouse-face mode-line-highlight
                       help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                       local-map ,mode-line-minor-mode-keymap)
         (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                     'mouse-face 'mode-line-highlight
                     'local-map (make-mode-line-mouse-map
                                 'mouse-2 #'mode-line-widen))
         (propertize " )" 'face 'mode-line-delim)
         (propertize "%]" 'help-echo recursive-edit-help-echo)
         " ")))

(setq-default mode-line-format
              '(:eval (simple-mode-line-render
                       '("%e" mode-line-front-space
                         mode-line-mule-info
                         mode-line-client
                         mode-line-modified
                         mode-line-auto-compile
                         mode-line-remote
                         mode-line-frame-identification
                         mode-line-buffer-identification
                         "   " mode-line-position "  ")
                       '("%e" mode-line-modes)
                       ;; ---- Spaces
                       '("%e" mode-line-misc-info mode-line-end-spaces))))

;;; -- Some utility functions for other modes
(defun simple-projectile-mode-line-function ()
  (when-let ((project-name (projectile-project-name)))
    (concat projectile-mode-line-prefix
            "["
            (propertize project-name 'face '(:foreground "orange"))
            "]")))

(provide 'mode-line-config)
;;; mode-line-config.el ends here
