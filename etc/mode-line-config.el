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

(defface mode-line-delim ()
  "Face for mode line delimiters."
  :group 'mode-line-faces)

(defface mode-line-mode-name ()
  "Face for major mode name."
  :group 'mode-line-faces)

(defface mode-line-minor-mode-face ()
  "Face for minor modes in mode line."
  :group 'mode-line-faces)

(defface mode-line-size-face ()
  "Face for minor modes in mode line."
  :group 'mode-line-faces)

(defface mode-line-battery-face ()
  "Face for battery information in the mode line."
  :group 'mode-line-faces)

(defface mode-line-dimmed '((t (:foreground "grey60")))
  "A Face for items that are always dimmed"
  :group 'mode-line-faces)

;;; Collapse spaces if the mode-line is longer than the window width
;; (setq mode-line-compact 'long)
;;
;; I've disabled this because even if the mode line can be displayed without
;; compacting/collapsing spaces, the spaces are compacted/collapsed in certain
;; situations.
(setq mode-line-compact nil)


;;; Disable variable-pitch for mode-line
(set-face-attribute 'mode-line nil :inherit 'default)
(set-face-attribute 'mode-line-inactive nil :inherit 'default)
(set-face-attribute 'mode-line-active nil :inherit 'default)

;;; Tabs

(defun tab-bar-modeline ()
  "Return a string representation of current tab-bars."
  (unless (< (length (tab-bar-tabs)) 2)
    (let ((tab-index 0))
      (format "|%s|"
              (mapconcat (lambda (tab)
                           (setq tab-index (1+ tab-index))
                           (format (if (eq 'current-tab (car tab))
                                       (propertize "[%s]" 'face 'mode-line-emphasis)
                                     "%s")
                                   tab-index))
                         (tab-bar-tabs)
                         " ")))))

;;; VC

(defvar mode-line-config-hide-vc nil
  "Hide or show vc-status in the mode line.")

;;; Rendering mode-line with spaces spread out evenly.

(defun simple-mode-line-render (left middle right)
  "Return a mode-line construct with MIDDLE centered
   and available space adjust after LEFT and before RIGHT."
  (let* ((l (format-mode-line left))
         (m (format-mode-line middle))
         (r (format-mode-line right))
         (c (/ (window-total-width) 2))
         (l-len (string-width l))
         (m-len (string-width m))
         (r-len (string-width r))
         (l-space (max 0 (- c (+ l-len (/ m-len 2))))))
    ;; Returning a string here, e.g. (concat l m r), is not a good idea because
    ;; then the string is treated as a mode line construct. The string can
    ;; contain %- format specifiers.
    (list "" left (make-string l-space ?\s) middle
          ;; Display enough space to make sure that right is right-aligned.
          `(:propertize " " display (space . (:align-to (- scroll-bar ,r-len))))
          right)))

(setq mode-line-buffer-identification
      `(:propertize "%10b" face mode-line-buffer-id))

(setq mode-line-position
      `((:propertize mode-line-percent-position)
        (size-indication-mode
         (8 ,(propertize " of %I" 3 10)))
        (line-number-mode ((column-number-mode
                            (10 ,(propertize " (%l,%C)"))
                            (6 ,(propertize " L%l")))))))

(add-to-list 'mode-line-misc-info '(:eval (tab-bar-modeline)) t)

(setq-default mode-line-format
              '(:eval (simple-mode-line-render
                       ;; -- Left
                       '("%e"
                         mode-line-front-space
                         mode-line-mule-info
                         mode-line-client
                         mode-line-modified
                         mode-line-auto-compile
                         mode-line-remote
                         mode-line-frame-identification
                         mode-line-buffer-identification
                         "   " mode-line-position "  "
                         (:eval (when (and vc-mode
                                           (not mode-line-config-hide-vc))
                                  vc-mode)))

                       ;; -- Middle
                       mode-line-modes

                       ;; -- Right
                       mode-line-misc-info)))

(provide 'mode-line-config)
;;; mode-line-config.el ends here
