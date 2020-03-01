;;; window-snapshots.el --- Code for saving and restoring window configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience

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

(require 'dash)
(require 'defs)

(defvar window-snapshots '())

(defvar window-snapshots-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "z") #'save-window-snapshot)
    (define-key m (kbd "C-z") #'cycle-window-snapshots)
    m))

(defun cheerful-message (msg)
  (message (propertize msg 'face (seq-random-elt (face-list)))))

(defun window-snapshots (arg)
  "Save or restore window snaphosts.
     Credits: https://erick.navarro.io/blog/save-and-restore-window-configuration-in-emacs/"
  (interactive "p")

  (cond
   ((= arg 1)
    (progn (cycle-window-snapshots)
           (set-transient-map window-snapshots-map t)))

   ((= arg 4)
    (progn (save-window-snapshot)
           (cheerful-message "=> Saved window configuration!")))

   ((= arg 16)
    (progn
      (setq window-snapshots '())
      (cheerful-message "=> Cleared all window snapshots!")))))

(defun save-window-snapshot ()
  "Save the current window configuration into `window-snapshots` alist."
  (interactive)
  (let ((-compare-fn #'compare-window-configurations))
    (unless (-contains? window-snapshots (current-window-configuration))
      (push (current-window-configuration) window-snapshots))))

(defun cycle-window-snapshots ()
  "Restore a window snapshot from the window-snapshots alist."
  (interactive)
  (let ((w (car window-snapshots))
        (rotate-fn (lambda ()
                     (setq window-snapshots
                           (-rotate -1 window-snapshots)))))
    (cond

     ((not w)
      (cheerful-message "=> No window configurations saved."))

     ((compare-window-configurations w (current-window-configuration))
      (if (= 1 (length window-snapshots))
          (cheerful-message "=> Already using the saved window configuration.")
        (funcall rotate-fn)
        (cycle-window-snapshots)))

     (t (set-window-configuration w)
        (cheerful-message "==> Switched window configuration")
        (funcall rotate-fn)))))

(provide 'window-snapshots)
;;; window-snapshots.el ends here
