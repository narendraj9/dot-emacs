;;; minibuffer-command-history.el --- Separate minibuffer history for each command  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1

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
(defvar minibuffer-command-history
  (make-hash-table :test 'eq
                   :size history-length)
  "Hash table for keeping history per command.")

(defun minibuffer-command-wrapper-fn
    (original-read initial-contents keymap read hist &rest args)
  "Advice for `read-from-minibuffer' to setup and record history for a command.
`ORIGINAL-READ' is the original `read-from-minibuffer' for this package.
`ARGS' are arguments that are passed onto `ORIGINAL-READ'."
  (let* ((cmd this-command)
         (cmd-history (gethash cmd minibuffer-command-history))
         (minibuffer-history (or hist cmd-history minibuffer-history))
         (input (apply original-read initial-contents keymap read nil args)))
    (puthash cmd (cons input cmd-history) minibuffer-command-history)
    input))

;;;###autoload
(defun minibuffer-command-history-enable ()
  "Setup advice for `read-from-minibuffer' to have history per command."
  (add-function :around
                (symbol-function 'read-from-minibuffer)
                #'minibuffer-command-wrapper-fn))

;;;###autoload
(defun minibuffer-command-history-disable ()
  "Remove advice added by `minibuffer-command-history-enable'."
  (remove-function (symbol-function 'read-from-minibuffer)
                   #'minibuffer-command-wrapper-fn))

(provide 'minibuffer-command-history)
;;; minibuffer-command-history.el ends here
