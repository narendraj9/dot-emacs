;;; cr/.el --- Customer Emacs register types for personal use.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Narendra Joshi

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

;; This package defines a few register types that I think are going to be useful
;; for me.

;;; Code:

(eval-when-compile (require 'cl-macs))
(require 'thingatpt)

;;; URLs
;; ----------------------------------------------------------------------------

(cl-defstruct custom-registers-url-register
  "A structure for storing a URL bookmark in a register."
  url
  description)


(cl-defmethod register-val-describe
  ((r custom-registers-url-register) _verbose)
  (princ "URL: ")
  (princ (custom-registers-url-register-url r))
  (princ "\n")
  (princ (custom-registers-url-register-description r)))


(cl-defmethod register-val-insert
  ((r custom-registers-url-register))
  (insert (custom-registers-url-register-url r)))


(cl-defmethod register-val-jump-to ((r custom-registers-url-register) _arg)
  (browse-url (custom-registers-url-register-url r)))


(defun cr/url-to-register (register arg)
  (interactive (list (register-read-with-preview "URL to register:")
                     current-prefix-arg))
  (let* ((url (if (region-active-p)
                  (buffer-substring (region-beginning) (region-end))
                (read-string "URL: " nil nil (thing-at-point 'url))))
         (description (read-string "Description: " nil nil url)))
    (set-register register
                  (make-custom-registers-url-register :url url :description description))))


(provide 'custom-registers)
;;; custom-registers.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("cr/" . "custom-registers-"))
;; End:
