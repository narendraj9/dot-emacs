;;; string-similarity.el --- Utils for sorting strings based on similarity  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience, data

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

;; Sorting strings based on similarity to a specific string is useful for
;; ranking candidates such as those shown by `ivy' or `helm'.  This module
;; contains functions that either implement such sorting functions or use
;; external scripts to achieve the same result.

;;; Code:
(require 'dash)

(defvar string-similarity-perl-script
  (executable-find "similarity.pl"))

(defun string-similarity-perl (name candidates)
  "Returned sorted CANDIDATES based on similarity with NAME.
Same as `levenshtein-sort' but uses an external perl script for
finding string distances.  The script uses
`libstring-similarity-perl' Perl module."
  (if (not string-similarity-perl-script)
      (message "Perl script for finding similarity isn't in $PATH.")
    (let ((input-to-script (mapconcat (lambda (c)
                                        (format "%s,%s\n" name c))
                                      candidates)))
      (setq x (make-process :name "string-similarity"
                            :command (list string-similarity-perl-script)
                            :filter (lambda (p s)
                                      (message "Got %s" s)))))))


(provide 'string-similarity)
;;; string-similarity.el ends here
