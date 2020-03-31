;;; transient-defs.el --- Some convenient transient definitions.   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Narendra Joshi

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

;; Transient definitions for commands that are complex with options
;; that I would like to remember or have some sort of quick
;; documentation for.

;;; Code:

(require 'transient)

(defun transient-defs-start-jvm (&rest args)
  "Start JVM with `ARGS'."
  (interactive)
  (message "%s" (transient-args this-command)))

(define-transient-command transient-defs-java ()
  "Start a JVM process with `java'."
  :man-page "java"
  ["Heap Options"
   ("-ms" "Initial heap size" "-Xms=")
   ("-mx" "Maximum heap size" "-Xmx=")
   ("-ss" "Thread stack size" "-Xss=")]
  ["Start"
   [("x" "Start JVM" transient-defs-start-jvm)]])


(provide 'transient-defs)
;;; transient-defs.el ends here
