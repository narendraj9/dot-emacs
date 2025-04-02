;;; spark.el --- Utility functions for interacting with Apache Spark  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Narendra Joshi

;; Author: Narendra Joshi <narendra@EUMHKQ260VFVJ>
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

(require 'comint)
(require 'multisession)

(define-multisession-variable spark-cluster-host nil)
(define-multisession-variable spark-cluster-id nil)
(define-multisession-variable spark-cli-profile nil)

(defun spark--get (var prompt)
  (if (multisession-value var)
      (multisession-value var)
    (setf (multisession-value var) (read-string prompt))))

(defun spark-init-script ()
  (let ((host  (spark--get spark-cluster-host "Host: "))
        (cluster (spark--get spark-cluster-id "Cluster ID: "))
        (profile (spark--get spark-cli-profile "Profile: "))
        (init-template "
            import com.databricks.connect.DatabricksSession
            import com.databricks.sdk.core.DatabricksConfig
            import org.apache.spark.sql.SparkSession
            import com.databricks.sdk.scala.dbutils.DBUtils
            val config = new DatabricksConfig().setProfile(\"%s\")
            val spark = DatabricksSession.builder().host(\"%s\").clusterId(\"%s\").remote().getOrCreate()
            val dbutils = DBUtils.getDBUtils()
            import spark.implicits._
            import org.apache.spark.sql.types._
            import org.apache.spark.sql.functions._"))
    (format init-template profile host cluster)))

(defun spark-connect! ()
  (interactive)
  (let* ((jvm-version "11")
         (scala-version "2.12.20")
         (databricks-connect "com.databricks:databricks-connect:16.2.0")
         (databricks-dbutils "com.databricks:databricks-dbutils-scala_2.12:0.1.4")
         (repl-command "scala-cli")
         (command-flags
          (append (list "repl"
                        "--jvm" jvm-version
                        "-S" scala-version
                        "--dep" databricks-dbutils
                        "--dep" databricks-connect)
                  (when (version<= "3.6.4" scala-version)
                    (list "--repl-init-script" (spark-init-script))))))
    (comint-run repl-command command-flags)
    ;; Hack for the absense of --repl-init-script
    (let ((temp-load-script-file (make-temp-file "spark-init-script" nil ".sc")))
      (with-temp-file temp-load-script-file
        (insert (spark-init-script)))
      (comint-send-string (current-buffer)
                          (format ":load %s\n" temp-load-script-file)))
    ;; Set `completion-at-point-functions' in the current buffer.
    (add-hook 'completion-at-point-functions #'spark-repl-completion-at-point nil t)
    (message "Try using `connected-repl-connect-manually' to attach the REPL to a source buffer.")))


;;; Completions
;; --------------------------------------------------------------------------------------

(defconst spark-repl-completion-regex "^\\[completions\\] \\(.*\\)$")

(defun spark-repl-get-completions (prefix)
  "Return a list of completions for PREFIX by querying the REPL process."
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (comint-redirect-results-list (concat ":completions " prefix "\n")
                                    spark-repl-completion-regex
                                    1))))

(defun spark-repl-completion-at-point ()
  "Completion-at-point function for the custom REPL."
  (when (comint-check-proc (current-buffer))
    (let* ((bounds (cons (save-excursion
                           (re-search-backward "\\s-" (point-min) t)
                           (1+ (point)))
                         (point)))
           (start (if bounds (car bounds) (point)))
           (end   (if bounds (cdr bounds) (point)))
           (prefix (buffer-substring-no-properties start end))
           (candidates (and (> (length prefix) 0)
                            (spark-repl-get-completions prefix)))
           ;; Compute the bounds again because the command seems to insert
           ;; newlines in the process buffer.
           (new-bounds (cons (save-excursion
                               (re-search-backward "\\s-" (point-min) t)
                               (1+ (point)))
                             (point))))
      (when candidates
        (list (car new-bounds) (cdr new-bounds) candidates :exclusive 'no)))))


(provide 'spark)
;;; spark.el ends here
