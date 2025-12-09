;;; java-config.el --- Java mode and related config  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Narendra Joshi

;; Author: Narendra Joshi
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

(cl-defmethod eglot-handle-notification
  (server (_method (eql language/status)) &key type message &allow-other-keys)
  (when (equal type "Started")
    (message "LSP server ready: %s" (eglot-project-nickname server))))

(defun java-eclipse-jdt-file-name-handler (operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir "/tmp/.eglot")
         (source-file
          (expand-file-name
           (file-name-concat
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(defun java-eclipse-jdt-launcher (_arg)
  "Returns a command to start Eclipse JDT launcher script `jdtls'."
  (let ((launcher-script (expand-file-name "org.eclipse.jdt.ls.product/target/repository/bin/jdtls" "~/code/eclipse.jdt.ls/"))
        (root-directory (project-root (project-current))))
    (if (file-exists-p launcher-script)
        (list launcher-script
              "-data"
              (expand-file-name (format "%s-%s" (md5 root-directory)
                                        (file-name-base (directory-file-name root-directory )))
                                "~/code/jdtls-workspace/")
              :initializationOptions '(:extendedClientCapabilities
                                       (:classFileContentsSupport t)))
      (message "Failed to find any JDT jar files.")
      nil)))

(use-package java-ts-mode
  :demand t
  :init
  ;; See:
  ;; https://github.com/eclipse-jdtls/eclipse.jdt.ls/issues/2322#issuecomment-1423092394
  (add-to-list 'file-name-handler-alist '("\\`jdt://" . java-eclipse-jdt-file-name-handler))
  (add-to-list 'recentf-exclude "^/tmp/.eglot/.*"))

(use-package javadoc-lookup
  :demand t
  :init
  (setq javadoc-lookup-completing-read-function completing-read-function)
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-local browse-url-browser-function
                          (if (featurep 'xwidget-internal)
                              #'xwidget-webkit-browse-url
                            #'eww-browse-url))
              (define-key java-mode-map (kbd "C-c ; d")  #'javadoc-lookup)
              (define-key java-mode-map (kbd "C-c ; i")  #'javadoc-add-import)
              (define-key java-mode-map (kbd "C-c ; s")  #'javadoc-sort-imports))))

(provide 'java-config)
