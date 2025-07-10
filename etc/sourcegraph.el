;;; sourcegraph.el --- Utilities for interacting with Sourcegraph  -*- lexical-binding: t; -*-

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

(defun sourcegraph-search ()
  "Simple Sourcegraph search with keymap for quit and refresh."
  (interactive)
  (let* ((query (read-string "Search: "))
         (command (format "COLOR=1 src search -less=0 -- 'context:global %s';"
                          query))
         (results (shell-command-to-string command))
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*sourcegraph*")
      (erase-buffer)
      (insert results)
      (ansi-color-apply-on-region (point-min) (point-max))
      (sourcegraph-mode)
      (display-buffer-full-frame (current-buffer) (list))
      (goto-char (point-min))
      (toggle-truncate-lines +1))))

(defun sourcegraph-fetch-file-at-point ()
  "Extract Sourcegraph URL at point and construct a GraphQL query to fetch the file content."
  (interactive)
  (let* ((url (thing-at-point 'url))
         (parsed (sourcegraph-parse-url url)))
    (if parsed
        (let* ((repo (plist-get parsed :repo))
               (branch (plist-get parsed :branch))
               (path (plist-get parsed :path))
               (query (format "query { repository(name: \"%s\") { commit(rev: \"%s\") { file(path: \"%s\") { content } } } }"
                              repo branch path))
               (command (format "src api -query='%s' | jq -r .data.repository.commit.file.content" query)))
          (message "Command: %s" command)
          (with-current-buffer (get-buffer-create "*sourcegraph-output*")
            (insert (shell-command-to-string command))
            (goto-char (point-min))
            ;; TODO: Fix
            (goto-line 5)))
      (message "No valid Sourcegraph URL found at point"))))

(defun sourcegraph-parse-url (url)
  "Parse a Sourcegraph URL and extract repository, branch, and file path."
  (when (and url (string-match "https://[^/]+/\\(.+?\\)\\(@\\([^/]+\\)\\)?/-/blob/\\(.+\\)" url))
    (list :repo (match-string 1 url)
          :branch (or (match-string 3 url) "master")
          :path (match-string 4 url))))

(define-minor-mode sourcegraph-mode
  "Minor mode for sourcegraph search results."
  :lighter " SG"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "g") 'sourcegraph-search)
            (define-key map (kbd "q") 'quit-window)
            map))


(provide 'sourcegraph)
;;; sourcegraph.el ends here
