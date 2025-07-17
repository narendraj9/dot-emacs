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

(require 'goto-addr)

(defvar sourcegraph-search-history nil
  "History of recent Sourcegraph search queries.")

(defvar sourcegraph-search-presets
  (let* ((date-format "%Y-%m-%d")
         (start-date-time (seconds-to-time
                           (- (float-time (current-time))
                              (* 60 60 24 30.44 6)))) ;; ~6 months
         (start-date (format-time-string date-format start-date-time)))
    (list
     (format "type:diff rev:* after:%s repo:" start-date)
     (format "type:commit after:%s repo:" start-date)
     "lang:java symbol:"))
  "Presets for commit, diff, and symbol Sourcegraph queries, with `after:<date>` set to six months ago by default.")

(defun sourcegraph-search ()
  (interactive)
  (let* ((query (completing-read
                 "Sourcegraph query: "
                 sourcegraph-search-presets
                 nil nil nil 'sourcegraph-search-history)))
    (push query sourcegraph-search-history)
    (let* ((command (format "COLOR=1 src search -less=0 -- 'context:global %s';" query))
           (results (shell-command-to-string command))
           (inhibit-read-only t))
      (with-current-buffer (get-buffer-create "*sourcegraph*")
        (erase-buffer)
        (insert results)
        (ansi-color-apply-on-region (point-min) (point-max))
        (sourcegraph-mode)
        (display-buffer-full-frame (current-buffer) (list))
        (goto-char (point-min))
        (toggle-truncate-lines +1)))))

(defun sourcegraph-next-url ()
  "Jump to the next URL in the buffer."
  (interactive)
  (let ((url-regex "https?://[^[:space:]]+"))
    ;; If we're on a URL, move past it first
    (when (thing-at-point 'url)
      (re-search-forward url-regex nil t))
    ;; Now search for the next URL
    (if (re-search-forward url-regex nil t)
        (goto-char (match-beginning 0))
      (message "No more URLs found"))))

(defun sourcegraph-previous-url ()
  "Jump to the previous URL in the buffer."
  (interactive)
  (let ((url-regex "https?://[^[:space:]]+"))
    ;; If we're on a URL, move before it first
    (when (thing-at-point 'url)
      (re-search-backward url-regex nil t))
    ;; Now search for the previous URL
    (if (re-search-backward url-regex nil t)
        (goto-char (match-beginning 0))
      (message "No previous URLs found"))))

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

(defun sourcegraph-browse-repo-mrs ()
  "Search for a repo via Sourcegraph CLI and browse to its merge requests page."
  (interactive)
  (let* ((query (read-string "Repository search query: "))
         (author (if (boundp 'my/user-full-name)
                     my/user-full-name
                   (read-string "Author filter (optional): " (user-full-name))))
         (command (format "src repos list -query='%s' -first=10" query))
         (command-output (shell-command-to-string command)))
    (if (string-empty-p (string-trim command-output))
        (message "No repositories found")
      (let* ((repos (split-string command-output))
             (selected-repo (if (< 1 (length repos))
                                (completing-read "Select repository: " repos)
                              (car repos)))
             (mr-url (format "https://%s/-/merge_requests?author_username=%s" selected-repo author)))
        (browse-url mr-url)))))

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
            (define-key map (kbd "s") 'sourcegraph-search)
            (define-key map (kbd "n") 'sourcegraph-next-url)
            (define-key map (kbd "p") 'sourcegraph-previous-url)
            (define-key map (kbd "RET") 'goto-address-at-point)
            (define-key map (kbd "v") 'sourcegraph-fetch-file-at-point)
            (define-key map (kbd "r") 'sourcegraph-browse-repo-mrs)
            (define-key map (kbd "g") 'sourcegraph-search)
            (define-key map (kbd "q") 'quit-window)
            map)
  (goto-address-mode sourcegraph-mode))


(provide 'sourcegraph)
;;; sourcegraph.el ends here
