;;; gnus-config.el --- My Gnus configuration                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; It's Gnus.

;;; Code:

(require 'bbdb)
(require 'bbdb-gnus)

(require 'gnus)
(require 'gnus-msg)
(require 'gnus-art)
(require 'gnus-async)


(require 'nnmail)
(require 'smtpmail)
(require 'message)

(require 'nnir)

(setq user-mail-address "narendraj9@gmail.com"
      user-full-name "Narendra Joshi"
      message-signature "Narendra Joshi")

;; Gnus startup
(setq gnus-always-read-dribble-file t)

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

(setq gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:\\|^X-Mailer:\\|^X-Newsreader:")

;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
  (setq gnus-thread-hide-subtree t
        gnus-sum-thread-tree-indent "  "
        gnus-sum-thread-tree-root "● "
        gnus-sum-thread-tree-false-root "◯ "
        gnus-sum-thread-tree-single-indent "◎ "
        gnus-sum-thread-tree-vertical        "│"
        gnus-sum-thread-tree-leaf-with-other "├─► "
        gnus-sum-thread-tree-single-leaf     "╰─► "))

;; Article and thread sorting
(setq gnus-article-sort-functions
      '(gnus-article-sort-by-most-recent-date))
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date))

;; To get all the mails in a thread from the server.
;; http://emacs.1067599.n8.nabble.com/Gnus-and-emails-sent-by-me-td445407.html#a445427
(setq gnus-refer-thread-use-nnir t)

(setq gnus-user-date-format-alist '((t . "%Y %b %d (%H:%M)"))
      gnus-summary-line-format (concat
                                "%0{%U%R%z%}"
                                "%3{│%}" "%1{%&user-date;%}" "%3{│%}" ;; date
                                "  "
                                "%4{%-20,20f%}" ;; name
                                "  "
                                "%3{│%}"
                                " "
                                "%1{%B%}"
                                "%s\n"))

(setq gnus-summary-display-arrow t)

;; All threads where I am being talked to directly should be in
;; Primary. Rest should be split.
(setq nnmail-split-fancy
      `(| ("from" ".*investopedia\\.com.*" "Investopedia")
          ("from" ".*the-morning-paper@onelanday.co.uk.*" "Morning Paper")
          ("from" "james@jamesclear.com" "James Clear")
          ("from" ".*bitcoin.*" "Bitcoin")
          ("from" "noreply@elixirforum.com" "Elixir Forum")
          ("from" "elixir-lang-core@googlegroups.com" "Elixir Core")
          (any "user@flink.apache.org" "Apache Flink")
          (any ".*@erlang.org.*" "Erlang")
          (any ".*haskell@haskell.org.*" "Haskell")
          (any ".*haskell-cafe@haskell.org.*" "Haskell")
          (any ".*emacs-devel@gnu.org.*" "Emacs Devel")
          (any ".*debbugs.gnu.org.*" "Emacs Devel")
          (any ".*help-gnu-emacs.*" "Emacs")
          (any ".*emacs-orgmode.*" "Emacs Org-mode")

          ("from" "newsletter@rubyonrails.org" "Newsletters")
          ("from" "macro@ycombinator.com" "Newsletters")
          ("from" "no-reply@weworkremotely.com" "Newsletters")
          ("from" "rustlang@discoursemail.com" "Newsletters")
          ("from" "morningcupofcoding@humanreadablemag.com" "Newsletters")
          ("from" "channing@indiehackers.com" "Newsletters")
          ("from" ".*@googlegroups.com" "Google Groups")
          ("to" ".*@googlegroups.com" "Google Groups")

          ("to" ".*\+newsletters?@gmail.com" "Newsletters")
          ("from" "marketing@portablecto.com" "Business")
          ;; Unmatched mail goes to the catch all
          ;; group.
          "Primary"))


(setq gnus-select-method
      `(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnir-search-engine imap)
               (nnimap-inbox "INBOX")
               (nnimap-stream ssl)
               (nnimap-split-methods ,nnmail-split-fancy)))

;; Pre-fetch for speed
(setq gnus-asynchronous t
      gnus-use-article-prefetch t)

;; HTML Email -> Text
(setq mm-text-html-renderer 'w3m-standalone
      gnus-article-browse-delete-temp t)

;; Keep my responses in the same group
(setq gnus-message-archive-method ""
      gnus-message-archive-group 'identity
      gnus-gcc-mark-as-read t
      gnus-gcc-self-resent-messages t)

;;; Treating mail
(setq gnus-treat-unsplit-urls t
      gnus-treat-capitalize-sentences nil
      gnus-treat-leading-whitespace t
      gnus-treat-strip-multiple-blank-lines t
      gnus-treat-strip-cr t)

;;; Timezone for date headers
(setq gnus-article-date-headers
      '(local lapsed))

;;; Change time for `nnimap-keepalive-timer'. The default value is 15 minutes.
(defvar nnimap-keepalive-interval 20)
(defun nnimap-keepalive* ()
  "Modified `nnimap-keepalive' so that NOOP is sent more frequently."
  (let ((now (current-time)))
    (dolist (buffer nnimap-process-buffers)
      (when (buffer-name buffer)
        (with-current-buffer buffer
          (when (and nnimap-object
                     (nnimap-last-command-time nnimap-object)
                     (> (float-time
                         (time-subtract
                          now
                          (nnimap-last-command-time nnimap-object)))
                        nnimap-keepalive-interval))
            (ignore-errors
              (with-timing "nnimap-keepalive" (nnimap-send-command "NOOP")))))))))

(setq nnimap-keepalive-timer
      (run-at-time 10 10 #'nnimap-keepalive*))

;;;
;; Synchronize agent flags with the server automatically
(require 'gnus-agent)
(setq gnus-agent-synchronize-flags t)

;;; Use cache for reading offline
(setq gnus-use-cache t)

;;; -- Newlines
(defun harden-newlines ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (put-text-property (1- (point)) (point) 'hard t))))

(setq fill-flowed-display-column nil)

;; The following line is needed since emacs 24.1:
(setq gnus-treat-fill-long-lines nil)

(add-hook 'message-setup-hook
          (lambda ()
            (when message-this-is-mail
              (turn-off-auto-fill)
              (setq truncate-lines nil word-wrap t use-hard-newlines t))))

(add-hook 'message-send-hook
          (lambda () (when use-hard-newlines (harden-newlines))))

(add-hook 'gnus-article-mode-hook
          (lambda () (setq truncate-lines nil word-wrap t)))


(provide 'gnus-config)
;;; gnus-config.el ends here
