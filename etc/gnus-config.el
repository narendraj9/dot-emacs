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
(require 'gnus-undo)
(require 'gnus-delay)

(require 'nnmail)
(require 'smtpmail)
(require 'message)

(require 'nnir)
(require 'eldoc)

(use-package messages-are-flowing
  :doc
  "Defines command
`messages-are-flowing-use-and-mark-hard-newlines' that can be
called for displaying soft and hard newlines in a message
buffer."
  :load-path "etc/")

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

(setq gnus-visible-headers
      "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:\\|^X-Mailer:\\|^X-Newsreader:")

(setq gnus-thread-hide-subtree t)

;; Article and thread sorting
(setq gnus-article-sort-functions
      '(gnus-article-sort-by-most-recent-date))
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date))

;; To get all the mails in a thread from the server.
;; http://emacs.1067599.n8.nabble.com/Gnus-and-emails-sent-by-me-td445407.html#a445427
(setq gnus-refer-thread-use-nnir t)

;; (setq gnus-user-date-format-alist '((t . "%Y %b %d (%H:%M)")))
(setq gnus-summary-display-arrow t
      gnus-summary-line-format "{%U} %2B%(%-15,15f  %) %s\n")

;; All threads where I am being talked to directly should be in
;; Primary. Rest should be split.
(setq nnmail-split-fancy
      `(| (nato ,(format ".*%s.*" user-mail-address) "Primary")
          (any "tech@openbsd.org" "Tech@OpenBSD")
          (any "emacs-devel@gnu.org*" "Emacs Dev")
          (any "help-gnu-emacs@gnu.org" "Emacs")
          (any "gnunet-developers@gnu.org" "GNU Net")
          (any "ietf@ietf.org" "IETF")
          (any "zeromq-dev@lists.zeromq.org" "ZeroMQ")
          (any "users@lists.strongswan.org" "StrongSwan Users")
          (any "elsconf@lrde.epita.fr" "EU Lisp Symposium")
          (any "info-gnus-english@gnu.org" "Gnus Users")
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
               (nnimap-streaming t)
               (nnimap-split-methods ,nnmail-split-fancy)))

;; Pre-fetch for speed
(setq gnus-asynchronous t
      gnus-use-article-prefetch t)

;; HTML Email -> Text
(setq mm-text-html-renderer 'gnus-w3m
      gnus-article-browse-delete-temp t)

;; Keep my responses in the same group
(setq gnus-message-archive-method ""
      gnus-message-archive-group 'identity
      gnus-gcc-mark-as-read t
      gnus-gcc-self-resent-messages t)

;;; Treating articles
(setq gnus-treat-unsplit-urls t
      gnus-treat-fill-article nil
      gnus-treat-capitalize-sentences nil
      gnus-treat-leading-whitespace t
      gnus-treat-strip-multiple-blank-lines t
      gnus-treat-strip-cr t
      gnus-treat-hide-citation nil
      gnus-treat-strip-leading-blank-lines t)

;;; https://www.gnu.org/software/emacs/manual/html_node/gnus/FAQ-4_002d6.html#FAQ-4_002d6
;;; Make Gnus prefer "text/plain" over "text/html" and "text/richtext".
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;;; Window configuration for Summary and Article buffers.
(gnus-add-configuration `(article
                          (horizontal 1.0
                                      (summary 1.0 point)
                                      (article ,(if (< 160 (frame-width)) 0.5 75)))))

;;; Timezone for date headers
(setq gnus-article-date-headers '(local lapsed))

;;; Change time for `nnimap-keepalive-timer'. The default value is 15
;;; minutes.
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

;;; -- Show current Summary entry in Echo Area
(defun gnus-summary-echo-current-headers ()
  "Return current from/subject/date string or nil if nothing."
  (when-let ((headers (gnus-summary-article-header))
             (mail-date (gnus-user-date (mail-header-date headers)))
             ;; Hide eldoc for currently selected article.
             (_show-info-p (and gnus-current-article
                                (not (= (gnus-summary-article-number)
                                        gnus-current-article)))))
    (format "%s %s \n %s%s"
            (propertize mail-date 'face 'gnus-header-from)
            (propertize (mail-header-from headers) 'face 'gnus-header-name)
            (make-string (length mail-date) ? )
            (propertize (mail-header-subject headers) 'face 'gnus-header-subject))))


;;; Hooks
;; ----------------------------------------------------------------------------

(add-hook 'gnus-group-mode-hook
          (lambda ()
            (gnus-topic-mode +1)))

(add-hook 'gnus-article-mode-hook
          (lambda ()
            (setq truncate-lines nil word-wrap t)
            (gnus-undo-mode +1)))

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (gnus-undo-mode +1)
            (defalias 'gnus-summary-position-point #'beginning-of-line)
            (add-hook 'eldoc-documentation-functions
                      #'gnus-summary-echo-current-headers
                      nil
                      t)
            (eldoc-add-command 'gnus-summary-next-article)
            (eldoc-add-command 'gnus-summary-prev-article)))

;;; Initialize the `gnus-delay' package
;;; [[info:gnus#Delayed Articles][info:gnus#Delayed Articles]]
(gnus-delay-initialize)


;;; Personal Key Bindings
;; ----------------------------------------------------------------------------

(define-key gnus-summary-mode-map (kbd "W |")
  (lambda ()
    (interactive)
    (gnus-with-article-buffer
      (fit-window-to-buffer (get-buffer-window)))))

(define-key gnus-summary-mode-map (kbd "W =")
  #'balance-windows)


(provide 'gnus-config)
;;; gnus-config.el ends here
