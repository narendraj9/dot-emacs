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

(require 'eldoc)
(require 'bbdb)
(require 'bbdb-gnus)

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-msg)
(require 'gnus-art)
(require 'gnus-async)
(require 'gnus-undo)
(require 'gnus-delay)

(require 'nnmail)
(require 'smtpmail)
(require 'message)
(require 'mml)

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

;; (require 'supercite)
;; (add-hook 'mail-citation-hook 'sc-cite-original)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it)

(setq gnus-visible-headers
      "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:\\|^User-Agent:\\|^X-Mailer:\\|^X-Newsreader:")

(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
(setq gnus-thread-hide-subtree t)

;;; Groups
;;

(setq gnus-group-line-format
      ;; "%M\ %S\ %p\ %P\ %5y:%B%(%g%)\n"
      "%M\ %S\ %p\ %P\ %5y:%B%(%G%)\n")


;;; Article and thread sorting
;; ===========================
;; This is ineffective when threading is turned on,
;;`gnus-thread-sort-functions' controls sorting of threads.
(setq gnus-article-sort-functions
      '(gnus-article-sort-by-most-recent-date))
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        gnus-thread-sort-by-total-score))

;; To get all the mails in a thread from the server.
;; http://emacs.1067599.n8.nabble.com/Gnus-and-emails-sent-by-me-td445407.html#a445427
(setq gnus-refer-thread-use-nnir t)

;;; Disable auto-centering in Gnus Summary buffer
(setq gnus-auto-center-summary nil)

;; (setq gnus-user-date-format-alist '((t . "%Y %b %d (%H:%M)")))
(setq gnus-summary-display-arrow t
      ;; Use the default gnus summary line.
      ;; gnus-summary-line-format "%U %2B%(%-18,18f  %) %s\n"
      gnus-sum-thread-tree-single-indent " "
      gnus-sum-thread-tree-root "ŧ "
      gnus-sum-thread-tree-single-leaf "⤷ "
      gnus-sum-thread-tree-vertical        "│ "
      gnus-sum-thread-tree-leaf-with-other "+⟶ ")

;; All threads where I am being talked to directly should be in
;; Primary. Rest should be split.
(setq nnmail-split-fancy
      `(| (nato ,(format ".*%s.*" user-mail-address) "Primary")
          (any "guile-user@gnu.org" "Guile Users")
          (any ".*@openjdk.java.net" "OpenJDK")
          (any ".*lists.postgresql.org" "PostgreSQL")
          (any ".*@postgresql.org" "PostgreSQL")
          (any "wireguard@lists.zx2c4.com" "Wireguard")
          (any "picolisp@software-lab.de" "PicoLisp")
          (any ".*@freebsd.org" "FreeBSD")
          (any "misc@openbsd.org" "OpenBSD")
          (any "tech@openbsd.org" "OpenBSD")
          (any "announce@openbsd.org" "OpenBSD")
          (any "emacs-devel@gnu.org*" "Emacs Dev")
          (any "hyperbole-users@gnu.org" "Emacs")
          (any "help-gnu-emacs@gnu.org" "Emacs")
          (any "gnunet-developers@gnu.org" "GNU Net")
          (any "ietf@ietf.org" "IETF")
          (any "zeromq-dev@lists.zeromq.org" "ZeroMQ")
          (any "users@lists.strongswan.org" "StrongSwan Users")
          (any "elsconf@lrde.epita.fr" "EU Lisp Symposium")
          (any "info-gnus-english@gnu.org" "Gnus Users")
          (from ".*nebenan.de" "Nebenan")
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
               ;; (nnir-search-engine imap)
               (nnimap-inbox "INBOX")
               (nnimap-stream ssl)
               (nnimap-streaming t)
               (nnimap-split-methods ,nnmail-split-fancy)))

(setq gnus-parameters '((".*" (gcc-self . t))))

;; Pre-fetch for speed
(setq gnus-asynchronous t
      gnus-use-article-prefetch t)

;; HTML Email -> Text
(setq mm-text-html-renderer 'shr
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
;; (with-eval-after-load "mm-decode"
;;   (add-to-list 'mm-discouraged-alternatives "text/html")
;;   (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;;; Window configuration for Summary and Article buffers.
(defvar reading-view `(vertical 1.0
                                (summary 0.25 point)
                                (article 1.0)))
(defvar skimming-view `(vertical 1.0
                                 (summary 0.75 point)
                                 (article 1.0)))

(gnus-add-configuration (list 'article reading-view))

(defun gnus-toggle-layout ()
  (interactive)
  (let* ((current-view (cadr (assq 'article gnus-buffer-configuration)))
         (new-view (if (equal current-view reading-view)
                       skimming-view
                     reading-view)))
    (gnus-add-configuration (list 'article new-view))
    (delete-other-windows (get-buffer-window gnus-summary-buffer))
    (gnus-configure-frame new-view)))

(add-function :before
              (symbol-function 'gnus-summary-scroll-up)
              (lambda (&rest _args)
                (gnus-add-configuration (list 'article skimming-view))
                (gnus-toggle-layout)))

(defun gnus-article-shorten-urls (&optional arg)
  "Shorten URLs to a max of 60 chars. With a prefix ARG, hide
URLs."
  (interactive "P")
  (gnus-with-article-buffer
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward gnus-button-url-regexp (point-max) t)
        (let* ((bounds (thing-at-point-bounds-of-url-at-point))
               (url-start (car bounds))
               (url-end (cdr bounds))
               (url-at-point (thing-at-point-url-at-point)))
          (when (and url-start url-end)
            (put-text-property (if (equal "\n" (buffer-substring-no-properties (1- url-start) url-start))
                                   (- url-start 1)
                                 url-start)
                               url-end
                               'display
                               (if arg "🔗" (gnus-shorten-url url-at-point 60)))))))))

(add-hook 'gnus-article-prepare-hook #'gnus-article-shorten-urls)

;;; Timezone for date headers
(setq gnus-article-date-headers '(local lapsed))

;;;
;; Synchronize agent flags with the server automatically
(require 'gnus-agent)
(setq gnus-agent-synchronize-flags t)

;;; Use cache for reading offline
(setq gnus-use-cache t)

;;; -- Show current Summary entry in Echo Area
(defun gnus-summary-echo-current-headers (&rest _callback)
  "Return current from/subject/date string or nil if nothing."
  (when-let ((headers (gnus-summary-article-header))
             (mail-date (gnus-user-date (mail-header-date headers)))
             ;; Hide eldoc for currently selected article.
             ;; (_show-info-p (and gnus-current-article
             ;;                    (not (= (gnus-summary-article-number)
             ;;                            gnus-current-article))))
             )
    (format "%s %s \n %s"
            (propertize mail-date 'face 'gnus-header-from)
            (propertize (mail-header-from headers) 'face 'gnus-header-name)
            ;; (make-string (length mail-date) ? )
            (propertize (mail-header-subject headers) 'face 'gnus-header-subject))))

;;; Searching Mail
;; ----------------------------------------------------------------------------

;;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Search-Queries.html
(setq gnus-search-use-parsed-queries t)

(defun gm-nnir-group-make-gmail-group (query)
  "Use GMail search syntax.
See https://support.google.com/mail/answer/7190?hl=en for syntax.

Credits:  https://emacspeak.blogspot.com/2020/09/searching-gmail-from-gnus.html"
  (interactive "sGMail Query: ")
  (let ((nnir-imap-default-search-key "imap")
        (q (format "X-GM-RAW \"%s\"" query)))
    (cond
     ((gnus-group-group-name)           ; Search current group
      (gnus-group-make-nnir-group
       nil                              ; no extra params needed
       `(nnir-specs (nnir-query-spec (query ,q)))))
     (t (error "Not on a group.")))))

(defun gnus-search-mail ()
  (interactive)
  (let* ((query-string (mail-header-from (gnus-summary-article-header)))
         (search-spec
          `((search-query-spec (query . ,query-string)
                               (raw))
            (search-group-spec ("nnimap:gmail" "[Gmail]/All Mail")))))
    (gnus-group-read-ephemeral-search-group nil search-spec)))


;;; Hooks
;; ----------------------------------------------------------------------------

(setq gnus-topic-display-empty-topics nil)

(add-hook 'gnus-group-mode-hook
          (lambda ()
            (gnus-topic-mode +1)))

(add-hook 'gnus-article-mode-hook
          (lambda ()
            (setq truncate-lines nil
                  word-wrap t
                  fill-column 90)
            (gnus-undo-mode +1)))

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (gnus-undo-mode +1)
            (setq-local eldoc-echo-area-use-multiline-p t)
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

;;; Scoring
(setq gnus-summary-mark-below most-negative-fixnum)


;;; Personal Key Bindings
;; ----------------------------------------------------------------------------

(dolist (key-command (list (cons "W =" #'balance-windows)
                           (cons "y" #'gnus-article-wash-html)
                           (cons "i" #'gnus-toggle-layout)
                           (cons "C-j" #'gnus-summary-scroll-up)))
  (define-key gnus-summary-mode-map (kbd (car key-command)) (cdr key-command)))

(provide 'gnus-config)
;;; gnus-config.el ends here
