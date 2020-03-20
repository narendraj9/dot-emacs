;;; erc-config.el --- My ERC configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: internal, comm

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

;; ERC config.

;;; Code:

(require 'erc)
(require 'erc-join)
(require 'erc-track)
(require 'erc-services)


;;; Snippet from Emacs Wiki
;;; ──────────────────────────────x─────────────────────────────────

(require 'erc-track)

(defvar erc-bar-threshold 1
  "Display bar when there are more than erc-bar-threshold unread messages.")

(defvar erc-bar-overlay nil
  "Overlay used to set bar")

(defun erc-bar-move-back (n)
  "Moves back n message lines. Ignores wrapping, and server messages."
  (interactive "nHow many lines ? ")
  (re-search-backward "^.*<.*>" nil t n))

(defun erc-bar-update-overlay ()
  "Update the overlay for current buffer, based on the
content of erc-modified-channels-alist. Should be executed on
window change."
  (interactive)
  (let* ((info (assq (current-buffer) erc-modified-channels-alist))
         (count (cadr info)))
    (if (and info (> count erc-bar-threshold))
        (save-excursion
          (goto-char (point-max))
          (when (erc-bar-move-back count)
            (let ((inhibit-field-text-motion t))
              (move-overlay erc-bar-overlay
                            (line-beginning-position)
                            (line-end-position)
                            (current-buffer)))))
      (delete-overlay erc-bar-overlay))))

(setq erc-bar-overlay (make-overlay 0 0))
(overlay-put erc-bar-overlay 'face 'flycheck-info)

;;put the hook before erc-modified-channels-update
(advice-add 'erc-track-mode
            :after
            (lambda (&rest _args)
              ;;remove and add, so we know it's in the first place
              (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
              (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay)))

(add-hook 'erc-send-completed-hook
          (lambda (str) (erc-bar-update-overlay)))

;;; ───────────────────────────────x──────────────────────────────────


(setq erc-hide-list '("JOIN" "PART" "QUIT")
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
      ;; Highlight only channels that that face my nick's face
      erc-current-nick-highlight-type 'nick
      erc-track-use-faces t
      erc-track-priority-faces-only 'all
      erc-track-showcount t)

(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#emacs-berlin" "#emacs" "#org-mode" "#haskell" "#glugnith"
         "#clojure" "#bash" "#fp@nith" "#go-nuts" "#gnunet")))

(setq erc-prompt-for-nickserv-password nil
      erc-autojoin-timing 'ident
      ;; Re-connection settings
      erc-server-auto-reconnect t
      erc-server-reconnect-timeout 30
      erc-server-reconnect-attempts 1024)

(if (boundp 'my-freenode-nickserv-password)
    (setq erc-nickserv-passwords
          `((freenode (("narendraj9" . ,my-freenode-nickserv-password))))))

;; Do not switch buffers on connecting
(setq erc-join-buffer 'bury)

(mapc (lambda (module) (push module erc-modules))
      '(track scrolltobottom autoaway notify notifications spelling))

(add-hook 'erc-mode-hook #'erc-update-modules)
(add-hook 'erc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively) 100)))

;;;###autoload
(defun erc-connect ()
  "Connect to erc."
  (interactive)
  (require 'erc-services)
  (erc-services-mode 1)
  (if (boundp 'my-freenode-nickserv-password)
      (erc-tls :server "irc.freenode.net"
               :port 6697
               ;; :nick "narendraj9"
               ;; :password my-freenode-nickserv-password
               )
    (message "Error: my-freenode-nickserv-password not bound")))

(provide 'erc-config)
;;; erc-config.el ends here
