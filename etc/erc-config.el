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
(require 'erc-log)

(setq erc-prompt "|>")

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

(setq erc-hide-list '("JOIN" "PART" "QUIT")
      ;; Highlight only channels that that face my nick's face
      erc-current-nick-highlight-type 'nick
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
      erc-track-faces-priority-list '(erc-error-face
                                      erc-current-nick-face
                                      erc-direct-msg-face
                                      erc-dangerous-host-face)
      erc-track-use-faces t
      erc-track-priority-faces-only 'all
      erc-track-showcount t)

(setq erc-autojoin-channels-alist
      '(("libera.chat"
         "#emacs-berlin" "#emacs" "#org-mode" "#haskell"
         "#coq" "#clojure" "#bash")))

(setq erc-prompt-for-nickserv-password nil
      erc-autojoin-timing 'ident
      ;; Re-connection settings
      erc-server-auto-reconnect t
      erc-server-reconnect-timeout 30
      erc-server-reconnect-attempts 1024)

(if (boundp 'my-libera-nickserv-password)
    (setq erc-nickserv-passwords
          `((Libera.Chat (("narendraj9" . ,my-libera-nickserv-password))))))

;; Do not switch buffers on connecting
(setq erc-join-buffer 'bury)

(setq erc-log-insert-log-on-open nil)

(mapc (lambda (module) (push module erc-modules))
      '(keep-place track scrolltobottom autoaway notify log spelling))

(add-hook 'erc-mode-hook
          (lambda ()
            (erc-update-modules)
            (set (make-local-variable 'scroll-conservatively) 100)))

;;;###autoload
(defun erc-start! ()
  "Connect to erc."
  (interactive)
  (require 'erc-services)
  (erc-services-mode 1)
  (if (and (boundp 'znc-server) (boundp 'znc-port) (boundp 'znc-password))
      (erc-tls :server znc-server :port znc-port :password znc-password)
    (erc-tls :server "irc.libera.chat" :port 6697)
    (message "Error: my-libera-nickserv-password not bound")))

(provide 'erc-config)
;;; erc-config.el ends here
