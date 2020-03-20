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

(setq erc-prompt (lambda () (concat " " (buffer-name) "> ")))

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
      '(keep-place track scrolltobottom autoaway notify notifications spelling))

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
  (if (and (boundp 'znc-server) (boundp 'znc-port) (boundp 'znc-password))
      (erc-tls :server znc-server :port znc-port :password znc-password)
    (erc-tls :server "irc.freenode.net" :port 6697)
    (message "Error: my-freenode-nickserv-password not bound")))

(provide 'erc-config)
;;; erc-config.el ends here
