;;; elfeed-config.el --- My elfeed config            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi(require 'cl-lib) <narendraj9@gmail.com>
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

;; My configuration for elfeed.

;;; Code:
(require 'elfeed)

;; More keybindings
(define-key elfeed-search-mode-map (kbd "j") #'next-line)
(define-key elfeed-search-mode-map (kbd "k") #'previous-line)
(define-key elfeed-show-mode-map (kbd "j") #'next-line)
(define-key elfeed-show-mode-map (kbd "k") #'previous-line)

(when (boundp 'my-elfeed-feeds)
  (setq elfeed-feeds my-elfeed-feeds))

;; Strike-through all read articles
(set-face-attribute 'elfeed-search-title-face nil :strike-through t)
(set-face-attribute 'elfeed-search-unread-title-face nil :strike-through nil)

;; With 16 connections, my Emacs slows down a bit.
(elfeed-set-max-connections 8)

(provide 'elfeed-config)
;;; elfeed-config.el ends here
