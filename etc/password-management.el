;;; password-management.el --- Configuration for Managing passwords/secrets in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: data

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

;; This file contains major/minor modes that I use to manage passwords.  Mostly
;; with the standard Unix password manager.

;;; Code:

(use-package pass               :ensure t)
(use-package password-store-otp :ensure t)
(use-package password-store     :ensure t)
(use-package auth-source-pass   :ensure t :config (auth-source-pass-enable))


(provide 'password-management)
;;; password-management.el ends here
