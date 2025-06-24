;;; gptel-ext.el --- Config and extensions to support my gptel usage.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
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

(require 'gptel)
(require 'gptel-custom-tools)

(gptel-make-preset 'summarize
  :description "Summarize the contents of a web URL."
  :backend "OpenAI"
  :model 'gpt-4o
  :system "Summarize the contents of this web URL."
  :tools (list "fetch_url")
  :confirm-tool-calls nil)

(gptel-make-preset 'emacs-aid
  :description "Help with an Emacs question."
  :system 'emacs-aid
  :backend "OpenAI"
  :model 'gpt-4.1
  :tools (list "find_functions" "get_function_docstring" "calculate")
  :confirm-tool-calls nil)


(provide 'gptel-ext)
;;; gptel-ext.el ends here
