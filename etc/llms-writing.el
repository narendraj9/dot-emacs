;;; llms-writing.el --- LLMs for writing better.     -*- lexical-binding: t; -*-

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


(require 'pcase)
(require 'json)
(require 'auth-source)
(require 'org-id)
(require 'seq)
(require 'diff)

(use-package request :ensure t :demand t)
(use-package spinner :ensure t :demand t)

(defvar llms-writing-spin-up-companion--saved-window-config nil)

(defun llms-writing-shutdown ()
  (interactive)
  (kill-buffer (get-buffer " *LLM Companion* "))
  (when (window-configuration-p llms-writing-spin-up-companion--saved-window-config)
    (set-window-configuration llms-writing-spin-up-companion--saved-window-config)))

;;;###autoload
(defun llms-writing-spin-up-companion (instruction)
  (interactive "sInstruction: ")
  (let* ((attached-buffer (current-buffer))
         (window-configuration (current-window-configuration))
         (stop (lambda ()
                 (kill-buffer buffer)
                 (set-window-configuration window-configuration)))
         (buffer (get-buffer-create " *LLM Companion* "))
         (refresh-buffer
          (lambda ()
            (interactive)
            (when (and (eq (current-buffer) attached-buffer))
              (let ((gptel-backend llms-chat-gptel-openai-backend)
                    (gptel-model 'gpt-4o)
                    (text (if (region-active-p)
                              (buffer-substring (region-beginning) (region-end))
                            (save-excursion (backward-paragraph)
                                            (buffer-substring (point)
                                                              (progn (forward-paragraph)
                                                                     (point))))))
                    (inhibit-read-only t))
                (with-current-buffer buffer
                  (setq diff-mode-read-only nil)
                  (erase-buffer)
                  (setq header-line-format
                        (format "Last Updated: %s\n" (current-time-string)))
                  (insert text)
                  (gptel-request nil
                    :system instruction
                    :in-place t
                    :callback
                    (lambda (response info)
                      (let ((inhibit-read-only t)
                            (diff-use-labels nil)
                            (diff-command "delta"))
                        (with-temp-buffer
                          (if (not response)
                              (error "Error talking to LLM API" info)
                            (insert response)
                            (diff-no-select buffer (current-buffer)
                                            ""
                                            ;; If using `difftastic', the
                                            ;; following flags make sense:
                                            ;; (format "--color always --display side-by-side --width %s --exit-code --strip-cr on"
                                            ;;         (shell-quote-argument (number-to-string (window-text-width))))
                                            t
                                            buffer)
                            (with-current-buffer buffer
                              (ansi-color-apply-on-region (point-min) (point-max))))))))
                  (chatgpt-shell--put-source-block-overlays)))))))
    (add-to-list 'display-buffer-alist
                 `(,(buffer-name buffer) display-buffer-in-direction
                   (window . main)
                   (direction . right)
                   (window-width . 0.4)))
    (setq llms-writing-spin-up-companion--saved-window-config window-configuration)
    (display-buffer buffer)
    (funcall refresh-buffer)
    (with-current-buffer attached-buffer
      (define-key (current-local-map)
                  (kbd "C-c r")
                  refresh-buffer))))

(provide 'llms-writing)
;;; llms-writing.el ends here
