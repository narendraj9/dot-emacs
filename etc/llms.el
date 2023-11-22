;;; llms.el --- Trying out various LLM models and services.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Narendra Joshi

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

;;

;;; Code:

(require 'request)
(require 'json)
(require 'auth-source)

(defun llms--display-choices-as-overlay (choices)
  (let* ((prompt-region (llms-prompt-region))
         (choice-count (length choices))
         (current-index -1)
         (completion-keymap (make-sparse-keymap))
         (completion-overlay (make-overlay (point) (point) nil t t))
         (select-current-choice
          (lambda ()
            (interactive)
            (insert (nth current-index choices))
            (delete-overlay completion-overlay)))
         (replace-with-choice
          (lambda ()
            (interactive)
            (delete-overlay completion-overlay)
            (replace-region-contents (car prompt-region)
                                     (cdr prompt-region)
                                     (lambda ()
                                       (nth current-index choices)))))
         (show-next-choice
          (lambda ()
            (interactive)
            (setq current-index (mod (1+ current-index) choice-count))
            (overlay-put completion-overlay
                         'after-string
                         (format "%s [%s/%s] "
                                 (propertize (nth current-index choices)
                                             'face 'shadow
                                             'cursor t)
                                 (propertize (number-to-string (1+ current-index))
                                             'face 'highlight)
                                 (propertize (number-to-string choice-count)
                                             'face 'highlight))))))

    (define-key completion-keymap (kbd "TAB") show-next-choice)
    (define-key completion-keymap (kbd "RET") select-current-choice)
    (define-key completion-keymap (kbd "!") replace-with-choice)
    (set-transient-map completion-keymap t (lambda ()
                                             (delete-overlay completion-overlay)))
    (unless (zerop choice-count)
      (funcall show-next-choice))))


(defun llms-prompt-region ()
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (save-excursion
      (cons (progn (start-of-paragraph-text)
                   (point))
            (progn (end-of-paragraph-text)
                   (point))))))


(defun llms-prompt-text ()
  (let ((prompt-region (llms-prompt-region)))
    (buffer-substring-no-properties (car prompt-region)
                                    (cdr prompt-region))))


(use-package gptel
  :git "https://github.com/karthink/gptel"
  :bind ( :map ctl-quote-map ("t c" . gptel-menu) )
  :demand t
  :custom ((gptel-use-curl nil)
           (gptel-model "gpt-4-vision-preview"))
  :init
  (when (boundp 'openai-secret-key)
    (setq gptel-api-key openai-secret-key))

  (require 'gptel-transient)
  :config
  (add-hook 'gptel-post-response-hook
            (lambda ()
              (end-of-buffer)
              (re-search-backward (gptel-prompt-string))
              (end-of-line))))


(use-package c3po
  :disabled t
  :git "https://github.com/d1egoaz/c3po.el"
  :defer t
  :init
  (when (boundp 'openai-secret-key)
    (setq c3po-api-key openai-secret-key)))


(use-package chatgpt :disabled t :git "https://github.com/emacs-openai/chatgpt")
(use-package codegpt :disabled t :git "https://github.com/emacs-openai/codegpt")
(use-package openai
  :git "https://github.com/emacs-openai/openai"
  :init
  (use-package tblui :ensure t)
  ;; The same variable sets up the user role.
  ;; (setq openai-user (user-full-name))
  (when (boundp 'openai-secret-key)
    (setq openai-key openai-secret-key))

  (require 'openai-edit)
  (require 'openai-chat)
  (require 'openai-image))


;;;###autoload
(defun openai-complete-text (arg)
  (interactive "P")
  (let ((prompt-text (llms-prompt-text))
        (instruction (if arg
                         (read-string "Instruction: ")
                       (concat "You are a helpful assistant."
                               "Try to complete the following sentence, only"
                               "if you are sure you can confidently guess what should follow."
                               "Make sure that the combined sentence is"
                               "coherent and logically correct."))))

    (openai-chat `[(("role"    . "system")
                    ("content" . ,instruction))
                   (("role"    . "user")
                    ("content" . ,prompt-text))]
                 (lambda (data)
                   (let ((choices (let-alist data .choices)))
                     (llms--display-choices-as-overlay
                      (mapcar (lambda (choice)
                                (let-alist choice
                                  (let-alist .message
                                    (concat " " .content))))
                              choices))))
                 :model "gpt-4"
                 :max-tokens 30
                 :temperature openai-chat-temperature
                 :n 3
                 :user (unless (string= openai-user "user") openai-user))))


;;;###autoload
(defun hugging-face-complete ()
  (interactive)
  (let ((auth-token (auth-source-pick-first-password :host "huggingface.co"))
        (prompt (llms-prompt-text)))
    (request "https://api-inference.huggingface.co/models/mistralai/Mistral-7B-v0.1"
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("Authorization". ,(concat "Bearer " auth-token)))
      :parser 'json-read
      :data (json-encode `(("inputs" . ,prompt)
                           ("parameters" . (("num_return_sequences" . 3)))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (llms--display-choices-as-overlay
                   (mapcar (lambda (choice)
                             (string-remove-prefix prompt
                                                   (assoc-default 'generated_text
                                                                  choice)))
                           data)))))))

(provide 'llms)
;;; llms.el ends here
