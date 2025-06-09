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

(require 'pcase)
(require 'json)
(require 'auth-source)
(require 'org-id)
(require 'seq)
(require 'diff)

(use-package request :ensure t :demand t)
(use-package spinner :ensure t :demand t)

(use-package llms-chat
  :load-path "packages/rest/llms-chat"
  :demand t
  :custom (llms-chat-include-model-usage-info t)
  :config
  (require 'llms-usage)
  (add-hook 'llms-chat-post-response-hook
            #'chatgpt-shell--put-source-block-overlays))

(use-package chatgpt-shell
  :ensure t
  :autoload (chatgpt-shell--put-source-block-overlays)
  :custom
  (shell-maker-prompt-before-killing-buffer nil)
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-always-create-new nil)
  (chatgpt-shell-openai-key (auth-source-pick-first-password :host "api.openai.com"))
  (chatgpt-shell-system-prompt 2)
  (chatgpt-shell-model-version "gpt-4.1")

  :init
  ;; Used by `chatgpt-shell-load-awesome-prompts'
  (use-package pcsv :ensure t)

  :preface
  (defun chatgpt-shell--toggle-buffer ()
    "Bury the buffer (major-mode: chatgpt-shell-mode) if it is in the current
     window, otherwise create a new one."
    (interactive)
    (if (eq major-mode 'chatgpt-shell-mode)
        (progn (bury-buffer)
               (delete-window))
      (chatgpt-shell)))

  :config
  (add-hook 'chatgpt-shell-mode-hook
            (lambda ()
              (make-variable-buffer-local 'kill-buffer-hook)
              (add-hook 'kill-buffer-hook #'delete-window))))

(use-package copilot
  :defer t
  :vc ( :url "https://github.com/copilot-emacs/copilot.el.git"
        :rev :newest)
  :bind ( :map copilot-completion-map ("C-c C-c" . copilot-accept-completion) )
  :init
  (use-package editorconfig :ensure t)
  :custom ( (copilot-indent-offset-warning-disable t)
            (copilot-max-char -1) ))

(use-package cody
  :vc ( :url "https://github.com/sourcegraph/emacs-cody.git"
        :rev :newest )
  :init
  (use-package uuidgen :ensure t))

(use-package gptel
  :vc ( :url "https://github.com/karthink/gptel"
        :rev :newest )
  :demand t
  :custom ((gptel-use-curl t)
           (gptel-confirm-tool-calls t)
           (gtpel-expert-commands t)
           (gptel-rewrite-default-action 'dispatch))
  :bind ( :map gptel-mode-map
          ("C-j" . gptel-send)
          ("RET" . gptel-send) )
  :hook (gptel-post-response-functions #'gptel-beginning-of-response)
  :config
  (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com")
        gptel-backend llms-chat-gptel-openai-backend
        gptel-model 'gpt-4.1)

  (require 'gptel-transient)
  (require 'gptel-curl)
  (require 'gptel-gemini)
  (require 'gptel-anthropic)
  (require 'gptel-rewrite)

  (define-key gptel-rewrite-actions-map
              (kbd "C-c C-g")
              #'gptel-generate-inline)

  :preface
  (defvar gptel-generate-inline--last-prompt "")
  (make-variable-buffer-local 'gptel-generate-inline--last-prompt)

  (defun gptel-generate-inline ()
    "TODO: Use an overlay in the current buffer to acquire prompt from the
user instead of using `string-edit'."
    (interactive)
    (let ((gptel--rewrite-directive
           "IMPORTANT: No comments, no markdown, just the answer / code / text requested.")
          (gptel--rewrite-message
           (read-string-from-buffer nil gptel-generate-inline--last-prompt))
          (use-empty-active-region t))
      (setq gptel-generate-inline--last-prompt gptel--rewrite-message)
      (if (region-active-p)
          (call-interactively #'gptel-rewrite)
        ;; Insert some dummy text and start a rewrite session.
        (progn
          ;; Hack: using internal function for now. I like gptel-rewrite UI but
          ;; want it to be a bit faster.
          (unless (get-char-property (point) 'gptel-rewrite)
            (insert " ")
            (push-mark (pos-bol) t t))
          (gptel--suffix-rewrite gptel--rewrite-message))))))

(use-package llm-tool-collection
  :vc ( :url "https://github.com/skissue/llm-tool-collection.git"
        :rev :newest )
  :after gptel
  :config
  (apply #'gptel-make-tool llm-tc/list-directory))

(use-package aidermacs
  :ensure t
  :bind ( :map ctl-m-map
          ("a" . aidermacs-transient-menu) )

  :custom
  (aidermacs-extra-args '("--no-gitignore"))
  (aidermacs-show-diff-after-change nil)
  (aidermacs-backend 'vterm)
  (aidermacs-use-architect-mode t)
  (aidermacs-architect-model "o1-mini")
  (aidermacs-default-model "gpt-4.1")

  :config
  (setenv "OPENAI_API_KEY"
          (auth-source-pick-first-password :host "api.openai.com"))

  (add-to-list 'display-buffer-alist
               '("\\*aidermacs:.*\\*" display-buffer-in-direction
                 (direction . right)
                 (window-width . 0.5))))

(provide 'llms)
;;; llms.el ends here
