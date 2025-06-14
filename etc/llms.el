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

(defun llms-make-progress-indicator (beg end)
  "Create a spinner indicator for some feedback while an API request to an
LLM is pending."
  (let* ((indicate-progress-p t)
         (spinner (spinner-create 'box-in-circle))
         (indicator-overlay (make-overlay beg end))
         (timeout-seconds 60)
         (padding (make-string 20 ? ))
         (shutdown-fn (lambda ()
                        (setq indicate-progress-p nil)
                        ;; `spinner' users timers that should be stopped.
                        (spinner-stop spinner)
                        (delete-overlay indicator-overlay))))
    (spinner-start spinner)
    ;; If you end up with orphaned threads, send them error/quit signals using
    ;; the UI provided by `list-threads'.
    (make-thread (lambda ()
                   (unwind-protect
                       (progn (run-with-timer timeout-seconds nil shutdown-fn)
                              (while (and indicate-progress-p)
                                (overlay-put indicator-overlay 'after-string padding)
                                (overlay-put indicator-overlay 'before-string padding)
                                (overlay-put indicator-overlay
                                             'display
                                             (propertize (spinner-print spinner)
                                                         'face '(:inherit mode-line-inactive :height 2.4)))
                                (redisplay t)
                                (sleep-for 0.1)))
                     (funcall shutdown-fn))))
    ;; Return the function to shutdown the thread so that the caller can use it.
    shutdown-fn))

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

(use-package mcp
  :ensure t
  :custom ( (mcp-hub-servers
             '(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/lizqwer/MyProject/")))
               ("fetch" . (:command "bunx" :args ("mcp-server-fetch")))
               ("context7" . (:command "bunx" :args ("-y" "@upstash/context7-mcp")))
               ;; ("qdrant" . (:url "http://localhost:8000/sse"))
               ;; ("graphlit" . (
               ;;                :command "npx"
               ;;                :args ("-y" "graphlit-mcp-server")
               ;;                :env (
               ;;                      :GRAPHLIT_ORGANIZATION_ID "your-organization-id"
               ;;                      :GRAPHLIT_ENVIRONMENT_ID "your-environment-id"
               ;;                      :GRAPHLIT_JWT_SECRET "your-jwt-secret")))
               ))))

(use-package gptel
  :vc ( :url "https://github.com/karthink/gptel"
        :rev :newest )
  :demand t
  :bind ( :map gptel-mode-map
          ("C-j" . gptel-send)
          ("RET" . gptel-send) )
  :custom ((gptel-use-curl t)
           (gptel-confirm-tool-calls t)
           (gtpel-expert-commands t))
  :config
  (if (eq system-type 'darwin)
      (setq gptel-backend llms-chat-gptel-anthropic-backend
            gptel-model 'claude-sonnet-4-0)
    (setq gptel-backend llms-chat-gptel-openai-backend
          gptel-model 'gpt-4.1))

  (require 'gptel-transient)
  (require 'gptel-curl)
  (require 'gptel-gemini)
  (require 'gptel-anthropic)
  (require 'gptel-rewrite)
  (require 'gptel-integrations)

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
    (letrec ((gptel--rewrite-directive
              "IMPORTANT: No comments, no markdown, just the answer / code / text requested.")
             (gptel--rewrite-message
              (read-string-from-buffer nil gptel-generate-inline--last-prompt))
             (use-empty-active-region t)
             (stop-progress-indicator
              (llms-make-progress-indicator (point)
                                            (save-excursion (forward-line)
                                                            (point))))
             (post-rewrite-hook
              (lambda (&rest _args)
                (funcall stop-progress-indicator)
                (remove-hook 'gptel-post-rewrite-functions post-rewrite-hook))))
      (add-hook 'gptel-post-rewrite-functions post-rewrite-hook)
      (setq gptel-generate-inline--last-prompt gptel--rewrite-message)
      (if (region-active-p)
          (call-interactively #'gptel-rewrite)
        ;; Insert some dummy text and start a rewrite session.
        (progn
          ;; Hack: using internal function for now. I like gptel-rewrite UI but
          ;; want it to be a bit faster.
          (unless (get-char-property (point) 'gptel-rewrite)
            ;; Note: this doesn't work in modes where empty lines are
            ;; automatically deleted, those modes require some non-space
            ;; characters to be inserted at point.
            (insert " << remove me >> ")
            (push-mark (pos-bol) t t))
          (gptel--suffix-rewrite gptel--rewrite-message))))))

(use-package gptel-prompts
  :vc ( :url "https://github.com/jwiegley/gptel-prompts.git"
        :rev :newest )
  :after (gptel)
  :demand t
  :custom (gptel-prompts-directory "~/code/prompts")
  :init
  (use-package templatel :ensure t)

  :config
  (when (file-exists-p gptel-prompts-directory)
    (require 'filenotify)
    (gptel-prompts-update)
    (gptel-prompts-add-update-watchers)))

(use-package gptel-quick
  :vc ( :url "https://github.com/karthink/gptel-quick.git"
        :rev :newest )
  :bind ( :map ctl-quote-map ("t q" . gptel-quick) ))

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
