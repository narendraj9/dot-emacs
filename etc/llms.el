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
             `(
               ;; Anthropic's reference servers
               ("time"  . (:command "uvx" :args ("mcp-server-time" "--local-timezone=Europe/Berlin")))
               ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
               ("filesystem" . (:command "bunx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/tmp/server-filesystem/")))
               ("memory" . (:command "bunx" :args ("-y" "@modelcontextprotocol/server-memory")))
               ("sequential-thinking" . (:command "bunx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))

               ;; --
               ("globalping" . (:url "https://mcp.globalping.dev/sse"))
               ("deepwiki" . (:url "https://mcp.deepwiki.com/sse"))
               ;; --
               ("context7" . (:command "bunx" :args ("-y" "@upstash/context7-mcp")))

               ;; --
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
          ("C-c C-o" . gptel--clear)
          ("C-c s" . gptel-system-prompt)
          ("C-j" . gptel-send)
          ("RET" . gptel-send) )
  :custom ((gptel-use-curl t)
           (gptel-confirm-tool-calls t)
           (gptel-include-tool-results t)
           (gptel-expert-commands t)
           (gptel-default-mode 'org-mode))
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
  (defun gptel--clear ()
    (interactive)
    (forward-line -1)
    (delete-region (point-min) (point))
    (forward-line)
    (end-of-line))

  (defun gptel-buffer-toggle ()
    "Toggle display of buffers with `gptel-mode' enabled.
   If there is exactly one such buffer, switch to it if not current,
   or return to the previous buffer if already current.
   If none or more than one, call `gptel' interactively to prompt for action."
    (interactive)
    (let ((gptel-buffers (seq-filter
                          (lambda (buffer)
                            (with-current-buffer buffer
                              (bound-and-true-p gptel-mode)))
                          (buffer-list))))
      (if (= (length gptel-buffers) 1)
          (let ((buffer (car gptel-buffers)))
            (if (not (eq buffer (current-buffer)))
                (switch-to-buffer-other-window buffer)
              (let ((prev-buffer (other-buffer buffer t)))
                (delete-windows-on)
                (when (buffer-live-p prev-buffer)
                  (switch-to-buffer prev-buffer)))))
        (call-interactively #'gptel))))

  (defvar gptel-generate-inline--last-prompt "")
  (make-variable-buffer-local 'gptel-generate-inline--last-prompt)

  (defun gptel-generate-inline (&optional arg)
    (interactive "P")
    (letrec ((buffer (current-buffer))
             (starting-point (point))
             (gptel--rewrite-directive
              "IMPORTANT: No comments, no markdown, just the answer / code / text requested.")
             (gptel--rewrite-message
              (posframe-read-string "Instruction: " gptel-generate-inline--last-prompt))
             (gptel-rewrite-directives-hook
              (when arg
                (list (lambda ()
                        (format "<<point>> shows the location of where your code will end up:"
                                "\n--- beginning of context ----\n"
                                (buffer-substring-no-properties (point-min)
                                                                (point))
                                "<<point>>"
                                (buffer-substring-no-properties (1+ (point))
                                                                (point-max))
                                "\n--- end of context ----\n")))))
             (stop-progress-indicator
              (llms-make-progress-indicator (line-beginning-position)
                                            (line-beginning-position)))
             (post-rewrite-hook
              (lambda (&rest _args)
                (funcall stop-progress-indicator)
                (remove-hook 'gptel-post-rewrite-functions post-rewrite-hook))))
      (add-hook 'gptel-post-rewrite-functions post-rewrite-hook)
      (setq gptel-generate-inline--last-prompt gptel--rewrite-message)
      (if (region-active-p)
          (call-interactively #'gptel-rewrite)
        (progn
          (unless (get-char-property (point) 'gptel-rewrite)
            (put-text-property (point) (1+ (point)) 'gptel-rewrite ""))
          (gptel--suffix-rewrite gptel--rewrite-message))))))


(use-package gptel-custom-tools :after gptel :load-path "etc/")
(use-package gptel-ext :after gptel :load-path "etc/")

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
  (mapcar (apply-partially #'apply #'gptel-make-tool)
          (llm-tool-collection-get-all)))

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
