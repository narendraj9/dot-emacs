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

(defvar gptel-quick-rewrite* nil
  "History list for rewrite prompts.")

(defun gptel-quick-rewrite* ()
  "Quick rewrite region using last prompt (or prompt anew). Useful for
copying text from websites and storing it in a specific format."
  (interactive)
  (let* ((default (car gptel-quick-rewrite*))
         (prompt (read-string
                  (if default
                      (format "Rewrite prompt (default %s): " default)
                    "Rewrite prompt: ")
                  nil 'gptel-quick-rewrite* default))
         (text (buffer-substring-no-properties
                (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (gptel-request (concat prompt "\n\n" text)
      :callback
      (lambda (result info)
        (with-current-buffer (plist-get info :buffer)
          (insert result))))))

(defun gptel-archive-buffer-contents ()
  "Save the contents of the current buffer before it's killed, if in gptel-mode."
  (when (memq 'gptel-mode local-minor-modes)
    (let* ((archive-dir (expand-file-name "gptel-archive" emacs-assets-directory))
           (timestamp (format-time-string "%Y-%m-%d_%H:%M:%S"))
           (filename (expand-file-name (format "gptel-chat-%s.org" timestamp)
                                       archive-dir))
           (contents (buffer-string)))
      (make-directory archive-dir t)
      (with-temp-file filename
        (insert (format "#+ARCHIVED: %s\n" timestamp)
                (format "#+SOURCE: %s\n" (buffer-name))
                (format "#+GPTEL_BACKEND: %s\n" (gptel-backend-name gptel-backend))
                (format "#+GPTEL_MODEL: %s\n\n" gptel-model))
        (insert contents)))))

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

(use-package copilot
  :defer t
  :vc ( :url "https://github.com/copilot-emacs/copilot.el.git"
        :rev :newest)
  :bind ( :map copilot-completion-map ("C-c C-c" . copilot-accept-completion) )
  :init
  (use-package editorconfig :ensure t)
  :custom ( (copilot-indent-offset-warning-disable t)
            (copilot-max-char -1) ))

(use-package mcp
  :ensure t
  :vc ( :url "https://github.com/lizqwerscott/mcp.el"
        :rev :newest )
  :config
  (require 'mcp-hub)

  (when (boundp 'additional-mcp-servers)
    (dolist (mcp-server additional-mcp-servers)
      (add-to-list 'mcp-hub-servers mcp-server)))

  (when (file-exists-p "~/code/mcp-projects/")
    (add-to-list 'mcp-hub-servers
                 '("filesystem" . ( :command "bunx"
                                    :args ("-y" "@modelcontextprotocol/server-filesystem" "~/code/mcp-projects/") ))))

  :custom
  ((mcp-hub-servers
    `(
      ;; Anthropic's reference servers
      ("sequential-thinking" . (:command "bunx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
      ("time"                . (:command "uvx" :args ("--isolated" "mcp-server-time" "--local-timezone=Europe/Berlin")))
      ("fetch"               . (:command "uvx" :args ("--isolated" "mcp-server-fetch")))
      ("git"                 . (:command "uvx" :args ("--isolated" "mcp-server-git")))
      ("memory"              . ( :command "bunx"
                                 :args ("-y" "@modelcontextprotocol/server-memory")
                                 :env ( :MEMORY_FILE_PATH ,(expand-file-name "mcp-memory.json" emacs-assets-directory) )))

      ("motherduck" . (:command "uvx" :args ("mcp-server-motherduck" "--db-path" ":memory:")))

      ;; -- Using `mcp-remote' because these require OAuth support
      ("cloudflare-docs"    . (:command "bunx" :args ("mcp-remote" "https://docs.mcp.cloudflare.com/sse")))
      ("cloudflare-browser" . (:command "bunx" :args ("mcp-remote" "https://browser.mcp.cloudflare.com/sse")))
      ("cloudflare-dns"     . (:command "bunx" :args ("mcp-remote" "https://dns-analytics.mcp.cloudflare.com/sse")))
      ("cloudflare-radar"   . (:command "bunx" :args ("mcp-remote" "https://radar.mcp.cloudflare.com/sse")))

      ("deepwiki"   . (:url "https://mcp.deepwiki.com/sse"))
      ("context7"   . (:command "bunx" :args ("-y" "@upstash/context7-mcp")))
      ("nixos"      . (:command "uvx" :args ("--isolated" "mcp-nixos")))
      ("aws-docs"   . (:command "uvx" :args ("awslabs.aws-documentation-mcp-server@latest")))
      ("playwright" . (:command "bunx" :args ("@playwright/mcp@latest" "--browser" "firefox" "--headless" "--isolated")))

      ;; --
      ("globalping" . (:command "bunx" :args ("mcp-remote" "https://mcp.globalping.dev/sse")))
      ("vantage-instances"       . (:command "bunx" :args ("mcp-remote" "https://instances-mcp.vantage.sh/mcp")))
      ("local-sandbox"           . (:url "http://localhost:3000/mcp"))
      ("smithery.ai/google-maps" .
       ( :command "bunx"
         :args ("mcp-remote"
                ,(apply #'format
                        "https://server.smithery.ai/@smithery-ai/google-maps/mcp?profile=%s&api_key=%s"
                        (when-let ((user-password (auth-source-user-and-password "server.smithery.ai")))
                          (list (car user-password)
                                (cadr user-password)))))))))))

(use-package gptel
  :vc ( :url "https://github.com/karthink/gptel"
        :rev :newest )
  :demand t
  :bind ( :map gptel-mode-map
          ("C-c C-o" . gptel-clear*)
          ("C-c C-t" . gptel-tools)
          ("C-c M-t" . gptel-auto-tool-calls*)
          ("C-c M-m" . gptel-switch-model*)
          ("C-c M-s" . gptel-menu)
          ("C-c M-p" . gptel--preset)
          ("C-c M-g" . gptel-abort)
          ("RET" . gptel-send) )
  :custom ((gptel-use-curl t)
           (gptel-cache t)
           (gptel-confirm-tool-calls 'auto)
           (gptel-include-tool-results t)
           (gptel-expert-commands t)
           (gptel-default-mode 'org-mode)
           (gptel-curl-file-size-threshold 1300000))
  :config
  (if (eq system-type 'gnu/linux)
      (setq gptel-backend llms-chat-gptel-gemini-backend
            gptel-model 'gemini-3-pro-preview)
    (setq gptel-backend llms-chat-gptel-anthropic-backend
          gptel-model 'claude-sonnet-4-5))

  (require 'gptel-integrations)
  (gptel-mcp-connect (list "time" "fetch")
                     #'gptel-mcp--activate-tools)

  (add-hook 'gptel-mode-hook
            (lambda ()
              (add-hook 'kill-buffer-hook
                        #'gptel-archive-buffer-contents
                        nil
                        t)))

  :preface
  (defun gptel-switch-model* ()
    (interactive)
    ;; TODO: this doesn't work yet.
    (call-interactively #'gptel-menu)
    (call-interactively #'gptel--infix-provider))

  (defun gptel-auto-tool-calls* ()
    (interactive)
    (setq-local gptel-confirm-tool-calls 'auto))

  (defun gptel-clear* ()
    (interactive)
    (gptel-archive-buffer-contents)
    (forward-line -1)
    (delete-region (point-min) (point))
    (forward-line)
    (end-of-line))

  (defun gptel-buffer-toggle* ()
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
        (call-interactively #'gptel)))))

(use-package gptel-custom-tools :after gptel :load-path "etc/")
(use-package gptel-ext :after gptel :load-path "etc/")

(use-package inline-diff
  :vc ( :url "https://code.tecosaur.net/tec/inline-diff"
        :rev :newest )
  :after gptel-rewrite)

(use-package gptel-rewrite
  :after gptel
  :bind ( :map gptel-rewrite-actions-map
          ("C-c C-i" . gptel--rewrite-inline-diff) )
  :config
  (defun gptel--rewrite-inline-diff (&optional ovs)
    "Start an inline-diff session on OVS."
    (interactive (list (gptel--rewrite-overlay-at)))
    (unless (require 'inline-diff nil t)
      (user-error "Inline diffs require the inline-diff package."))
    (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
                ((buffer-live-p ov-buf)))
      (with-current-buffer ov-buf
        (cl-loop for ov in (ensure-list ovs)
                 for ov-beg = (overlay-start ov)
                 for ov-end = (overlay-end ov)
                 for response = (overlay-get ov 'gptel-rewrite)
                 do (delete-overlay ov)
                 (inline-diff-words
                  ov-beg ov-end response))))))

(use-package gptel-prompts
  :vc ( :url "https://github.com/jwiegley/gptel-prompts.git"
        :rev :newest )
  :demand t
  :custom (gptel-prompts-directory (expand-file-name "prompts" emacs-assets-directory))
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
  :custom
  (gptel-quick-timeout 60)

  :config
  (unless (eq system-type 'darwin)
    (setq gptel-quick-model 'gemini-3-pro-preview)
    (setq gptel-quick-backend llms-chat-gptel-gemini-backend))

  (setq gptel-quick-system-message
        (lambda (count)
          (concat (format "Write an informative summary in roughly %s words." count)
                  "- If you are given a short sentence in a foreign language, explain it in English."
                  "  Help me learn the foreign language."
                  "- Do not repeat the instructions, be concise and optimize for providing"
                  "  correct information in as few words as possible."
                  "- Use tools to fetch relevant information if needed.")))

  :preface
  (defun gptel-quick* ()
    (interactive)
    (let ((gptel-use-tools nil)
          (prompt (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (read-string "Text: "))))
      (gptel-quick prompt)))

  (defun gptel-quick-proofread* ()
    (interactive)
    (let ((gptel-use-tools nil)
          (gptel-quick-system-message
           (lambda (_count)
             (alist-get 'proof gptel-directives)))
          (prompt
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (read-string "Text: "))))
      (gptel-quick (format "Here's the text:---\n%s\n---"  prompt))))

  (defun gptel-quick-comment* ()
    (interactive)
    (let ((gptel-use-tools nil)
          (gptel-quick-system-message
           (lambda (count)
             (concat "Write a review of the text I share with you.\n"
                     "Limit yourself to " (number-to-string count) " words but make sure you convey information within the word limit.\n"
                     "- If you see code, do a concise code review focusing on common community conventions.\n"
                     "- If you see general text, help me improve the clarity and highlight mistakes that might cause confusion."
                     "- If there is non-English text, help me understand the meaning and teach me specifics so that my knowledge of the foreign langauge improves.")))
          (query-text (if (region-active-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (read-string "Text: "))))
      (gptel-quick query-text))))

(use-package macher
  :vc ( :url "https://github.com/kmontag/macher"
        :rev :newest )
  :after gptel
  :init
  (use-package buttercup :ensure t)
  :custom (macher-action-buffer-ui 'org)
  :config (macher-install))

(use-package claude-code-ide
  :vc ( :url "https://github.com/manzaltu/claude-code-ide.el"
        :rev :newest )
  :demand t
  :custom
  (claude-code-ide-window-width 80)
  (claude-code-ide-use-ide-diff nil)
  (claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-use-side-window nil)
  (claude-code-eat-read-only-mode-cursor-type '(bar nil nil))

  :config
  (claude-code-ide-emacs-tools-setup)

  :init
  (setenv "DISABLE_AUTOUPDATER" "true")
  ;; Claude Code uses a few symbols that constantly change the line height in
  ;; Emacs. This is an attempt to fix that.
  ;; I am rescaling these fonts so that if they are used for unicode characters
  ;; appearining in animations, the line height isn't increased temporarily.
  (add-to-list 'face-font-rescale-alist
               '(".*STIX Two Math.*" . 0.7))
  (add-to-list 'face-font-rescale-alist
               '(".*Arial Unicode MS.*" . 0.7)))

(use-package eca
  :ensure t
  :bind ( :map eca-chat-mode-map
          ("C-c C-n" . eca-chat-go-to-next-expandable-block)
          ("C-c C-p" . eca-chat-go-to-prev-expandable-block) )


  :custom
  (eca-chat-use-side-window nil)
  (eca-chat-prompt-prefix-loading "◾ ")
  (eca-chat-expandable-block-open-symbol "➕")
  (eca-chat-expandable-block-close-symbol "➖")
  (eca-chat-mcp-tool-call-loading-symbol "⏳")
  (eca-chat-mcp-tool-call-error-symbol "✖")
  (eca-chat-mcp-tool-call-success-symbol "✓")

  :config
  (when-let (api-key (and (eq system-type 'gnu/linux)
                          (gptel-api-key-from-auth-source "api.openai.com")))
    (setenv "OPENAI_API_KEY" api-key))

  (when-let (api-key (and (eq system-type 'darwin)
                          (gptel-api-key-from-auth-source "api.anthropic.com")))
    (setenv "ANTHROPIC_API_KEY" api-key)))

(use-package esi-dictate
  :disabled t
  :demand t
  :vc ( :url "https://github.com/narendraj9/emacs-speech-input"
        :rev :newest )
  :bind ( :map esi-dictate-mode-map ("C-g" . esi-dictate-stop) )
  :hook (esi-dictate-speech-final . esi-dictate-fix-context)
  :config
  (require 'gptel)
  (setq llm-warn-on-nonfree nil)
  (setq esi-dictate-transcribe-api-key (gptel-api-key-from-auth-source "api.openai.com"))
  (setq esi-dictate-llm-provider (make-llm-openai :key (gptel-api-key-from-auth-source "api.openai.com")
                                                  :chat-model "gpt-4o-mini")))

(use-package ragmacs
  :vc ( :url "https://github.com/positron-solutions/ragmacs"
        :rev :newest )
  :after gptel
  :init
  (gptel-make-preset 'introspect
    :pre (lambda () (require 'ragmacs))
    :tools '("introspection")
    :system
    "You are pair programming with the user in Emacs and on Emacs.

 Your job is to dive into Elisp code and understand the APIs and
 structure of elisp libraries and Emacs.  Use the provided tools to do
 so, but do not make duplicate tool calls for information already
 available in the chat.

 <tone>
 1. Be terse and to the point.  Speak directly.
 2. Explain your reasoning.
 3. Do NOT hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do NOT summarize your answers.
 </tone>

 <code_generation>
 When generating code:
 1. Always check that functions or variables you use in your code exist.
 2. Also check their calling convention and function-arity before you use them.
 3. Write code that can be tested by evaluation, and offer to evaluate
 code using the `elisp_eval` tool.
 </code_generation>

 <formatting>
 1. When referring to code symbols (variables, functions, tags etc) enclose them in markdown quotes.
    Examples: `read_file`, `getResponse(url, callback)`
    Example: `<details>...</details>`
 2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
 </formatting>"))

(use-package shell-maker
  :ensure t
  :custom
  (shell-maker-transcript-default-path
   (expand-file-name "agent-shell-archive/" emacs-assets-directory))
  (shell-maker-transcript-default-filename
   (lambda () (format-time-string "%F-%T-agent-shell.txt"))))

(use-package acp
  :vc ( :url "https://github.com/xenodium/acp.el"
        :rev :newest ))

(use-package agent-shell
  :vc ( :url "https://github.com/xenodium/agent-shell"
        :rev :newest ))




(provide 'llms)
