;;; llms-chat.el --- Chat with LLMs by @mentioning them anywhere in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: tools, convenience
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (gptel "0.8.6") (spinner "1.7.4"))

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
(require 'spinner)
(require 'url-http)

(require 'gptel)
(require 'gptel-transient)
(require 'gptel-request)
(require 'gptel-gemini)
(require 'gptel-anthropic)
(require 'gptel-kagi)

(defgroup llms-chat nil
  "Chat with LLMs from anywhere in Emacs.")

(defcustom llms-chat-include-model-usage-info nil
  "Include model usage information in the response inserted by `llms-chat'."
  :type 'boolean
  :group 'llms-chat)

(defface llms-chat-usage-text-face
  '((t :inherit shadow :height 0.6))
  "Face for text displaying usage information after LLM replies in buffers.")

(defface llms-chat-reply-face
  '((t :inherit default))
  "Face for replies from an LLM.")

(defface llms-chat-prompt-face
  '((t :inherit default))
  "Face for prompt sent to an LLM.")

(defun llms-chat--pandoc-url->markdown (url)
  (when (executable-find "pandoc")
    (shell-command-to-string (format "pandoc --from html --to markdown %s"
                                     (shell-quote-argument url)))))

(defun llms-chat--pandoc-url->plaintext (url)
  (when (executable-find "pandoc")
    (shell-command-to-string (format "pandoc --from html --to plain %s"
                                     (shell-quote-argument url)))))

(defun llms-chat--url-retrieve-plaintext (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char url-http-end-of-headers)
    (shr-render-region (point-min) (point-max))
    (encode-coding-string
     (buffer-substring-no-properties (point-min) (point-max))
     'utf-8)))

(defun llms-chat-make-progress-indicator (starting-point)
  (when llms-chat-show-in-progress-indicator
    (let* ((indicate-progress-p t)
           (spinner (spinner-create 'box-in-circle))
           (indicator-overlay (make-overlay starting-point starting-point))
           (timeout-seconds 60)
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
                                  (overlay-put indicator-overlay
                                               'after-string (spinner-print spinner))
                                  (redisplay t)
                                  (sleep-for 1)))
                       (funcall shutdown-fn))))
      ;; Return the function to shutdown the thread so that the caller can use it.
      shutdown-fn)))

(defun llms-chat-stop-progress-indicator (progress-indicator)
  (when llms-chat-show-in-progress-indicator
    (funcall progress-indicator)))

(defun llms-chat--api-key-from-auth-source (host)
  ;; See: https://github.com/karthink/gptel/issues/4
  ;;      https://github.com/tkf/emacs-request/issues/185#issuecomment-654553599
  ;;      -- Library users are supposed to encode multi-byte strings to utf-8
  ;;      unibyte string. --
  (when-let* ((api-key (auth-source-pick-first-password :host host)))
    (encode-coding-string api-key 'utf-8)))

(defvar llms-chat-gptel-ollama-backend
  (gptel-make-ollama "Ollama"
    :models '("ollama3.2")
    :stream t))

(defvar llms-chat-gptel-openrouter-backend
  (when-let* ((api-key (llms-chat--api-key-from-auth-source "openrouter.ai")))
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key api-key
      :models '("anthropic/claude-3-opus"
                "anthropic/claude-3-sonnet"
                "anthropic/claude-3-haiku"
                "meta-llama/codellama-34b-instruct"
                "codellama/codellama-70b-instruct"
                "google/palm-2-codechat-bison-32k"))))

(defvar llms-chat-gptel-openai-backend
  (gptel-make-openai "OpenAI"
    :key (or (and (boundp 'openai-secret-key) openai-secret-key)
             (llms-chat--api-key-from-auth-source "api.openai.com"))
    :models '(gpt-5.2 gpt-5 gpt-3.5-turbo gpt-4.1 gpt-4o o1 o3 o3-pro o3-mini)
    :stream t))

(defvar llms-chat-gptel-anthropic-backend
  (when-let* ((api-key (llms-chat--api-key-from-auth-source "api.anthropic.com")))
    (gptel-make-anthropic "Anthropic"
      :models '(claude-opus-4-6 claude-sonnet-4-5 claude-opus-4-5 claude-haiku-4-5)
      :key api-key
      :stream t)))

(defvar llms-chat-gptel-groq-backend
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (llms-chat--api-key-from-auth-source "api.groq.com")
    :models '(moonshotai/kimi-k2-instruct
              groq/compound
              groq/compound-mini
              gemma2-9b-it
              meta-llama/llama-guard-4-12b
              deepseek-r1-distill-llama-70b
              meta-llama/llama-4-maverick-17b-128e-instruct)))

(defvar llms-chat-gptel-preplexity-backend
  (when-let* ((api-key (llms-chat--api-key-from-auth-source "api.perplexity.ai")))
    (gptel-make-openai "Perplexity"
      :host "api.perplexity.ai"
      :endpoint "/chat/completions"
      :stream t
      :key api-key
      :models '(sonar
                sonar-pro))))

(defvar llms-chat-gptel-gemini-backend
  (when-let* ((api-key (llms-chat--api-key-from-auth-source "generativelanguage.googleapis.com")))
    (gptel-make-gemini "Gemini"
      :key api-key
      :stream nil)))

(defvar llms-chat-gptel-kagi-backend
  (when-let* ((api-key (llms-chat--api-key-from-auth-source "kagi.com")))
    (gptel-make-kagi "Kagi"
      :stream t
      :key api-key)))

(defvar llms-chat-gptel-deepseek-backend
  (when-let* ((api-key (llms-chat--api-key-from-auth-source "api.deepseek.com")))
    (gptel-make-openai "DeepSeek"
      :host "api.deepseek.com"
      :endpoint "/chat/completions"
      :stream t
      :key api-key
      :models '(deepseek-chat deepseek-reasoner deepseek-coder))))

(defvar llms-chat-xai-backend
  (when-let* ((api-key (llms-chat--api-key-from-auth-source "api.x.ai")))
    (gptel-make-xai "Grok"
      :key api-key
      :stream t
      :models '(grok-4 grok-3))))


(defvar llms-chat-openrouter-models nil)
(defun llms-chat-openrouter-models ()
  (when-let* ((api-key (llms-chat--api-key-from-auth-source "openrouter.ai")))
    (unless llms-chat-openrouter-models
      (let* ((json-object-type 'plist)
             (json-array-type 'list)
             (json-key-type 'keyword)
             (models-url "https://openrouter.ai/api/v1/models"))
        (request models-url
          :parser #'json-read
          :success
          (cl-function
           (lambda (&key data &allow-other-keys)
             (setq llms-chat-openrouter-models
                   (plist-get data :data))
             ;; Redefine OpenRouter backend with an updated model list.
             (gptel-make-openai "OpenRouter"
               :host "openrouter.ai"
               :endpoint "/api/v1/chat/completions"
               :stream t
               :key api-key
               :models (mapcar (lambda (m) (plist-get m :id)) llms-chat-openrouter-models))))
          :sync t)))
    llms-chat-openrouter-models))

(defun llms-chat-openrouter-model (model-id)
  (car (seq-filter (lambda (model)
                     (string= (plist-get model :id) model-id))
                   (llms-chat-openrouter-models))))

(defun llms-chat-openrouter-model-search (model-name)
  "Try finding a model with the same name as MODEL-NAME on openrouter.ai"
  (let ((model-name (if (symbolp model-name)
                        (symbol-name model-name)
                      model-name)))
    (or (llms-chat-openrouter-model model-name)
        (car (seq-filter (lambda (model)
                           (string-suffix-p model-name (plist-get model :id)))
                         (llms-chat-openrouter-models))))))

(defun llms-chat-models ()
  (seq-concatenate 'list
                   (mapcar #'car llms-chat-gptel-backends)
                   (mapcar (lambda (model) (plist-get model :id))
                           (llms-chat-openrouter-models))))

(defun llms-chat-completion-function (str pred action)
  (let ((model-ids (llms-chat-models))
        (annotation-function
         (lambda (model-id)
           (when-let* ((model (llms-chat-openrouter-model model-id))
                       (model-description (plist-get model :description))
                       (annotation (format " %s " model-description)))
             (truncate-string-to-width annotation
                                       (frame-width)
                                       nil
                                       nil
                                       (truncate-string-ellipsis))))))
    (cond
     ((eq action 'metadata)
      `(metadata . ((annotation-function . ,annotation-function))))
     ((eq action t)
      (all-completions str model-ids pred))
     ((eq action 'lambda)
      (test-completion str model-ids pred))
     ((eq action 'nil)
      (try-completion str model-ids pred)))))

(defun llms-chat-completion-at-point-function ()
  (when-let* ((beg (save-excursion
                     (re-search-backward "@[^\\S]*" (line-beginning-position) t)
                     (point)))
              (end (point))
              (symbol-str (buffer-substring-no-properties beg end)))
    (when (string-prefix-p "@" symbol-str)
      (list (1+ beg)
            end
            (completion-table-dynamic
             (lambda (_)
               (llms-chat-models)))))))

;;; ──────────────────────────────────────────────────────────────────
;; Functions to have a conversation with LLMs using a chat/mention interface.
;;

(defvar llms-chat-gptel-backends
  ;; An association list each association of which is of the form:
  ;;
  ;; (<llm-name> . (<llm-backend> . <llm-model>))
  ;;
  `(("ollama"   . (,llms-chat-gptel-ollama-backend     . qwen2.5))
    ("groq"     . (,llms-chat-gptel-groq-backend       . llama3-groq-70b-8192-tool-use-preview))
    ("sonnet"   . (,llms-chat-gptel-openrouter-backend . @anthropic/claude-3.5-sonnet:beta))
    ("openai"   . (,llms-chat-gptel-openai-backend     . gpt-4.1))
    ("pplx"     . (,llms-chat-gptel-preplexity-backend . sonar))
    ("seek"     . (,llms-chat-gptel-deepseek-backend   . deep-reasoner))
    ("deepchat" . (,llms-chat-gptel-deepseek-backend   . deep-chat))
    ("gemini"   . (,llms-chat-gptel-gemini-backend     . gemini-2.5-pro-preview-06-05))
    ("flash"    . (,llms-chat-gptel-gemini-backend     . gemini-2.0-flash))
    ("kagi"     . (,llms-chat-gptel-kagi-backend       . summarize:muriel))))


(defvar llms-chat-context-providers
  ;; Hardcoding context provider usage to start with the `#' char,
  ;; e.g. #webpage https://example.com Current implementation doesn't allow
  ;; spaces to be present in the argument to the context name.
  `(("webpage"   . llms-chat-context-provider-webpage)
    ("plainpage" . llms-chat-context-provider-plainpage)
    ("file"      . llms-chat-context-provider-file)))


(defun llms-chat-context-provider-plainpage (url)
  "Similar to `llms-chat-context-provider-webpage' but costs fewer
tokens. The only real drawback is the fact that urls are completely
removed and replaced by text."
  (let* ((url-contents
          (or (llms-chat--pandoc-url->plaintext url)
              (llms-chat--url-retrieve-plaintext url))))
    (format "Content of webpage at %s: \n%s\n----\n"
            url
            url-contents)))

(defun llms-chat-context-provider-webpage (url)
  (let* ((url-contents
          (or (llms-chat--pandoc-url->markdown url)
              (llms-chat--url-retrieve-plaintext url))))
    (format "Content of webpage at %s: \n%s\n----\n"
            url
            url-contents)))

(defun llms-chat-context-provider-file (file-path)
  (let ((file-contents (with-temp-buffer
                         (insert-file-contents file-path)
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (format "Contents of file at path %s:\n%s\n---\n"
            file-path
            file-contents)))

(defun llms-chat-apply-context-providers (prompt)
  "Expand prompt to include context provided by
`llms-chat-context-providers' used in the prompt."
  (let ((new-prompt prompt))
    (with-temp-buffer
      (insert prompt)
      (dolist (context-provider llms-chat-context-providers)
        (goto-char (point-min))
        (while (re-search-forward (concat "#"
                                          (regexp-quote (car context-provider))
                                          ;; TODO: arg to context provider
                                          ;; cannot have spaces.
                                          "\\S+\\([^ ]+\\)")
                                  (point-max)
                                  t)
          (let ((context-string (json-encode-string
                                 (funcall (cdr context-provider)
                                          (match-string 1)))))
            (setq new-prompt
                  (string-join (list new-prompt context-string) "\n"))))))
    new-prompt))

(defun llms-chat--name->gptel-params (name)
  (or (assoc-default name llms-chat-gptel-backends)
      (when (llms-chat-openrouter-model name)
        (cons llms-chat-gptel-openrouter-backend name))
      (user-error (format "Unknown LLM: %s" name))))

(defun llms-chat--prompt-bounds (&optional start-at-bobp)
  (cond
   ((region-active-p)
    (cons (region-beginning) (region-end)))

   (start-at-bobp
    (cons (point-min) (line-end-position)))

   ;; Inside an existing prompt, then send the potentially updated prompt
   ((eq 'user (get-text-property (point) 'llm-role))
    (save-excursion
      (text-property-search-backward 'llm-role 'user t)
      (when-let* ((prop (text-property-search-forward 'llm-role 'user t)))
        (cons (prop-match-beginning prop)
              (prop-match-end prop)))))

   (t (cons (save-excursion (backward-paragraph) (point))
            (line-end-position)))))

(let ((llm-name-regexp (mapconcat (lambda (llm-name)
                                    (format "\\(@%s:?\\)" llm-name))
                                  (llms-chat-models)
                                  "\\|")))
  (defun llms-chat--prompt-text (beg end)
    (let* ((initial-prompt
            ;; Include text properties so that `gptel--parse-buffer' can separate
            ;; text into user and assistant messages.
            (buffer-substring beg end))
           ;; Remove all LLM @handles.
           (prompt-with-no-mentions
            (replace-regexp-in-string llm-name-regexp "" initial-prompt)))
      ;; Expand context providers to append information to the overall prompt
      ;; sent to the LLM
      (llms-chat-apply-context-providers prompt-with-no-mentions))))

(defun llms-chat--reply-bounds (prompt-id)
  "Return the bounds of the next reply starting at current point."
  (when-let* ((reply-start
               (text-property-any (point) (point-max) 'llm-role 'assistant))
              (reply-end
               (and (string= prompt-id (get-text-property reply-start 'llm-prompt-id))
                    (or (text-property-not-all reply-start (point-max)
                                               'llm-prompt-id prompt-id)
                        ;; All of the buffer is filled with reply from an LLM
                        ;; till the end of buffer.
                        (point-max)))))
    (cons reply-start reply-end)))

(defvar llms-chat--local-text-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-' t C-c") #'llms-chat)
    (define-key keymap (kbd "C-' t C-k") #'llms-chat-cleanup)
    (define-key keymap (kbd "C-' t C-n") #'llms-chat-add-reply)
    keymap)
  "Keymap active when point is within text inserted because of `llms-chat'
interactions.")

(defun llms-chat--prompt-properties (prompt-id &rest extra-props)
  `( llm-prompt-id ,prompt-id
     llm-role user
     keymap ,llms-chat--local-text-keymap
     ;; face llms-chat-prompt-face
     ;; font-lock-face llms-chat-prompt-face
     ,@extra-props ))

(defun llms-chat--reply-properties (prompt-id &rest extra-props)
  `(
    ;; Implementation detail and might change without
    ;; notice. This text property is used by
    ;; `gptel--parse-buffer' to build the list of messages
    ;; (from user and assisstant) to send to the LLM.
    gptel response
    ;; Including this in text properties just in case I would like to attribute
    ;; the response to a specific backend.
    gptel-backend ,gptel-backend
    ;; ---------------------------------------------------

    llm-prompt-id ,prompt-id
    llm-role assistant

    keymap ,llms-chat--local-text-keymap
    front-sticky nil
    rear-nonsticky t
    ,@extra-props))

(defun llms-chat--remove-old-reply (prompt-id)
  ;; Try removing existing response.
  (when-let* ((bounds (llms-chat--reply-bounds prompt-id)))
    (delete-region (car bounds) (cdr bounds))))

(defun llms-chat--fill-text (start end)
  "Fill text present between positions start and end, inclusive."
  ;; To be implemented.
  ;; ──────────────────────────────────────────────────────────────────
  ;; Fill response text.
  ;; (let ((end-of-reply (point)))
  ;;   (with-restriction position end-of-reply
  ;;     (goto-char position)
  ;;     (while (< (point) end-of-reply)
  ;;       ;; Leave code blocks as they are.
  ;;       ;; TODO: fontify code blocks. Can I just use something provided by
  ;;       ;; org-mode here?
  ;;       (if (looking-at "\\S*```[a-z- ]*\\S+")
  ;;           (forward-paragraph)
  ;;         (fill-region (point) (forward-paragraph))))
  ;;     (widen)))
  ;; ──────────────────────────────────────────────────────────────────
  :noop)

(defun llms-chat--model-usage-text (prompt-id model-usage)
  (when-let* ((tokens (plist-get model-usage :tokens))
              (cost (plist-get model-usage :cost)))
    (apply #'propertize
           "\n"
           'display
           (format "\n─────────────────── [ Tokens: %.2fk, Cost: ¢%0.4f ] \n"
                   (/ tokens 1000.0)
                   (* cost 100))
           'face  'llms-chat-usage-text-face
           'font-lock-face 'llms-chat-usage-text-face
           (llms-chat--reply-properties prompt-id))))

(defun llms-chat--insert-reply (prompt-id response info)
  (let* ((model-usage-info (plist-get info :model-usage-info))
         (position (marker-position (plist-get info :position)))
         (buffer  (buffer-name (plist-get info :buffer)))
         (text-properties (llms-chat--reply-properties prompt-id
                                                       'face 'llms-chat-reply-face
                                                       'font-lock-face 'llms-chat-reply-face)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char position)
        (insert response)
        (add-text-properties position (point) text-properties)
        (when model-usage-info
          (insert model-usage-info))
        (llms-chat--fill-text position (point))))))

(defun llms-chat--prompt-id (prompt-bounds)
  (let* ((prompt-start-position (car prompt-bounds))
         (text-author (get-text-property prompt-start-position 'llm-role))
         (org-id-method 'ts))
    (or (and (eq text-author 'user)
             (get-text-property prompt-start-position 'llm-prompt-id))
        (org-id-new "llm"))))

(defun llms-chat--llm-name (prompt-bounds)
  (let* ((prompt-start-position (car prompt-bounds))
         (prompt-end-position (cdr prompt-bounds))
         (llm-name-regex "@\\([-/a-z0-9\\.]+\\)\\(\\S+\\|$\\)")
         (llm-name (save-excursion
                     (goto-char prompt-end-position)
                     (and (re-search-backward llm-name-regex prompt-start-position t)
                          (buffer-substring (match-beginning 1) (match-end 1))))))
    (unless llm-name
      (user-error "No LLM name found in the prompt."))
    llm-name))

(defvar llms-chat--system-prompt
  "Follow these rules no matter what:
1. Be factually correct. If you are not sure about something, don't say
anything at all.
2. Be concise.
3. Answer in as few sentences as possible but make sure to provide a complete
answer. Use mathematical equations if that helps.
4. Use Emacs org-mode source code blocks for code snippets.")


(defvar llms-chat--send-whole-buffer nil)
(make-variable-buffer-local 'llms-chat--send-whole-buffer)

(defcustom llms-chat-post-response-hook (list)
  "Hook to run after response from an LLM is inserted into the buffer."
  :group 'llms-chat)

(defcustom llms-chat-show-in-progress-indicator t
  "Disable progress indicators, hangs Emacs on macOS for me."
  :group 'llms-chat)

;;;###autoload
(defun llms-chat (arg)
  "Talk to LLMs as if you are chatting to them,

With a prefix ARG, send the whole buffer to the LLM upto current point.

@openai: current world population is.. (one sentence)
@openai:

As of 2023, the estimated world population is approximately 8 billion.
"
  (interactive "P")
  (unless llms-chat-minor-mode
    (llms-chat-minor-mode +1))
  (when arg
    ;; Once you send the whole buffer, keep sending it in future interactions by
    ;; default.
    (setq llms-chat--send-whole-buffer t))
  (save-excursion
    (let* (
           ;; A struct (position, id, etc.) probably makes more sense here.
           (prompt-bounds (llms-chat--prompt-bounds llms-chat--send-whole-buffer))
           (prompt-start-position (car prompt-bounds))
           (prompt-end-position (cdr prompt-bounds))
           (prompt (llms-chat--prompt-text prompt-start-position prompt-end-position))
           (prompt-id (llms-chat--prompt-id prompt-bounds))
           (llm-name (llms-chat--llm-name prompt-bounds))
           (gptel-params (llms-chat--name->gptel-params llm-name))

           ;; Keeping these around as lexical variables for use in the callback.
           (llm-backend (car gptel-params))
           (llm-model (cdr gptel-params))
           (gptel-backend llm-backend)
           (gptel-model llm-model))

      (unless (and gptel-backend gptel-model)
        (error "No backend found for LLM: %s" llm-name))
      (when (s-blank-str? prompt)
        (user-error "No prompt found."))
      (setq llms-chat--last-used-model gptel-model)

      (pulse-momentary-highlight-region prompt-start-position prompt-end-position)
      (add-text-properties prompt-start-position
                           prompt-end-position
                           (llms-chat--prompt-properties prompt-id))

      (llms-chat--remove-old-reply prompt-id)

      (goto-char prompt-end-position)
      (insert (apply #'propertize
                     (format "%s@%s: "
                             (if (looking-back "^\\S+") "" "\n")
                             llm-name)
                     (llms-chat--reply-properties prompt-id)))

      (let ((progress-indicator (llms-chat-make-progress-indicator (point)))
            (model-name gptel-model))
        (condition-case error
            (gptel-request prompt
              :system llms-chat--system-prompt
              :position (point)
              :callback
              (lambda (response info)
                (llms-chat-stop-progress-indicator progress-indicator)
                (if (not response)
                    (error "Error talking to LLM %s: %s" llm-name info)
                  (let* ((model-usage
                          (and llms-chat-include-model-usage-info
                               (llms-chat-model-usage llm-backend model-name response)))
                         (model-usage-text
                          (llms-chat--model-usage-text prompt-id model-usage))
                         (mode-line-string
                          (llms-chat-model-usage->mode-line-string model-usage)))
                    (when llms-chat-include-model-usage-info
                      (setq llms-chat--mode-line-string mode-line-string)
                      (run-with-timer 5 nil (lambda ()
                                              (setq llms-chat--mode-line-string nil))))
                    (llms-chat--insert-reply prompt-id
                                             response
                                             `( :model-usage-info ,model-usage-text
                                                ,@info ))
                    ;; A final newline that is not considered part of the LLM
                    ;; response, i.e. has no custom text properties.
                    (insert "\n")
                    (with-current-buffer (plist-get info :buffer)
                      (run-hooks 'llms-chat-post-response-hook))))))
          (error
           (message "Error sending request to LLM: %s" error)
           (llms-chat-stop-progress-indicator progress-indicator)))))))

;;;###autoload
(defun llms-chat-multi-assisstant ()
  "Each assistant maintains its own state and messages from other asssistants are
   treated as user messages."
  (interactive)
  (let* ((llm-names (remove "kagi" (llms-chat-models)))
         (next-llm (nth (random (length llm-names)) llm-names))
         (gptel-params (llms-chat--name->gptel-params next-llm))
         (next-llm-backend (car gptel-params))
         (next-llm-model (cdr gptel-params))
         (current-buffer (current-buffer))
         (current-point (point))
         (temp-buffer-prefix (format " *temp: %s* " next-llm))
         (temp-buffer (generate-new-buffer temp-buffer-prefix t))
         ;; Set up gptel params before making a call
         (gptel-backend next-llm-backend)
         (gptel-model next-llm-model)
         (gptel-max-tokens 100)
         (system-prompt
          (progn
            (goto-char (point-min))
            (search-forward "System Prompt:")
            (buffer-substring (point)
                              (progn (forward-paragraph)
                                     (point)))))
         (current-text (buffer-substring (point) (point-max)))
         (prop))
    (message "%s..." next-llm)
    (setq llms-chat--last-used-model gptel-model)
    (with-current-buffer temp-buffer
      (insert current-text)
      ;; Fix up text before sending it to the next llm.
      (goto-char (point-min))
      ;; Consider everything other than what `next-llm' replied with as user
      ;; text by removing the `gptel' text property.
      (while (setq prop (text-property-search-forward 'llm-name
                                                      next-llm
                                                      nil))
        (remove-text-properties (prop-match-beginning prop)
                                (prop-match-end prop)
                                '(gptel response)))
      (gptel-request nil
        :system system-prompt
        :callback
        (lambda (response info)
          (if response
              (with-current-buffer current-buffer
                (goto-char current-point)
                (insert
                 (propertize (format "\n%s: %s\n\n"
                                     (propertize (capitalize next-llm)
                                                 'face 'highlight
                                                 'font-lock-face 'highlight)
                                     response)
                             'gptel 'response
                             'llm-name next-llm)))
            (message "Request to %s failed with: %s"
                     next-llm info))))
      ;; Clean up
      (kill-buffer temp-buffer))
    (goto-char current-point)))


(defun llms-chat-cleanup ()
  "Clean up all messages associated with the current prompt under point."
  (interactive)
  (let ((prompt-id (get-text-property (point) 'llm-prompt-id))
        (prop))
    (save-excursion
      (text-property-search-backward 'llm-prompt-id prompt-id t)
      (while (setq prop (text-property-search-forward 'llm-prompt-id prompt-id t))
        (delete-region (prop-match-beginning prop) (prop-match-end prop))))))

(defun llms-chat-add-reply ()
  (interactive)
  (when-let* ((prompt-id (get-text-property (point) 'llm-prompt-id))
              (prompt-bounds (llms-chat--prompt-bounds))
              (llm-name (llms-chat--llm-name prompt-bounds))
              (next-position (or (text-property-not-all (point)
                                                        (point-max)
                                                        'llm-prompt-id
                                                        prompt-id)
                                 (point-max))))
    (goto-char next-position)
    (insert (format "\n@%s " llm-name))))


;;; ---------------------------------------------------------------------

(defvar llms-chat--mode-line-string nil)
(put 'llms-chat--mode-line-string 'risky-local-variable t)

;;;###autoload
(define-minor-mode llms-chat-minor-mode
  "Minor mode for chatting with LLMs. Exists mainly to set up completion at
point for the large language models openrouter.ai supports."
  :lighter (llms-chat--mode-line-string
            (" " llms-chat--mode-line-string " "))
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "C-c RET") #'llms-chat)
            keymap)
  (if llms-chat-minor-mode
      (progn (llms-chat-models)
             ;; When `llms-chat-minor-mode'is enabled, this completion function is the
             ;; first in the list.
             (add-to-list 'completion-at-point-functions
                          #'llms-chat-completion-at-point-function))
    (setq completion-at-point-functions
          (remove #'llms-chat-completion-at-point-function
                  completion-at-point-functions))
    (message "LLMS chat minor mode disabled.")))



(provide 'llms-chat)
;;; llms-chat.el ends here
