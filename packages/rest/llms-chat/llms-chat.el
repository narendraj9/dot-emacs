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

(require 'gptel)
(require 'gptel-transient)
(require 'gptel-curl)
(require 'gptel-gemini)
(require 'gptel-anthropic)
(require 'gptel-kagi)

(defun llms-chat-make-progress-indicator (starting-point)
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
                                (sleep-for 0.1)))
                     (funcall shutdown-fn))))
    ;; Return the function to shutdown the thread so that the caller can use it.
    shutdown-fn))

(defun llms-chat-stop-progress-indicator (progress-indicator)
  (funcall progress-indicator))

(defun llms-chat--api-key-from-auth-source (host)
  ;; See: https://github.com/karthink/gptel/issues/4
  ;;      https://github.com/tkf/emacs-request/issues/185#issuecomment-654553599
  ;;      -- Library users are supposed to encode multi-byte strings to utf-8
  ;;      unibyte string. --
  (when-let ((api-key (auth-source-pick-first-password :host host)))
    (encode-coding-string api-key 'utf-8)))

(defvar llms-chat-gptel-openrouter-backend
  (when-let ((api-key (llms-chat--api-key-from-auth-source "openrouter.ai")))
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
  (gptel-make-openai "OpenAI" :key openai-secret-key))

(defvar llms-chat-gptel-anthropic-backend
  (gptel-make-anthropic "Anthropic"
    :key (llms-chat--api-key-from-auth-source "api.anthropic.com")))

(defvar llms-chat-gptel-groq-backend
  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (llms-chat--api-key-from-auth-source "api.groq.com")
    :models '("mixtral-8x7b-32768"
              "gemma-7b-it"
              "llama2-70b-4096"
              "llama3-70b-8192")))

(defvar llms-chat-gptel-preplexity-backend
  (when-let ((api-key (llms-chat--api-key-from-auth-source "api.perplexity.ai")))
    (gptel-make-openai "Perplexity"
      :host "api.perplexity.ai"
      :endpoint "/chat/completions"
      :stream t
      :key api-key
      :models '("sonar-small-chat"
                "sonar-medium-chat"
                "sonar-small-online"
                "sonar-medium-online"))))

(defvar llms-chat-gptel-gemini-backend
  (when-let ((api-key (llms-chat--api-key-from-auth-source "generativelanguage.googleapis.com")))
    (gptel-make-gemini "Gemini" :key api-key)))

(defvar llms-chat-gptel-kagi-backend
  (when-let ((api-key (llms-chat--api-key-from-auth-source "kagi.com")))
    (gptel-make-kagi "Kagi"
      :stream t
      :key api-key)))

(setq gptel-backend llms-chat-gptel-groq-backend
      gptel-model "llama3-70b-8192")

(defvar llms-chat-openrouter-models nil)
(defun llms-chat-openrouter-models ()
  (when-let ((api-key (llms-chat--api-key-from-auth-source "openrouter.ai")))
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
                     (re-search-backward "@[^\\S]+" (line-beginning-position) t)
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
  `(("groq"   . (,llms-chat-gptel-groq-backend       . "llama3-70b-8192"))
    ("opus"   . (,llms-chat-gptel-openrouter-backend . "anthropic/claude-3-opus"))
    ("haiku"  . (,llms-chat-gptel-openrouter-backend . "anthropic/claude-3-haiku"))
    ("openai" . (,llms-chat-gptel-openai-backend     . "gpt-4o"))
    ("pplx"   . (,llms-chat-gptel-preplexity-backend . "sonar-medium-online"))
    ("gemini" . (,llms-chat-gptel-gemini-backend     . "gemini-1.5-pro"))
    ("flash"  . (,llms-chat-gptel-gemini-backend     . "gemini-1.5-flash"))
    ("kagi"   . (,llms-chat-gptel-kagi-backend       . "fastgpt"))))

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
      (when-let ((prop (text-property-search-forward 'llm-role 'user t)))
        (cons (prop-match-beginning prop)
              (prop-match-end prop)))))

   (t (cons (save-excursion (backward-paragraph) (point))
            (line-end-position)))))

(let ((llm-name-regexp (mapconcat (lambda (llm-name)
                                    (format "\\(@%s:?\\)" llm-name))
                                  (llms-chat-models)
                                  "\\|")))
  (defun llms-chat--prompt-text (beg end)
    (replace-regexp-in-string  llm-name-regexp
                               ""
                               ;; Include text properties so that
                               ;; `gptel--parse-buffer' can separate text into
                               ;; user and assistant messages.
                               (buffer-substring beg end))))

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

(defun llms-chat--prompt-properties (prompt-id &rest extra-props)
  `( llm-prompt-id ,prompt-id
     llm-role user
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
    front-sticky nil
    rear-nonsticky t
    ,@extra-props))

(defun llms-chat--remove-old-reply (prompt-id)
  ;; Try removing existing response.
  (when-let ((bounds (llms-chat--reply-bounds prompt-id)))
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

(defun llms-chat--insert-reply (prompt-id response info)
  (let* ((position (marker-position (plist-get info :position)))
         (buffer  (buffer-name (plist-get info :buffer)))
         (color-% 18)
         (background-color (color-lighten-name (background-color-at-point) color-%))
         (foreground-color (color-darken-name (foreground-color-at-point) color-%))
         (face (list :background background-color :foreground foreground-color))
         (text-properties (llms-chat--reply-properties prompt-id
                                                       'face face
                                                       'font-lock-face face)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char position)
        (insert response)
        (add-text-properties position (point) text-properties)
        (insert "\n")
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
         (llm-name-regex "@\\([-/a-z0-9]+\\)\\(\\S+\\|$\\)")
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

;;;###autoload
(defun llms-chat (arg)
  "Talk to LLMs as if you are chatting to them,

With a prefix ARG, send the whole buffer to the LLM upto current point.

@openai: current world population is.. (one sentence)
@openai:

As of 2023, the estimated world population is approximately 8 billion.
"
  (interactive "P")
  (save-excursion
    (let* (
           ;; A struct (position, id, etc.) probably makes more sense here.
           (prompt-bounds (llms-chat--prompt-bounds arg))
           (prompt-start-position (car prompt-bounds))
           (prompt-end-position (cdr prompt-bounds))
           (prompt (llms-chat--prompt-text prompt-start-position prompt-end-position))
           (prompt-id (llms-chat--prompt-id prompt-bounds))
           ;; --
           (llm-name (llms-chat--llm-name prompt-bounds))
           (gptel-params (llms-chat--name->gptel-params llm-name))
           (gptel-backend (car gptel-params))
           (gptel-model (cdr gptel-params)))

      (unless (and gptel-backend gptel-model)
        (error "No backend found for LLM: %s" llm-name))
      (when (s-blank-str? prompt)
        (user-error "No prompt found."))
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

      (let ((progress-indicator (llms-chat-make-progress-indicator (point))))
        (condition-case error
            (gptel-request prompt
              :system llms-chat--system-prompt
              :position (point)
              :callback
              (lambda (response info)
                (llms-chat-stop-progress-indicator progress-indicator)
                (if (not response)
                    (error "Error talking to LLM %s: %s" llm-name info)
                  (llms-chat--insert-reply prompt-id response info))))
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
                                '(gptel response))
        (put-text-property (prop-match-beginning prop)
                           (prop-match-end prop)
                           'face '(:background "green")))
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

;;; ---------------------------------------------------------------------

;;;###autoload
(define-minor-mode llms-chat-minor-mode
  "Minor mode for chatting with LLMs. Exists mainly to set up completion at
point for the large language models openrouter.ai supports."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c RET") #'llms-chat)
            map)
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
