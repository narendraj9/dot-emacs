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

(use-package request :ensure t :demand t)
(use-package spinner :ensure t :demand t)

(defun llms-api-key-from-auth-source (host)
  ;; See: https://github.com/karthink/gptel/issues/4
  ;;      https://github.com/tkf/emacs-request/issues/185#issuecomment-654553599
  ;;      -- Library users are supposed to encode multi-byte strings to utf-8
  ;;      unibyte string. --
  (when-let ((api-key (auth-source-pick-first-password :host host)))
    (encode-coding-string api-key 'utf-8)))

(defun image-file->base64-data (file-path)
  (with-temp-buffer
    (insert-file-contents-literally (expand-file-name file-path))
    (base64-encode-region (point-min) (point-max) t)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun image-file->base64-data-uri (file-path)
  (format "data:image/jpeg;base64,%s" (image-file->base64-data file-path)))

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
                                 (propertize (string-fill (nth current-index choices) 80)
                                             'face 'shadow
                                             'cursor t)
                                 (propertize (number-to-string (1+ current-index))
                                             'face 'highlight)
                                 (propertize (number-to-string choice-count)
                                             'face 'highlight))))))

    (define-key completion-keymap (kbd "C-n") show-next-choice)
    (define-key completion-keymap (kbd "TAB") select-current-choice)
    (define-key completion-keymap (kbd "!") replace-with-choice)
    (set-transient-map completion-keymap t (lambda ()
                                             (delete-overlay completion-overlay)))
    (unless (zerop choice-count)
      (funcall show-next-choice))))

(defun llms-prompt-region ()
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (save-excursion
      (cons (window-start) (point)))))

(defun llms-prompt-text ()
  (let ((prompt-region (llms-prompt-region)))
    (buffer-substring-no-properties (car prompt-region)
                                    (cdr prompt-region))))
;;;###autoload
(defun llms-complete (arg)
  (interactive "P")
  (let ((gptel-backend llms-gptel-groq-backend)
        (prompt-text (llms-prompt-text))
        (gptel-mode "llama3-70b-8192")
        (system-prompt
         (if arg (read-string "Instruction: ")
           (concat "Complete the sentence and reply with just the sentence and nothing else."
                   "Your output will be used in a program."
                   "So, make sure that you start your response with the prompt text."))))
    (gptel-request prompt-text
      :system system-prompt
      :callback
      ;; `gptel' cannot handle more than 1 choice in the response yet.
      (lambda (response info)
        (llms--display-choices-as-overlay (list response))))))

(defvar llms-complete-timer nil)
(define-minor-mode llms-complete-minor-mode
  "Minor mode for completing text with LLMs."
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c TAB") #'llms-complete)
    map)

  (let ((llms-complete-timer-fn
         (lambda ()
           (when (and llms-complete-minor-mode
                      (member last-command
                              (list 'self-insert-command
                                    'org-self-insert-command)))
             (llms-complete nil)))))
    (if llms-complete-minor-mode
        (progn (setq llms-complete-timer
                     (run-with-idle-timer 1 t llms-complete-timer-fn))
               (message "LLMS minor mode enabled."))
      (cancel-timer llms-complete-timer)
      (message "LLMS minor mode disabled."))))

(use-package gptel
  :vc ( :url "https://github.com/karthink/gptel"
        :rev :newest )
  :demand t
  :custom ((gptel-use-curl nil))
  :bind ( :map gptel-mode-map
          ("C-j" . gptel-send)
          ("RET" . gptel-send) )
  :config
  (when (boundp 'openai-secret-key)
    (setq gptel-api-key openai-secret-key))

  (require 'gptel-transient)
  (require 'gptel-curl)
  (require 'gptel-gemini)
  (require 'gptel-anthropic)
  (require 'gptel-kagi)

  (add-hook 'gptel-post-response-functions
            #'gptel-end-of-response)

  (defvar llms-gptel-openrouter-backend
    (when-let ((api-key (llms-api-key-from-auth-source "openrouter.ai")))
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

  (defvar llms-gptel-openai-backend
    (gptel-make-openai "OpenAI" :key openai-secret-key))

  (defvar llms-gptel-anthropic-backend
    (gptel-make-anthropic "Anthropic"
      :key (llms-api-key-from-auth-source "api.anthropic.com")))

  (defvar llms-gptel-groq-backend
    (gptel-make-openai "Groq"
      :host "api.groq.com"
      :endpoint "/openai/v1/chat/completions"
      :stream t
      :key (llms-api-key-from-auth-source "api.groq.com")
      :models '("mixtral-8x7b-32768"
                "gemma-7b-it"
                "llama2-70b-4096"
                "llama3-70b-8192")))

  (defvar llms-gptel-preplexity-backend
    (when-let ((api-key (llms-api-key-from-auth-source "api.perplexity.ai")))
      (gptel-make-openai "Perplexity"
        :host "api.perplexity.ai"
        :endpoint "/chat/completions"
        :stream t
        :key api-key
        :models '("sonar-small-chat"
                  "sonar-medium-chat"
                  "sonar-small-online"
                  "sonar-medium-online"))))

  (defvar llms-gptel-gemini-backend
    (when-let ((api-key (llms-api-key-from-auth-source "generativelanguage.googleapis.com")))
      (gptel-make-gemini "Gemini" :key api-key)))

  (defvar llms-gptel-kagi-backend
    (when-let ((api-key (llms-api-key-from-auth-source "kagi.com")))
      (gptel-make-kagi "Kagi"
        :stream t
        :key api-key)))


  (setq gptel-backend llms-gptel-groq-backend
        gptel-model "llama3-70b-8192"))

(use-package chatgpt-shell
  :ensure t
  :custom
  (shell-maker-prompt-before-killing-buffer nil)
  (chatgpt-shell-openai-key openai-secret-key)
  (chatgpt-shell-system-prompt 2)
  (chatgpt-shell-model-version "gpt-4o")

  :init
  ;; Used by `chatgpt-shell-load-awesome-prompts'
  (use-package pcsv :ensure t)

  :config
  (add-to-list 'chatgpt-shell-model-versions "gpt-4o" t))

(use-package dall-e-shell
  :ensure t
  :defer t
  :custom (dall-e-shell-openai-key openai-secret-key))

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
  :disabled t
  :init
  (use-package uuidgen :ensure t))

;;;###autoload
(defun openai-insert-image (prompt)
  "Use PROMPT to ask for image, and insert results in the current buffer."
  (interactive (list (read-string "Describe image: ")))
  (let ((target-buffer (current-buffer)))
    (openai-image prompt
                  (lambda (data)
                    (let-alist data
                      (mapc (lambda (images)
                              (dolist (image images)
                                (with-current-buffer target-buffer
                                  (insert-image (create-image (url-file-local-copy (cdr image)))))))
                            .data))
                    :size "256x256"
                    :n 1
                    :response-format "url"))))

;;; Useful for having a conversation about an image with a model.
(defvar llms--interpret-image-history nil)
(defvar llms-interpret-image-function #'openai-interpret-image)

(defun llms-process-result (result buffer notify)
  (if notify
      (notify result)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (start (point)))
        (insert result)
        (font-lock-ensure start (point))))))

;;;###autoload
(defun openai-interpret-image (file-path &optional instruction notify buffer)
  (interactive "fFile: ")
  (let* ((progress-reporter (make-progress-reporter "Sending request to OpenAI..." 0 1))
         (gptel-backend llms-gptel-openai-backend)
         (gptel-model "gpt-4o")

         (instruction (or instruction (read-string "Instruction: ")))
         (base64-image-data (image-file->base64-data-uri file-path))
         (image-message `( :role  "user"
                           :content  [( :type  "image_url"
                                        :image_url ( :url ,base64-image-data
                                                     :detail  "low" ) )
                                      ( :type  "text"
                                        :text  ,instruction )])))
    (push image-message llms--interpret-image-history)
    (gptel-request (list image-message)
      :callback (lambda (response info)
                  (progress-reporter-done progress-reporter)
                  (if response
                      (llms-process-result response buffer notify)
                    (error "Request to LLM failed: %s" info))))))


;;;###autoload
(defun claude-opus-interpret-image (file-path &optional instruction notify buffer)
  (let* ((image-base64 (image-file->base64-data file-path))
         (image-message `(("role" . "user")
                          ("content" . [(("type" . "image")
                                         ("source" . (("type" . "base64")
                                                      ("media_type" . "image/jpeg")
                                                      ("data" . ,image-base64))))
                                        (("type" . "text")
                                         ("text" . ,instruction))]))))
    (push image-message llms--interpret-image-history)
    (message "Sending request to Anthropic API...")
    (request "https://api.anthropic.com/v1/messages"
      :type "POST"
      :headers `(("x-api-key" . ,(llms-api-key-from-auth-source "api.anthropic.com"))
                 ("anthropic-version" . "2023-06-01")
                 ("content-type" . "application/json"))
      ;; Use Anthropic's fastest model for usual image interpretation
      :data (json-encode `(("model" . "claude-3-haiku-20240307")
                           ("max_tokens" . 1024)
                           ("messages" . [,image-message])))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let-alist (elt (assoc-default 'content data) 0)
                    (llms-process-result .text buffer notify))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error: %S" error-thrown))))))

;;;###autoload
(defun tesseract-openai-interpret-image (file-path &optional instruction notify buffer)
  (interactive "fFile: ")
  (let* ((instruction (or instruction (read-string "Instruction: ")))
         (progress-reporter (make-progress-reporter "Sending request to OpenAI..." 0 1))
         (gptel-backend llms-gptel-openai-backend)
         (gptel-model "gpt-4o")
         (image-text
          (shell-command-to-string (format "tesseract %s -" (shell-quote-argument file-path)))))
    (gptel-request image-text
      :system instruction
      :callback (lambda (response info)
                  (progress-reporter-done progress-reporter)
                  (if response
                      (llms-process-result response buffer notify)
                    (error "Request to LLM failed: %s" info))))    ))
;;;###autload
(defun tesseract-groq-interpret-image (file-path &optional instruction notify buffer)
  (interactive "fFile: ")
  (let* ((instruction (or instruction (read-string "Instruction: ")))
         (progress-reporter (make-progress-reporter "Sending request to OpenAI..." 0 1))
         (gptel-backend llms-gptel-groq-backend)
         (gptel-model "llama3-70b-8192")
         (image-text
          (shell-command-to-string (format "tesseract %s -" (shell-quote-argument file-path)))))
    (gptel-request image-text
      :system instruction
      :callback (lambda (response info)
                  (progress-reporter-done progress-reporter)
                  (if response
                      (llms-process-result response buffer notify)
                    (error "Request to LLM failed: %s" info))))    ))

(defvar llms-screenshot-command "gnome-screenshot")

;;;###autoload
(defun llms-explain-image-with-context ()
  (interactive)
  (let* ((temp-file (make-temp-file "image-with-context-" nil ".jpg"))
         (_ (shell-command (format "%s -p -f %s"
                                   llms-screenshot-command
                                   (shell-quote-argument temp-file))))

         (llm-buffer (get-buffer-create " *LLM Interpretation*"))
         (prompt
          "For a non-native English reader, collect words that might be new or
difficult to understand in the context of the articles in the screenshot
and create a glossary in markdown with concise explanations. Have enough
space between words for readability. Use examples whenever possible to
make explanations more concrete.

Remember to avoid adding extra header and footer text with suggestions
or questions to me.

Example Output Format:

## Word

Concise Explanation about the above Word.")
         (screenshot-image-data-uri ())
         (prepare-buffer (lambda ()
                           (let ((inhibit-read-only t)
                                 (screenshot-image
                                  (create-image temp-file 'imagemagick nil :width (frame-pixel-width))))
                             (end-of-buffer)
                             (insert (format "\n\n# ─[%s ]─\n\n" (current-time-string)))
                             (insert-image screenshot-image
                                           (format "![Captured Image](%s)" (image-file->base64-data-uri temp-file)))
                             (insert "\n\n"))
                           (visual-line-mode +1))))
    (with-current-buffer llm-buffer
      (unless (eq major-mode 'gfm-view-mode)
        (gfm-view-mode))
      (funcall prepare-buffer)
      (funcall llms-interpret-image-function temp-file prompt nil llm-buffer)
      (let ((keymap (current-local-map)))
        (define-key keymap (kbd "q") #'lower-frame)
        (define-key keymap (kbd "G") (lambda ()
                                       (interactive)
                                       (funcall prepare-buffer)
                                       (funcall llms-interpret-image-function
                                                temp-file
                                                (read-string "Instruction: ")
                                                nil
                                                llm-buffer))))
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (eq (current-buffer) llm-buffer)
                    (append-to-file (point-min)
                                    (point-max)
                                    (expand-file-name "var/llm-interpretation.log"
                                                      user-emacs-directory))
                    (delete-file temp-file)))
                nil
                t))
    llm-buffer))

;;;###autoload
(defun llms-switch-image-interpret-function ()
  (interactive)
  (setq llms-interpret-image-function
        (intern (completing-read "Interpret images with: "
                                 `(openai-interpret-image
                                   tesseract-openai-interpret-image
                                   tesseract-groq-interpret-image
                                   claude-opus-interpret-image)))))

(defvar llms-chat-openrouter-models nil)
(defun llms-chat-openrouter-models ()
  (when-let ((api-key (llms-api-key-from-auth-source "openrouter.ai")))
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
                   (mapcar #'car llms-chat--known-llms)
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
  (when-let* ((symbol-bounds (bounds-of-thing-at-point 'symbol))
              (beg (car symbol-bounds))
              (end (cdr symbol-bounds))
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

(defvar llms-chat--known-llms
  ;; An association list each association of which is of the form:
  ;;
  ;; (<llm-name> . (<llm-backend> . <llm-model>))
  ;;
  `(("groq"   . (,llms-gptel-groq-backend       . "llama3-70b-8192"))
    ("opus"   . (,llms-gptel-openrouter-backend . "anthropic/claude-3-opus"))
    ("haiku"  . (,llms-gptel-openrouter-backend . "anthropic/claude-3-haiku"))
    ("openai" . (,llms-gptel-openai-backend     . "gpt-4o"))
    ("pplx"   . (,llms-gptel-preplexity-backend . "sonar-medium-online"))
    ("gemini" . (,llms-gptel-gemini-backend     . "gemini-1.5-pro"))
    ("flash"  . (,llms-gptel-gemini-backend     . "gemini-1.5-flash"))
    ("kagi"   . (,llms-gptel-kagi-backend       . "fastgpt"))))

(defun llms-chat--name->gptel-params (name)
  (or (assoc-default name llms-chat--known-llms)
      (when (llms-chat-openrouter-model name)
        (cons llms-gptel-openrouter-backend name))
      (user-error (format "Unknown LLM: %s" name))))

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

(defun llms-chat--prompt-bounds (&optional start-at-bobp)
  (let ((start (cond
                ((region-active-p) (region-beginning))
                (start-at-bobp (point-min))
                (t (save-excursion (backward-paragraph) (point)))))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position))))
    (cons start end)))

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
4. Use markdown code blocks for code snippets.")

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

;;; ---------------------------------------------------------------------
;;;###autoload
(defun llms-let-them-all-chat! ()
  (interactive)
  (let* ((llm-names (remove "kagi" (mapcar #'car llms-chat--known-llms)))
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


(provide 'llms)
;;; llms.el ends here
