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

(use-package request :ensure t :demand t)
(use-package spinner :ensure t :demand t)

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
  (let ((prompt-text (llms-prompt-text))
        (instruction (if arg (read-string "Instruction: ")
                       (concat "Complete the sentence and reply with just the sentence and nothing
else. Your output will be used in a program so make sure that you start your response with the promt text."))))
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
                                    (string-remove-prefix prompt-text
                                                          .content))))
                              choices))))
                 :base-url "https://api.groq.com/openai/v1"
                 :key (auth-source-pick-first-password :host "api.groq.com")
                 :model "llama3-70b-8192"
                 :temperature openai-chat-temperature
                 :user (unless (string= openai-user "user") openai-user))))

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
  :bind ( :map gptel-mode-map
          ("C-j" . gptel-send)
          ("RET" . gptel-send) )

  :custom ((gptel-use-curl nil))
  :init
  (when (boundp 'openai-secret-key)
    (setq gptel-api-key openai-secret-key))

  (require 'gptel-transient)
  (require 'gptel-curl)
  (require 'gptel-gemini)

  :config
  (require 'gptel-anthropic)

  (defvar llms-gptel-groq-backend
    (gptel-make-openai "Groq"
      :host "api.groq.com"
      :endpoint "/openai/v1/chat/completions"
      :stream t
      :key (auth-source-pick-first-password :host "api.groq.com")
      :models '("mixtral-8x7b-32768"
                "gemma-7b-it"
                "llama2-70b-4096"
                "llama3-70b-8192")))

  (defvar llms-gptel-preplexity-backend
    (when-let ((api-key (auth-source-pick-first-password :host "api.perplexity.ai")))
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
    (when-let ((api-key (auth-source-pick-first-password :host "generativelanguage.googleapis.com")))
      (gptel-make-gemini "Gemini" :key api-key)))

  (defvar llms-gptel-kagi-backend
    (when-let ((api-key (auth-source-pick-first-password :host "kagi.com")))
      (gptel-make-kagi "Kagi"
        :stream t
        :key api-key)))


  (setq gptel-backend llms-gptel-groq-backend
        gptel-model "llama3-70b-8192"))

(use-package openai
  :vc ( :url "https://github.com/emacs-openai/openai"
        :rev :newest )
  :defer t
  :init
  (use-package tblui :ensure t)
  ;; The same variable sets up the user role.
  ;; (setq openai-user (user-full-name))
  (when (boundp 'openai-secret-key)
    (setq openai-key openai-secret-key))

  (require 'openai-edit)
  (require 'openai-chat)
  (require 'openai-image))

(use-package chatgpt-shell
  :ensure t
  :custom
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
  (when buffer (set-buffer buffer))
  (if notify (notify result)
    (let ((inhibit-read-only t)
          (start (point)))
      (insert result)
      (font-lock-ensure start (point)))))

;;;###autoload
(defun openai-interpret-image (file-path &optional instruction notify buffer)
  (interactive "fFile: ")
  (let* ((result "")
         (instruction (or instruction (read-string "Instruction: ")))
         (progress-reporter (make-progress-reporter "Sending request to OpenAI..." 0 1))
         (base64-image-data (image-file->base64-data-uri file-path))
         (image-message `(("role"    . "user")
                          ("content" . [(("type" . "image_url")
                                         ("image_url" . (("url" . ,base64-image-data)
                                                         ("detail" . "low"))))
                                        (("type" . "text")
                                         ("text" . ,instruction))]))))
    (push image-message llms--interpret-image-history)
    (openai-chat `[,image-message]
                 (lambda (data)
                   (progress-reporter-done progress-reporter)
                   (let ((choices (let-alist data .choices)))
                     (dolist (item (mapcar (lambda (choice)
                                             (let-alist choice
                                               (let-alist .message
                                                 (concat " " .content))))
                                           choices))
                       (setq result (format "%s %s\n" result item))))
                   (llms-process-result result buffer notify))
                 :max-tokens 3000
                 ;; gpt-4o is cheaper than gpt-4o.
                 :model "gpt-4o")))


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
      :headers `(("x-api-key" . ,(auth-source-pick-first-password :host "api.anthropic.com"))
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
  (let* ((result "")

         (instruction (or instruction (read-string "Instruction: ")))

         (progress-reporter
          (make-progress-reporter "Sending request to OpenAI..." 0 1))

         (image-text
          (shell-command-to-string (format "tesseract %s -" (shell-quote-argument file-path)))))
    (openai-chat `[(("role"    . "system")
                    ("content" . ,(format "%s. %s"
                                          instruction
                                          "For your convenience, I have converted the image to text using
tesseract. Include a 4 sentence summary at the beginning of the output please.")))
                   (("role"    . "user")
                    ("content" . ,image-text))]
                 (lambda (data)
                   (progress-reporter-done progress-reporter)
                   (let ((choices (let-alist data .choices)))
                     (dolist (item (mapcar (lambda (choice)
                                             (let-alist choice
                                               (let-alist .message
                                                 (concat " " .content))))
                                           choices))
                       (setq result (format "%s %s\n" result item))))
                   (llms-process-result result buffer notify))
                 :max-tokens 3000
                 :model "gpt-4o")))
;;;###autload
(defun tesseract-groq-interpret-image (file-path &optional instruction notify buffer)
  (interactive "fFile: ")
  (let* ((result "")

         (instruction (or instruction (read-string "Instruction: ")))

         (image-text
          (progn (message "Using tesseract to convert image to text.")
                 (shell-command-to-string (format "tesseract %s -" (shell-quote-argument file-path)))))

         (progress-reporter
          (make-progress-reporter "Sending request to Groq Cloud API..." 0 1)))
    (openai-chat `[(("role"    . "system")
                    ("content" . ,(format "%s. %s"
                                          instruction
                                          "For your convenience, I have converted the image to text using
tesseract. Include a 4 sentence summary at the beginning of the output please.")))
                   (("role"    . "user")
                    ("content" . ,image-text))]
                 (lambda (data)
                   (progress-reporter-done progress-reporter)
                   (let ((choices (let-alist data .choices)))
                     (dolist (item (mapcar (lambda (choice)
                                             (let-alist choice
                                               (let-alist .message
                                                 (concat " " .content))))
                                           choices))
                       (setq result (format "%s %s\n" result item))))
                   (llms-process-result result buffer notify))
                 :max-tokens 3000
                 :base-url "https://api.groq.com/openai/v1"
                 :key (auth-source-pick-first-password :host "api.groq.com")
                 :model "llama3-70b-8192")))

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

;;; ──────────────────────────────────────────────────────────────────
;; Functions to have a conversation with LLMs using a chat/mention interface.
;;

(defun llms-chat--name->gptel-backend (name)
  (cond
   ((string= name "groq")
    (cons llms-gptel-groq-backend "llama3-70b-8192"))

   ((string= name "openai")
    (cons gptel--openai "gpt-4o"))

   ((string= name "pplx")
    (cons llms-gptel-preplexity-backend "sonar-medium-online"))

   ((string= name "gemini")
    (cons llms-gptel-gemini-backend "gemini-pro"))

   ((string= name "kagi")
    (cons llms-gptel-kagi-backend "summarize:cecil"))

   (t
    (user-error (format "Unknown LLM: %s" name)))))


(defun llms-chat-make-progress-indicator (starting-point)
  (let ((indicate-progress-p t)
        (spinner (spinner-create 'box-in-circle))
        (indicator-overlay (make-overlay starting-point starting-point)))
    (spinner-start spinner)
    ;; If you end up with orphaned threads, send them error/quit signals using
    ;; the UI provided by `list-threads'.
    (make-thread
     (lambda ()
       (while indicate-progress-p
         (overlay-put indicator-overlay 'after-string (spinner-print spinner))
         (sleep-for 0.2))))
    (lambda ()
      (setq indicate-progress-p nil)
      (delete-overlay indicator-overlay))))

(defun llms-chat-stop-progress-indicator (progress-indicator)
  (funcall progress-indicator))


(defun llms-chat--remove-old-reply (prompt-id)
  ;; Try removing existing response.
  (when-let* ((reply-start
               (text-property-any (point) (point-max) :llm-role :assistant))
              (reply-end
               (and (string= prompt-id (get-text-property reply-start :llm-prompt-id))
                    (or (text-property-not-all reply-start (point-max)
                                               :llm-prompt-id prompt-id)
                        ;; All of the buffer is filled with reply from an LLM
                        ;; till the end of buffer.
                        (point-max)))))
    (delete-region reply-start reply-end)))

(defun llms-chat--insert-reply (prompt-id position response-text)
  (goto-char position)
  (let* ((color-% 18)
         (background-color (color-lighten-name (background-color-at-point) color-%))
         (foreground-color (color-darken-name (foreground-color-at-point) color-%))
         (face (list :background background-color :foreground foreground-color)))
    (insert response-text)
    (add-text-properties position (point)
                         (list :llm-prompt-id prompt-id
                               :llm-role :assistant
                               'face face
                               'font-lock-face face))))

(defun llms-chat--prompt-bounds ()
  (let ((start (if (region-active-p)
                   (region-beginning)
                 (save-excursion (backward-paragraph)
                                 (point))))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position))))
    (cons start end)))

(defun llms-chat--prompt-id (prompt-bounds)
  (let ((prompt-start-position (car prompt-bounds))
        (org-id-method 'ts))
    (or (get-text-property prompt-start-position :llm-prompt-id)
        (org-id-new "llm"))))

(defun llms-chat--llm-name ()
  (let* ((llm-name-regex "@\\([a-z]+\\)\\(\\S+\\|$\\)")
         (llm-name (save-excursion
                     (and (re-search-backward llm-name-regex (window-start) t)
                          (buffer-substring (match-beginning 1) (match-end 1))))))
    (unless llm-name
      (user-error "No LLM name found in the prompt."))
    llm-name))

(defvar llms-chat--system-prompt
  "Follow these rules no matter what:
1. Be factually correct. If you are not sure about something, don't say
anything at all.
2. Try to respond with text that is made to fit in 80 columns.
3. Be concise.
4. Answer in less than 8 sentences but make sure to provide a complete
answer. Use mathematical equations if that helps.")

;;;autoload
(defun llms-chat ()
  "Talk to LLMs as if you are chatting to them,

@openai: current world population is.. (one sentence)
@openai:

As of 2023, the estimated world population is approximately 8 billion.
"
  (interactive)
  (save-excursion
    (let* ((original-point (point))
           (llm-name (llms-chat--llm-name))

           (gptel-params (llms-chat--name->gptel-backend llm-name))
           (gptel-backend (car gptel-params))
           (gptel-model (cdr gptel-params))

           ;; A struct (position, id, etc.) probably makes more sense here.
           (prompt-bounds (llms-chat--prompt-bounds))
           (prompt-start-position (car prompt-bounds))
           (prompt-end-position (cdr prompt-bounds))
           (prompt (buffer-substring-no-properties prompt-start-position prompt-end-position))
           (prompt-id (llms-chat--prompt-id prompt-bounds)))

      (unless (and gptel-backend gptel-model)
        (error "No backend found for LLM: %s" llm-name))
      (when (s-blank-str? prompt)
        (user-error "No prompt found."))
      (pulse-momentary-highlight-region prompt-start-position prompt-end-position)
      (add-text-properties prompt-start-position
                           prompt-end-position
                           (list :llm-prompt-id prompt-id
                                 :llm-role :user
                                 'face 'bold
                                 'font-lock-face 'bold))

      (llms-chat--remove-old-reply prompt-id)

      (goto-char original-point)
      (insert (propertize (format "%s@%s: "
                                  (if (looking-back "^\\S+") "" "\n")
                                  llm-name)
                          :llm-prompt-id prompt-id
                          :llm-role :assistant))


      (let ((progress-indicator (llms-chat-make-progress-indicator (point))))
        (gptel-request prompt
          :system llms-chat--system-prompt
          :position (point)
          :callback
          (lambda (response info)
            (llms-chat-stop-progress-indicator progress-indicator)
            (if (not response)
                (error "Error talking to LLM %s: %s" llm-name info)
              (let ((position (marker-position (plist-get info :position)))
                    (buffer  (buffer-name (plist-get info :buffer))))
                (with-current-buffer buffer
                  (llms-chat--insert-reply prompt-id position response)
                  (goto-char original-point))))))))))


(provide 'llms)
;;; llms.el ends here
