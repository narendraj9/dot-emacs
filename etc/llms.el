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
      (cons (window-start) (point)))))


(defun llms-prompt-text ()
  (let ((prompt-region (llms-prompt-region)))
    (buffer-substring-no-properties (car prompt-region)
                                    (cdr prompt-region))))


(use-package gptel
  :git "https://github.com/karthink/gptel"
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

  :config
  (require 'gptel-anthropic)

  ;; Register Groq as a backend with gptel.
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

  ;; A gptel backend for perplexity API
  (defvar llms-gptel-preplexity-backend
    (when-let ((api-key (auth-source-pick-first-password :host "api.perplexity.ai")))
      (gptel-make-openai "Perplexity"
        :host "api.perplexity.ai"
        :protocol "https"
        :endpoint "/chat/completions"
        :stream t
        :key api-key
        :models '("sonar-small-chat"
                  "sonar-medium-chat"
                  "sonar-small-online"
                  "sonar-medium-online"))))

  (setq gptel-backend llms-gptel-groq-backend
        gptel-model "llama3-70b-8192"))


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

(use-package chatgpt-shell
  :demand t
  :ensure t
  :custom
  (chatgpt-shell-openai-key openai-secret-key)
  (chatgpt-shell-system-prompt 2)
  (chatgpt-shell-model-version "gpt-4o")

  :init
  ;; Used by `chatgpt-shell-load-awesome-prompts'
  (use-package pcsv :ensure t)

  :config
  (add-to-list 'chatgpt-shell-model-versions "gpt-4o" t)

  :bind ( :map ctl-quote-map
          ("t C" . chatgpt-shell)
          ("t u" . chatgpt-shell-generate-unit-test)
          ("t e" . chatgpt-shell-explain-code)
          ("t i" . chatgpt-shell-interrupt)
          ("t p" . chatgpt-shell-proofread-region)
          ("t S" . chatgpt-shell-send-region)
          ("t d" . chatgpt-shell-describe-code)
          ("t r" . chatgpt-shell-refactor-code)
          ("t g" . chatgpt-shell-write-git-commit)
          ("t s" . chatgpt-shell-send-and-review-region)
          ("t R" . chatgpt-shell-restore-session-from-transcript) ))


(use-package copilot
  :git "https://github.com/copilot-emacs/copilot.el.git"
  :demand t
  :bind ( :map copilot-completion-map
          ("C-c C-c" . copilot-accept-completion) )

  :init
  (use-package editorconfig :ensure t)

  :custom ( (copilot-indent-offset-warning-disable t)
            (copilot-max-char -1) ))


(use-package cody
  :git "https://github.com/sourcegraph/emacs-cody.git"
  :disabled t
  :init
  (use-package uuidgen :ensure t))


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
                                    (concat " " .content))))
                              choices))))
                 :base-url "https://api.groq.com/openai/v1"
                 :key (auth-source-pick-first-password :host "api.groq.com")
                 :model "llama3-70b-8192"
                 :temperature openai-chat-temperature
                 :user (unless (string= openai-user "user") openai-user))))


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


(defvar gptel-quick-proofreader--history nil)


;;;###autoload
(defun gptel-ask-quickly (system-prompt)
  (interactive "sInstruction: ")
  (let* ((prompt
          (encode-coding-string (if (region-active-p)
                                    (buffer-substring-no-properties (region-beginning) (region-end))
                                  (read-string "Use markdown for the response and be very very concise. \nContext: "
                                               nil
                                               gptel-quick-proofreader--history))
                                'utf-8))
         (progress-reporter
          (make-progress-reporter "Communicating to OpenAI API..." 0  1))

         (gptel-backend llms-gptel-groq-backend)
         (gptel-model "llama3-70b-8192")
         (gptel-use-curl t)
         (gptel-response-callback
          (lambda (response info)
            (progress-reporter-done progress-reporter)
            (if (not response)
                (message "gptel-quick failed with message: %s" (plist-get info :status))
              (with-current-buffer (get-buffer-create "*gptel-quick*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert response)
                  (gfm-view-mode)
                  (visual-line-mode)
                  (font-lock-fontify-buffer)
                  (display-buffer (current-buffer)
                                  `((display-buffer-in-side-window)
                                    (side . right)
                                    (window-width . 60)))))))))

    (when (string= prompt "") (user-error "A prompt is required."))
    (gptel-request prompt
      :system system-prompt
      :callback gptel-response-callback)))


;;;###autoload
(defun gptel-quick-proofreader ()
  (interactive)
  (gptel-ask-quickly "I want you act as a proofreader. Please format your replies as
text in markdown format and for readability on an 80-columns
display. I will provide you texts and I would like you to review
them for any spelling, grammar, or punctuation errors. Once you
have finished reviewing the text, provide me with any necessary
corrections or suggestions for improve the text."))


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

;;; Advice to HACK gptel to include an image in the beginning.
;;; -----------------------------------------------------------
(defun gptel--include-last-image-in-conversation (result)
  (let ((gptel-formatted-history
         (mapcar (-lambda ((&alist "role" role "content" content))
                   (-let (([(&alist "source" source "image_url" image-url)] content))
                     (cond
                      ((string= (gptel-backend-name gptel-backend) "Anthropic")
                       `( :role ,role
                          :content [( :type  "image"
                                      :source  ( :type  "base64"
                                                 :media_type  "image/jpeg"
                                                 :data  ,(assoc-default "data" source)))]))
                      ((string= (gptel-backend-name gptel-backend) "ChatGPT")
                       `( :role ,role
                          :content [( :type  "image_url"
                                      :image_url  ( :url  ,(assoc-default "url" image-url)
                                                    :details "low" ))])))))
                 ;; Only include the latest image.
                 (take 1 llms--interpret-image-history))))
    (seq-concatenate 'list
                     gptel-formatted-history
                     ;; Yet another hack.
                     `(( :role "assistant"
                         :content "I am taking a minute to articulate my thoughts."))
                     result)))

(defun llms-enable-image-in-gptel-conversation ()
  (interactive)
  (advice-add 'gptel--parse-buffer :filter-return #'gptel--include-last-image-in-conversation))

(defun llms-disable-image-in-gptel-conversation ()
  (interactive)
  (advice-remove 'gptel--parse-buffer #'gptel--include-last-image-in-conversation))

;;; End of HACK

(defun llms-view-inline-image ()
  "Replaces base64 data url at point with a rendered image. "
  (interactive)
  (beginning-of-line)
  (let ((temp-file (make-temp-file "llm-image-" nil ".jpg"))
        (data (-> (search-forward "data:image/jpeg;base64," (line-end-position))
                  (buffer-substring-no-properties (1- (search-forward ")" (line-end-position)))))))
    (with-temp-file temp-file
      (insert data)
      (base64-decode-region (point-min) (point-max)))
    (insert-image (create-image temp-file 'imagemagick nil :width (frame-pixel-width)))))

;;;###autoload
(defun llms-switch-image-interpret-function ()
  (interactive)
  (setq llms-interpret-image-function
        (intern (completing-read "Interpret images with: "
                                 `(openai-interpret-image
                                   tesseract-openai-interpret-image
                                   tesseract-groq-interpret-image
                                   claude-opus-interpret-image)))))


(defvar llms-complete-timer nil)
(define-minor-mode llms-complete-minor-mode
  "Minor mode for completing text with LLMs."
  :lighter "LLMS"
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



(provide 'llms)
;;; llms.el ends here
