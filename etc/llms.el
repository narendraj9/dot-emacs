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

(defun image-file->base64-data (file-path)
  (with-temp-buffer
    (insert-file-contents-literally (expand-file-name file-path))
    (base64-encode-region (point-min) (point-max) t)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun image-file->base64-data-uri (file-path)
  (format "data:image/jpeg;base64,%s" (image-file->base64-data file-path)))

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

(use-package llms-chat
  :load-path "packages/rest/llms-chat"
  :demand t
  :custom (llms-chat-include-model-usage-info t)
  :config
  (require 'llms-usage)
  (add-hook 'llms-chat-post-response-hook
            #'chatgpt-shell--put-source-block-overlays))

(use-package gptel
  :vc ( :url "https://github.com/karthink/gptel"
        :rev :newest )
  :demand t
  :custom ((gptel-use-curl nil)
           (gtpel-expert-commands t))
  :bind ( :map gptel-mode-map
          ("C-j" . gptel-send)
          ("RET" . gptel-send) )
  :config
  (setq gptel-api-key
        (auth-source-pick-first-password :host "api.openai.com"))

  (require 'gptel-transient)
  (require 'gptel-curl)
  (require 'gptel-gemini)
  (require 'gptel-anthropic)
  (require 'gptel-kagi)

  (add-hook 'gptel-post-response-functions
            #'gptel-end-of-response)

  (setq gptel-backend llms-chat-gptel-groq-backend
        gptel-model 'llama-3.3-70b-versatile))

(use-package aidermacs
  :ensure t
  :bind ( :map ctl-m-map
          ("a" . aidermacs-transient-menu) )

  :custom
  (aidermacs-extra-args '("--no-gitignore"))
  (aidermacs-show-diff-after-change t)
  (aidermacs-backend 'vterm)
  (aidermacs-use-architect-mode t)
  (aidermacs-architect-model "o1")
  (aidermacs-default-model "gpt-4o")

  :config
  (setenv "OPENAI_API_KEY"
          (auth-source-pick-first-password :host "api.openai.com"))

  (add-to-list 'display-buffer-alist
               '("\\*aidermacs:.*\\*" display-buffer-in-direction
                 (direction . right)
                 (window-width . 0.5))))


;;; Expermients
;; ──────────────────────────────────────────────────────────────────

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
  (let ((gptel-backend llms-chat-gptel-groq-backend)
        (prompt-text (llms-prompt-text))
        (gptel-model 'llama-3.1-70b-versatile)
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
         (gptel-backend llms-chat-gptel-openai-backend)
         (gptel-model 'gpt-4o)

         (instruction (or instruction (read-string "Instruction: ")))
         (base64-image-data (image-file->base64-data-uri file-path))
         (image-message `[( :type  "image_url"
                            :image_url ( :url ,base64-image-data
                                         :detail  "low" ) )
                          ( :type  "text"
                            :text  ,instruction )]))
    (push image-message llms--interpret-image-history)
    (gptel-request (list image-message)
      :callback (lambda (response info)
                  (progress-reporter-done progress-reporter)
                  (if response
                      (llms-process-result response buffer notify)
                    (error "Request to LLM failed: %s" info))))))


;;;###autoload
(defun claude-opus-interpret-image (file-path &optional instruction notify buffer)
  (let* ((api-key (encode-coding-string (auth-source-pick-first-password :host "api.anthropic.com")
                                        'utf-8))
         (image-base64 (image-file->base64-data file-path))
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
      :headers `(("x-api-key" . ,api-key)
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
         (gptel-backend llms-chat-gptel-openai-backend)
         (gptel-model 'gpt-4o)
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
         (gptel-backend llms-chat-gptel-groq-backend)
         (gptel-model 'llama-3.1-70b-versatile)
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
(defvar llms-explain-image--default-prompt
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

(defvar llms-explain-image--input-image nil)

;;;###autoload
(defun llms-explain-image-with-context ()
  (interactive)
  (let* ((image-file (or llms-explain-image--input-image
                         (make-temp-file "image-with-context-" nil ".jpg")))
         (_ (unless llms-explain-image--input-image
              (shell-command (format "%s -p -f %s"
                                     llms-screenshot-command
                                     (shell-quote-argument image-file)))))

         (llm-buffer (get-buffer-create " *LLM Interpretation*"))
         (screenshot-image-data-uri ())
         (prepare-buffer
          (lambda ()
            (let ((inhibit-read-only t)
                  (screenshot-image
                   (create-image image-file nil nil :width (frame-pixel-width))))
              (end-of-buffer)
              (insert (format "\n\n# ─[%s ]─\n\n" (current-time-string)))
              (insert-image screenshot-image
                            (format "![Captured Image](%s)" (image-file->base64-data-uri image-file)))
              (insert "\n\n"))
            (visual-line-mode +1)))
         (reinterpret-image
          (lambda ()
            (interactive)
            (funcall prepare-buffer)
            (funcall llms-interpret-image-function
                     image-file
                     (read-string "Instruction: ")
                     nil
                     llm-buffer)))
         (archive-llm-interaction
          (lambda ()
            (when (eq (current-buffer) llm-buffer)
              (append-to-file (point-min)
                              (point-max)
                              (expand-file-name "var/llm-interpretation.log"
                                                user-emacs-directory))
              (delete-file image-file)))))
    (with-current-buffer llm-buffer
      (unless (eq major-mode 'gfm-view-mode)
        (gfm-view-mode))
      (let ((display-buffer-alist '((".*" display-buffer-full-frame))))
        (funcall prepare-buffer)
        (display-buffer llm-buffer))
      (funcall llms-interpret-image-function
               image-file
               (read-string "Prompt: " nil nil llms-explain-image--default-prompt)
               nil llm-buffer)

      (let ((keymap (current-local-map)))
        (define-key keymap (kbd "q") #'lower-frame)
        (define-key keymap (kbd "G") reinterpret-image))
      (add-hook 'kill-buffer-hook archive-llm-interaction nil t))

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

;;; *Experiment* : An AI Companion (e.g. speedbar-mode but with custom
;;; *instructions).

(defvar llms-spin-up-companion--saved-window-config nil)

(defun llms-spin-up-companion-stop ()
  (interactive)
  (kill-buffer (get-buffer " *LLM Companion* "))
  (when (window-configuration-p llms-spin-up-companion--saved-window-config)
    (set-window-configuration llms-spin-up-companion--saved-window-config)))

;;;###autoload
(defun llms-spin-up-companion (instruction)
  (interactive "sInstruction: ")
  (let* ((attached-buffer (current-buffer))
         (window-configuration (current-window-configuration))
         (stop (lambda ()
                 (kill-buffer buffer)
                 (set-window-configuration window-configuration)))
         (buffer (get-buffer-create " *LLM Companion* "))
         (refresh-buffer
          (lambda ()
            (interactive)
            (when (and (eq (current-buffer) attached-buffer))
              (let ((gptel-backend llms-chat-gptel-openai-backend)
                    (gptel-model 'gpt-4o)
                    (text (if (region-active-p)
                              (buffer-substring (region-beginning) (region-end))
                            (save-excursion (backward-paragraph)
                                            (buffer-substring (point)
                                                              (progn (forward-paragraph)
                                                                     (point))))))
                    (inhibit-read-only t))
                (with-current-buffer buffer
                  (setq diff-mode-read-only nil)
                  (erase-buffer)
                  (setq header-line-format
                        (format "Last Updated: %s\n" (current-time-string)))
                  (insert text)
                  (gptel-request nil
                    :system instruction
                    :in-place t
                    :callback
                    (lambda (response info)
                      (let ((inhibit-read-only t)
                            (diff-use-labels nil)
                            (diff-command "delta"))
                        (with-temp-buffer
                          (if (not response)
                              (error "Error talking to LLM API" info)
                            (insert response)
                            (diff-no-select buffer (current-buffer)
                                            ""
                                            ;; If using `difftastic', the
                                            ;; following flags make sense:
                                            ;; (format "--color always --display side-by-side --width %s --exit-code --strip-cr on"
                                            ;;         (shell-quote-argument (number-to-string (window-text-width))))
                                            t
                                            buffer)
                            (with-current-buffer buffer
                              (ansi-color-apply-on-region (point-min) (point-max))))))))
                  (chatgpt-shell--put-source-block-overlays)))))))
    (add-to-list 'display-buffer-alist
                 `(,(buffer-name buffer) display-buffer-in-direction
                   (window . main)
                   (direction . right)
                   (window-width . 0.4)))
    (setq llms-spin-up-companion--saved-window-config window-configuration)
    (display-buffer buffer)
    (funcall refresh-buffer)
    (with-current-buffer attached-buffer
      (define-key (current-local-map)
                  (kbd "C-c r")
                  refresh-buffer))))

(provide 'llms)
;;; llms.el ends here
