;;; llms-images.el --- LLMs for interpreting images  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience

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
(defvar llms-images--interpret-image-history nil)
(defvar llms-images-interpret-image-function #'openai-interpret-image)

(defun llms-images-process-result (result buffer notify)
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
         (gptel-backend llms-images-chat-gptel-openai-backend)
         (gptel-model 'gpt-4o)

         (instruction (or instruction (read-string "Instruction: ")))
         (base64-image-data (image-file->base64-data-uri file-path))
         (image-message `[( :type  "image_url"
                            :image_url ( :url ,base64-image-data
                                         :detail  "low" ) )
                          ( :type  "text"
                            :text  ,instruction )]))
    (push image-message llms-images--interpret-image-history)
    (gptel-request (list image-message)
      :callback (lambda (response info)
                  (progress-reporter-done progress-reporter)
                  (if response
                      (llms-images-process-result response buffer notify)
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
    (push image-message llms-images--interpret-image-history)
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
                    (llms-images-process-result .text buffer notify))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error: %S" error-thrown))))))

;;;###autoload
(defun tesseract-openai-interpret-image (file-path &optional instruction notify buffer)
  (interactive "fFile: ")
  (let* ((instruction (or instruction (read-string "Instruction: ")))
         (progress-reporter (make-progress-reporter "Sending request to OpenAI..." 0 1))
         (gptel-backend llms-images-chat-gptel-openai-backend)
         (gptel-model 'gpt-4o)
         (image-text
          (shell-command-to-string (format "tesseract %s -" (shell-quote-argument file-path)))))
    (gptel-request image-text
      :system instruction
      :callback (lambda (response info)
                  (progress-reporter-done progress-reporter)
                  (if response
                      (llms-images-process-result response buffer notify)
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
                      (llms-images-process-result response buffer notify)
                    (error "Request to LLM failed: %s" info))))    ))

(defvar llms-images-screenshot-command "gnome-screenshot")
(defvar llms-images-explain-image--default-prompt
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

(defvar llms-images-explain-image--input-image nil)

;;;###autoload
(defun llms-images-explain-image-with-context ()
  (interactive)
  (let* ((image-file (or llms-images-explain-image--input-image
                         (make-temp-file "image-with-context-" nil ".jpg")))
         (_ (unless llms-images-explain-image--input-image
              (shell-command (format "%s -p -f %s"
                                     llms-images-screenshot-command
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
            (funcall llms-images-interpret-image-function
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
      (funcall llms-images-interpret-image-function
               image-file
               (read-string "Prompt: " nil nil llms-images-explain-image--default-prompt)
               nil llm-buffer)

      (let ((keymap (current-local-map)))
        (define-key keymap (kbd "q") #'lower-frame)
        (define-key keymap (kbd "G") reinterpret-image))
      (add-hook 'kill-buffer-hook archive-llm-interaction nil t))

    llm-buffer))

;;;###autoload
(defun llms-images-switch-image-interpret-function ()
  (interactive)
  (setq llms-images-interpret-image-function
        (intern (completing-read "Interpret images with: "
                                 `(openai-interpret-image
                                   tesseract-openai-interpret-image
                                   tesseract-groq-interpret-image
                                   claude-opus-interpret-image)))))


(provide 'llms-images)
;;; llms-images.el ends here
