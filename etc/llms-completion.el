;;; llms-completion.el --- Using LLMs to auto-complete.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords:

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

(provide 'llms-completion)
;;; llms-completion.el ends here
