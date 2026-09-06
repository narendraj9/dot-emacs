;;; llms-langs.el --- LLM helpers for language learning.  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: data, convenience, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Helpers for using LLMs while learning languages.

;;; Code:

(require 'auth-source)
(require 'subr-x)

(defvar llms-speaker-vocab-script
  (expand-file-name "bin/speaker-vocab.sh" user-emacs-directory)
  "Script used by `llms-speaker-vocab'.")

(defvar llms-speaker-vocab-chunk-seconds 5
  "Number of seconds of speaker audio to process at a time.")

(defvar llms-speaker-vocab-poll-seconds 1
  "How often to look for completed speaker audio chunks.")

(defvar llms-speaker-vocab-watch-max-completed-chunks 1
  "Maximum completed chunks to keep queued in watch mode.
Older completed chunks are dropped so the overlay stays close to the episode.")

(defvar llms-speaker-vocab-language "de"
  "Language code sent to the speech-to-text backend.")

(defvar llms-speaker-vocab-target-language "German"
  "Language being learned by `llms-speaker-vocab'.")

(defvar llms-speaker-vocab-learner-level "B1"
  "Learner/exam level used for vocabulary selection.")

(defvar llms-speaker-vocab-groq-stt-model "whisper-large-v3-turbo"
  "Groq speech-to-text model used by `llms-speaker-vocab'.")

(defvar llms-speaker-vocab-groq-model "llama-3.3-70b-versatile"
  "Groq chat model used by `llms-speaker-vocab'.")

(defvar llms-speaker-vocab-known-words-file nil
  "Optional newline-delimited file of already-known words to skip.")

(defvar llms-speaker-vocab-latest-file
  (expand-file-name "var/speaker-vocab-latest.txt" user-emacs-directory)
  "File where `llms-speaker-vocab-watch' asks the script to write latest help.")

(defvar llms-speaker-vocab-overlay-buffer-name "*speaker vocab overlay*"
  "Buffer name used for the small speaker vocabulary overlay.")

(defvar llms-speaker-vocab-overlay-frame nil
  "Frame used by `llms-speaker-vocab-watch'.")

(defvar llms-speaker-vocab-overlay-process nil
  "Process running `llms-speaker-vocab-script' for the overlay.")

(defvar llms-speaker-vocab-overlay-timer nil
  "Timer refreshing the speaker vocabulary overlay buffer.")

(defun llms-langs--groq-api-key ()
  "Return the Groq API key using the auth-source helpers used in this config."
  ;; `gptel-api-key-from-auth-source' is the main helper used elsewhere in this
  ;; repo.  Load gptel lazily if necessary, and keep the llms-chat helper as a
  ;; fallback because that is also used for the Groq backend in this config.
  (require 'gptel nil t)
  (or (and (fboundp 'gptel-api-key-from-auth-source)
           (ignore-errors (gptel-api-key-from-auth-source "api.groq.com")))
      (and (fboundp 'llms-chat--api-key-from-auth-source)
           (llms-chat--api-key-from-auth-source "api.groq.com"))))

(defun llms-langs--speaker-vocab-environment (&optional display-mode latest-file)
  "Return process environment for `llms-speaker-vocab-script'.
DISPLAY-MODE is passed as DISPLAY_MODE.  LATEST-FILE, when non-nil, is
passed as LATEST_FILE."
  (let ((groq-api-key (llms-langs--groq-api-key)))
    (unless groq-api-key
      (user-error "No Groq API key found in auth-source for api.groq.com"))
    (append
     (delq nil
           (list (format "GROQ_API_KEY=%s" (string-trim groq-api-key))
                 "STT=groq"
                 "LLM=groq"
                 (format "DISPLAY_MODE=%s" (or display-mode "normal"))
                 (format "CHUNK_SECONDS=%s" llms-speaker-vocab-chunk-seconds)
                 (format "POLL_SECONDS=%s" llms-speaker-vocab-poll-seconds)
                 (format "WATCH_MAX_COMPLETED_CHUNKS=%s" llms-speaker-vocab-watch-max-completed-chunks)
                 (format "LANGUAGE=%s" llms-speaker-vocab-language)
                 (format "TARGET_LANGUAGE=%s" llms-speaker-vocab-target-language)
                 (format "LEARNER_LEVEL=%s" llms-speaker-vocab-learner-level)
                 (format "GROQ_STT_MODEL=%s" llms-speaker-vocab-groq-stt-model)
                 (format "GROQ_MODEL=%s" llms-speaker-vocab-groq-model)
                 (when latest-file
                   (format "LATEST_FILE=%s" latest-file))
                 (when (and llms-speaker-vocab-known-words-file
                            (file-readable-p llms-speaker-vocab-known-words-file))
                   (format "KNOWN_WORDS_FILE=%s" llms-speaker-vocab-known-words-file))))
     process-environment)))

(defun llms-langs--speaker-vocab-command (&optional once)
  "Return command list for `llms-speaker-vocab-script'."
  (unless (file-executable-p llms-speaker-vocab-script)
    (user-error "Script is not executable: %s" llms-speaker-vocab-script))
  (list llms-speaker-vocab-script (if once "once" "start")))

;;;###autoload
(defun llms-speaker-vocab (&optional once)
  "Listen to speaker audio and show vocabulary help using Groq.

This runs `llms-speaker-vocab-script' with STT=groq and LLM=groq,
passing GROQ_API_KEY from auth-source.  With prefix argument ONCE,
process just one audio chunk; otherwise keep running until the
compilation process is killed."
  (interactive "P")
  (let ((process-environment (llms-langs--speaker-vocab-environment "normal"))
        (compilation-buffer-name-function
         (lambda (_mode) "*speaker vocab*")))
    (compile
     (mapconcat #'shell-quote-argument
                (llms-langs--speaker-vocab-command once)
                " "))))

(defun llms-langs--speaker-vocab-refresh-overlay (latest-file buffer)
  "Refresh BUFFER from LATEST-FILE."
  (when (file-readable-p latest-file)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (text (with-temp-buffer
                    (insert-file-contents latest-file)
                    (buffer-string))))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (visual-line-mode 1)
        (setq-local mode-line-format nil)
        (setq-local cursor-type nil)
        (read-only-mode 1)))))

(defun llms-langs--speaker-vocab-display-overlay (buffer)
  "Display BUFFER in a small undecorated frame."
  (unless (frame-live-p llms-speaker-vocab-overlay-frame)
    (setq llms-speaker-vocab-overlay-frame
          (make-frame '((name . "speaker vocab")
                        (minibuffer . nil)
                        (undecorated . t)
                        (skip-taskbar . t)
                        (z-group . above)
                        (alpha-background . 88)
                        (width . 72)
                        (height . 16)
                        (left . 40)
                        (top . 60)))))
  (with-selected-frame llms-speaker-vocab-overlay-frame
    (switch-to-buffer buffer)
    (set-window-dedicated-p (selected-window) t)))

;;;###autoload
(defun llms-speaker-vocab-watch (&optional once)
  "Show live speaker vocabulary help in a small overlay frame.

This is intended for watching TV in another window.  It runs the same Groq
speech-to-text/vocabulary pipeline as `llms-speaker-vocab', but asks the bash
script for compact overlay text and refreshes only the latest result.  With
prefix argument ONCE, process just one audio chunk."
  (interactive "P")
  (llms-speaker-vocab-watch-stop)
  (make-directory (file-name-directory llms-speaker-vocab-latest-file) t)
  (with-temp-file llms-speaker-vocab-latest-file
    (insert "Starting speaker vocabulary helper…\n"))
  (let* ((buffer (get-buffer-create llms-speaker-vocab-overlay-buffer-name))
         (log-buffer (get-buffer-create "*speaker vocab log*"))
         (process-environment
          (llms-langs--speaker-vocab-environment
           "watch" llms-speaker-vocab-latest-file)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Starting speaker vocabulary helper…\n")
        (special-mode)))
    (llms-langs--speaker-vocab-display-overlay buffer)
    (setq llms-speaker-vocab-overlay-process
          (make-process
           :name "speaker-vocab"
           :buffer log-buffer
           :command (llms-langs--speaker-vocab-command once)
           :noquery t
           :sentinel (lambda (_process event)
                       (message "speaker-vocab: %s" (string-trim event)))))
    (setq llms-speaker-vocab-overlay-timer
          (run-at-time 0 1 #'llms-langs--speaker-vocab-refresh-overlay
                       llms-speaker-vocab-latest-file buffer))))

;;;###autoload
(defun llms-speaker-vocab-watch-stop ()
  "Stop the speaker vocabulary overlay and its background process."
  (interactive)
  (when (timerp llms-speaker-vocab-overlay-timer)
    (cancel-timer llms-speaker-vocab-overlay-timer)
    (setq llms-speaker-vocab-overlay-timer nil))
  (when (process-live-p llms-speaker-vocab-overlay-process)
    (delete-process llms-speaker-vocab-overlay-process))
  (setq llms-speaker-vocab-overlay-process nil)
  (when (frame-live-p llms-speaker-vocab-overlay-frame)
    (delete-frame llms-speaker-vocab-overlay-frame))
  (setq llms-speaker-vocab-overlay-frame nil))

(provide 'llms-langs)
;;; llms-langs.el ends here
