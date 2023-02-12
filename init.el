;;; init.el --- narendraj9's Emacs configuration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2016, 2021  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  My Emacs configuration.

;;; Code:

;;; The Epoch
(defconst emacs-start-time (current-time))

;;; Avoid garbage collection during Emacs startup. Garbage collection when
;;; Emacs loses focus.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 10 1024 1024))))

;; Try to make `.emacs.d` relocatable
(setq user-emacs-directory
      (file-name-directory (or load-file-name
                               "~/.emacs.d/init.el")))

;; Remember the last 10000 keystrokes
(lossage-size 10000)

;;; GnuTLS
;; ──────────────────────────────────────────────────────────────────
(setq gnutls-verify-error t)

;;; PACKAGE ARCHIVES
;;  ─────────────────────────────────────────────────────────────────

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))
(setq package-native-compile t)

;;; USE-PACKAGE
;; ──────────────────────────────────────────────────────────────────
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish :ensure t :demand t)
(use-package bind-key :ensure t)

(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (add-to-list 'use-package-keywords :git nil)

  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
Currently, the value for this keyword is just ignored.  In the
future, I might want to add its value to name-symbol's
documentation string.

Argument NAME-SYMBOL is the first argument to `use-package' in a declaration.
Argument KEYWORD here is simply :doc.
Argument DOCSTRING is the value supplied for :doc keyword.
Argument REST is the list of rest of the  keywords.
Argument STATE is maintained by `use-package' as it processes symbols."
    (let ((body (use-package-process-keywords name-symbol rest state)))
      body)))

(defun use-package-normalize/:git (name keyword args)
  ;; No error handling yet.
  (car args))

(defun use-package-handler/:git (name-symbol _keyword git-url rest state)
  (let* ((body (use-package-process-keywords name-symbol rest state))
         (package-directory (file-name-base git-url))
         (package-path (expand-file-name (format "packages/git/%s" package-directory)
                                         user-emacs-directory)))
    (use-package-concat
     `((eval-and-compile (add-to-list 'load-path ,package-path))
       (when (or (not (file-exists-p ,package-path))
                 (directory-empty-p ,package-path))
         (message "Cloning %s" ,git-url)
         (shell-command ,(format "git clone %s %s"
                                 (shell-quote-argument git-url)
                                 (shell-quote-argument package-path)))))
     body)))

;;; Emacs Lisp Compilation

(setq load-prefer-newer t)

(use-package comp
  :if (fboundp 'native-compile)
  :custom (comp-async-report-warnings-errors nil))


;; LIBRARIES
;;  ─────────────────────────────────────────────────────────────────

(use-package s       :demand t :ensure t)
(use-package f       :demand t :ensure t)
(use-package dash    :demand t :ensure t)
(use-package request :defer t :ensure t)

(use-package ednc  :defer t :ensure t)
(use-package alert :defer t :ensure t)

(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))

(use-package pcre2el
  :doc "I intend to read the code carefully someday."
  :after re-builder
  :bind (:map ctl-quote-map
              ("c /" . pcre->elisp))
  :preface
  (defun pcre->elisp (beg end)
    "Replace PCRE regex in region (BEG END) with its elisp equivalent."
    (interactive "r")
    (let ((pcre-regex (buffer-substring-no-properties beg end)))
      (delete-region beg end)
      (insert (pcre-to-elisp pcre-regex)))))

;;
;;  ─────────────────────────────────────────────────────────────────
(use-package defs
  :doc "Var and function definitions that must be loaded before
  everything else."
  :demand t
  :load-path "lib/"
  :pin manual
  :init
  ;; Some ergonomic alternatives
  (define-key input-decode-map
              ;; Default: \C-i => TAB
              "\C-i" "\C-c")

  (define-key input-decode-map
              ;; Default: \C-[ => ESC
              "\C-[" "\C-x")

  (define-key input-decode-map
              ;; Default: \C-m => RET
              "\C-m" [C-m])

  (bind-keys* :prefix [C-m]   :prefix-map ctl-m-map)
  (bind-keys* :prefix "C-'"   :prefix-map ctl-quote-map)
  (bind-keys* :prefix "C-."   :prefix-map ctl-period-map)
  (bind-keys* :prefix "C-;"   :prefix-map ctl-semicolon-map)
  (bind-keys* :prefix "C-h x" :prefix-map ctl-h-x-map)

  :bind ( ("C-c m" . switch-to-minibuffer)
          ("C-c 0" . quick-switch-themes)
          ("C-c b" . switch-to-buffer-with-mode)
          ("<print>" . snap-it)

          :map ctl-m-map
          ("t" . switch-to-scratch-new-tab)
          ("k" . swap-ctrl-right-win)
          ("f" . pretty-format-temporarily)

          :map ctl-quote-map
          ("w s" . websearch-it)
          ("l l" . search-linguee)
          ("l t" . translate-with-linguee)
          ("d ." . insert-date-time-at-point)
          ("c e" . vicarie/eval-print-last-sexp)
          ("c =" . vicarie/eval-replace-last-sexp)
          ("c r" . rename-file-and-buffer)
          ;; ("C-a" . emacspeak-wizards-execute-asynchronously)
          ("M-x" . async-M-x)

          :map ctl-period-map
          ("k" . compile)
          ("K" . recompile)
          ("$" . selective-display-beyond-col)
          ("u" . underline-text)
          ("d" . duplicate-current-line)
          ("s" . surround-symbol-with) ))


(use-package custom-registers
  :load-path "etc"
  :bind ( :map ctl-x-r-map ("U" . custom-registers-url-to-register) ))

(use-package no-littering
  :ensure t
  :init
  ;; Make var/ default. Paths are overridden if the files are
  ;; important.
  ;; Keep all misc state in $(user-emacs-direcotry)/var/
  (make-directory (expand-file-name "var/" user-emacs-directory) t)
  ;; These variables need to be set before `no-littering' is loaded.
  (setd no-littering-etc-directory "etc/"
        no-littering-var-directory "var/"))

(use-package "startup"
  :init
  (setq inhibit-splash-screen t)

  ;; ----------------------------------------
  ;; Thanks to
  ;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; So that man pages are rendered properly irrespective of LC_* variable
  ;; values.
  (setq locale-coding-system 'utf-8)

  (setq buffer-file-coding-system 'utf-8)

  (setq large-file-warning-threshold
        (* 30 1024 1024))

  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  ;; ----------------------------------------

  (setd tutorial-directory "var/tutorial/")

  ;; Buffer contents auto-saved post initial file contents
  (setq auto-save-list-file-prefix
        (expand-file-name "var/autosaves/.saves-" user-emacs-directory))

  (setq delete-by-moving-to-trash t)

  ;; Backups of file before current changes
  (setq backup-directory-alist
        `((".*" . ,(expand-file-name "var/backups/"
                                     user-emacs-directory)))
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 5
        kept-old-versions 5)

  (setq confirm-kill-emacs
        (lambda (prompt)
          (let* ((random-number (random 100)))
            (equal (number-to-string random-number)
                   (read-string (format "%s [Type %d to confirm] "
                                        prompt
                                        random-number))))))
  (setq initial-scratch-message ""
        initial-major-mode
        (lambda ()
          (lisp-interaction-mode)
          (setq header-line-format
                '(:eval
                  (format "Emacs Uptime: %-20s | Sys Time: %-20s | System Load: %-20s"
                          (emacs-uptime)
                          (format-seconds "%Y, %D, %2H, %2M, %z%S"
                                          (time-convert (get-internal-run-time)
                                                        'integer))
                          (load-average 'use-float)))))))

(use-package custom
  :doc "Custom configuration and personal information."
  :init
  (defvar secrets-file (expand-file-name "secrets.el"
                                         emacs-assets-directory))
  (setd custom-file "custom.el")

  ;; My custom file usually doesn't contain settings that would be visible to
  ;; me. I have some variables setup though that help me reduce disk reads
  ;; while setting up fonts.
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; The default font should be properly set by now, make sure that
  ;; each newly created frame uses the same font.
  ;; (add-hook 'after-init-hook
  ;;           (lambda ()
  ;;             (add-to-list 'default-frame-alist
  ;;                          `(font . ,(font-xlfd-name (face-attribute 'default :font))))))

  ;; Load secrets if available.
  (when (file-exists-p secrets-file)
    (load secrets-file))

  ;; Misc
  (setq-default tab-width 4)
  (setq-default fill-column 80)

  ;; Disable bell
  (setq ring-bell-function (lambda ()))

  ;; Enable some disabled commands
  (mapc (lambda (c) (put c 'disabled nil))
        '(narrow-to-region
          upcase-region
          downcase-region
          capitalize-region
          erase-buffer
          set-goal-column
          list-timers
          list-threads))

  ;; Dialog boxes don't work with Xmonad.
  (setq use-dialog-box nil)

  ;; --
  ;; (defalias 'yes-or-no-p 'y-or-n-p)
  ;; (setq use-short-answers t)
  )

(use-package tool-bar   :config (tool-bar-mode -1))
(use-package scroll-bar :config (scroll-bar-mode -1))
(use-package menu-bar
  :doc "The menu bar is useful for discovering features that
   exist in some modes, e.g Gnus, SQLi."
  :bind (:map ctl-x-map ("w m" . menu-bar-open))
  :config (menu-bar-mode -1))

(use-package appearance
  :doc "`use-package' doesn't throw an error for non-existent packages"
  :load-path "themes/"
  :init
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes/"
                                 user-emacs-directory))
  :config
  (appearance-init))

(use-package mode-line-config
  :bind ( :map ctl-m-map
          ("<C-m>" . mode-line-config-flash-cleaner-line) )
  :demand t
  :load-path "etc/"
  :config
  (set-face-attribute 'mode-line-active nil :inherit 'mode-line)
  (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)

  :preface
  (defun mode-line-config-flash-cleaner-line ()
    (interactive)
    (let ((mode-line-modes (list))
          (mode-line-position nil)
          (mode-line-config-hide-vc t)
          (mode-line-buffer-identification nil)
          (header-line-format (string-trim (pomodoro-status))))
      (force-mode-line-update)
      (sit-for 2)
      (force-mode-line-update))))

(use-package fringe
  :init
  (defvar default-fringe-style (cons (floor (* 1.5 (frame-char-width)))
                                     (frame-char-width))
    "This needs to be defined because it's used elsewhere to
    reset fringes back to the default after highlighting
    something that requires immediate attention.")

  (defun fringe-set-louder ()
    (fringe-mode 20)
    (set-face-attribute 'fringe nil :inverse-video t))

  (defun fringe-restore-default ()
    (fringe-mode default-fringe-style)
    (set-face-attribute 'fringe nil :inverse-video nil))

  (fringe-mode default-fringe-style))

(use-package quoted-scratch
  :load-path "packages/rest/quoted-scratch"
  :disabled t
  :init
  (add-hook 'after-init-hook #'quoted-scratch-refresh-quote-when-idle)
  :config
  (setq quoted-scratch-show-auroville-quality nil))


;;; Battery and Time display in the mode line
;;; ----------------------------------------------------------------------------

(use-package time
  :demand t
  :preface
  :bind ( :map ctl-quote-map ("c t" . world-clock*) )
  :init
  (display-time-mode +1)

  :config
  (setq world-clock-timer-enable nil
        world-clock-time-format "\n──────────────\n\t%A %d %B %R %Z\n")

  (setq display-time-string-forms
        '((propertize
           (format " %s/%s/%s %s %s:%s "
                   (substring year -2) (string-trim month) (string-trim day) dayname 24-hours minutes)
           'face 'bold))
        display-time-default-load-average 1     ; 5 minute load avg
        display-time-load-average-threshold 0.8 ; >80%
        display-time-mail-string "")

  :preface
  (defun world-clock* (&optional arg)
    (interactive "P")
    (if arg
        (with-selected-date-time (world-clock))
      (world-clock))))

(use-package battery
  :demand t
  :config
  (setq battery-mode-line-format
        (propertize "%b%p%% " 'face 'mode-line-battery-face))
  (display-battery-mode +1)

  (run-with-timer 60 60 #'battery-protection-notifications)

  :preface
  (defun battery-protection-pause ()
    (interactive)
    (put 'battery-protection-notifications
         :enabler-timer
         (run-with-timer 7200 nil
                         (lambda ()
                           (put 'battery-protection-notifications :paused nil)))))


  (defun battery-protection-resume ()
    (interactive)
    (when-let ((timer (get 'battery-protection-notifications :enabler-timer)))
      (cancel-timer timer)
      (put 'battery-protection-notifications :enabler-timer nil)))


  (defun battery-protection-notifications ()
    (let* ((status (funcall battery-status-function))
           (lowest-percentage 20)
           (highest-percentage 101)
           (percentage (string-to-number (alist-get ?p status)))
           (charging-status (alist-get ?b status)))
      (when (not (get 'battery-protection-notifications :enabler-timer))
        (when (and (string= charging-status "+")
                   (< highest-percentage percentage))
          (notifications-notify :title "Battery Protection"
                                :body "You need to remove the charger."))
        (when (and (not (string= charging-status "+"))
                   (< percentage lowest-percentage))
          (notifications-notify :title "Battery Protection"
                                :body "Start charging your battery."))))))

(use-package ibuffer
  :bind (:map ctl-x-map ("C-b" . ibuffer-other-window) )
  :config
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 28 28 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 -1 :left) " "
                filename-and-process)
          (mark " "
                (name 36 36 :left) " | "
                (filename-and-process 10 -1 :right)))))

(use-package ibuf-ext
  :config
  (setq ibuffer-saved-filter-groups
        '(("Categorized"
           ("Org Mode" (mode . org-mode))
           ("IRC" (mode . erc-mode))
           ("*Auxiliary*" (name . "\\*.*\\*"))))))


;;; Utilities
;; ──────────────────────────────────────────────────────────────────

(use-package wtf
  :load-path "packages/lisp/"
  :commands wtf-is)

(use-package tiny
  :doc
  "Provides `tiny-expand' to quickly generate linear sequences.
   Syntax: m[<range start:=0>][<separator:= >]<range end>[Lisp expr]|[format expr]
   Example: m1\n10(list x (sqrt x))|Square root of %d is %.2f"
  :ensure t
  :bind (("C-c C-;" . tiny-expand)))

(use-package copy-as-format
  :ensure t
  :preface
  (advice-add 'copy-as-format
              :around (lambda (orig-fn &rest _args)
                        (let ((current-prefix-arg '(4)))
                          (funcall orig-fn))))
  :commands copy-as-format)

(use-package restclient
  :doc "Restclient comes in handy a lot of times."
  :defer t
  :ensure t
  :commands restclient
  :mode ("\\.restclient\\'" . restclient-mode)
  :config
  (add-hook 'restclient-response-received-hook (lambda () (message "Done!"))))

(use-package prodigy
  :defer t
  :bind ( :map ctl-quote-map ("s p" . prodigy) )
  :ensure t
  :config
  (load-file (expand-file-name "etc/prodigy-service-defs.el"
                               user-emacs-directory))

  ;; Load definitions if they are kept in assets directory too
  (let ((extra-service-defs-path (expand-file-name "prodigy-service-defs.el"
                                                   emacs-assets-directory)))
    (when (file-exists-p extra-service-defs-path)
      (load-file extra-service-defs-path))))

;;; Thanks to https://github.com/Wilfred
;;; ====================================

(use-package ag :ensure t :bind ("M-s M-a" . ag))

;; KEY BINDINGS
;; ──────────────────────────────────────────────────────────────────

(ffap-bindings)
(bind-keys :map ctl-period-map
           ("C-o" . goto-address-at-point)
           ("C-f" . ffap))

;; ──────────────────────────────────────────────────────────────────

(use-package tab-bar
  :bind (:map tab-prefix-map ("s" . tab-bar-new-scratch*))
  :doc
  "This built-in package provides a way to keep a set of window
   configurations around that can be switched to easily."
  :config
  (tab-bar-history-mode +1)
  (setq tab-bar-show nil
        tab-bar-tab-name-function #'tab-bar-tab-name-all)
  :preface
  (defun tab-bar-new-scratch* ()
    (interactive)
    (tab-new)
    (switch-to-buffer "*scratch*")))

(use-package calendar
  :defer t
  :bind (:map ctl-quote-map
              ("c c" . calendar))
  :init
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)

  (setq diary-file (expand-file-name "diary" emacs-assets-directory)
        ;; Weeks start on Monday.
        calendar-week-start-day 1
        ;; Month header show the month of the year.
        calendar-month-header
        '(propertize (format "%s %d/%d" (calendar-month-name month) month year)
                     'font-lock-face 'calendar-month-header)))

(use-package holidays
  :defer t
  :config
  (use-package german-holidays
    :ensure t
    :config
    (->> holiday-german-holidays
         (append calendar-holidays)
         -distinct
         (setq calendar-holidays))))

(use-package appt
  :defer t
  :preface
  (defun show-appt-notifications (disp-fn &rest args)
    "Show notifications for an appointment using default alert style."
    (alert (format "In %s minutes: %s" (car args) (caddr args)) :title "Reminder")
    (apply disp-fn args))

  :init
  (eval-after-load "org-mode"
    '(appt-activate +1))
  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              (quietly (org-agenda-to-appt))))

  :config
  (setq appt-audible t
        appt-display-duration 15)

  (advice-add 'appt-disp-window :around #'show-appt-notifications))

(use-package eww
  :custom ((eww-auto-rename-buffer 'title)
           (eww-browse-url-new-window-is-tab nil))
  :config
  (add-hook 'eww-after-render-hook #'eww-readable))

(use-package browse-url
  :defer t
  :config
  (setq browse-url-new-window-flag nil)

  (cond
   ((executable-find "firefox")
    (setq browse-url-browser-function 'browse-url-firefox))
   ((executable-find "chromium")
    (setq browse-url-browser-function 'browse-url-chromium))
   ((executable-find "google-chrome")
    (setq browse-url-browser-function 'browse-url-chrome))))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package rainbow-mode :ensure t :defer t)

(use-package uniquify
  :doc "Unique buffer names"
  :diminish t
  :init
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator " • "))

(use-package ialign
  :doc "Very useful to get quick feedback for alignment with
  `align.el'."
  :ensure t
  :bind (:map ctl-period-map
              ("C-a" . ialign)))

(use-package symbol-overlay
  :ensure t
  :bind ( :map global-map
          ("M-n" . symbol-overlay-put*)
          ("M-p" . symbol-overlay-put*)

          :map symbol-overlay-map
          ("M-n" . symbol-overlay-jump-next)
          ("M-p" . symbol-overlay-jump-prev) )

  :init
  (advice-add 'symbol-overlay-jump-call :after #'flash-current-symbol)

  :preface
  (defvar symbol-overlay-remove-all-timer nil)
  (defun symbol-overlay-remove-all-timer (buffer)
    (when (timerp symbol-overlay-remove-all-timer)
      (cancel-timer symbol-overlay-remove-all-timer))
    (setq symbol-overlay-remove-all-timer
          (run-with-timer 2
                          nil
                          (lambda ()
                            (with-current-buffer buffer
                              (call-interactively #'symbol-overlay-remove-all))))))

  (defun symbol-overlay-put* ()
    "Jump to the next or previous occurrence of symbol at point
     after doing `symbol-overlay-put'."
    (interactive)
    (symbol-overlay-put)
    (when-let ((command (lookup-key symbol-overlay-map
                                    (this-command-keys))))
      (call-interactively command)))

  (defun flash-current-symbol (&rest _)
    "Pulse highlight symbol at point."
    (let ((bounds (bounds-of-thing-at-point 'symbol))
          (pulse-delay 0.02))
      (symbol-overlay-remove-all-timer (current-buffer))
      (pulse-momentary-highlight-region (car bounds)
                                        (cdr bounds)))))

(use-package crux
  :ensure t
  :bind (("C-<backspace>" . crux-kill-line-backwards)
         ("S-<return>"    . crux-switch-to-previous-buffer))
  :hook (after-init . crux-reopen-as-root-mode))

;;; TEXT-EDITING, FOLDING and NAVIGATION
;; ─────────────────────────────────────────────────────────────────

(use-package elec-pair :init (electric-pair-mode +1))
(use-package wgrep :ensure t)

(use-package outline-minor-mode
  :defer t
  :custom ((outline-blank-line t))
  :init
  (add-hook 'outline-minor-mode-hook
            (lambda ()
              (diminish 'outline-minor-mode)))
  ;; This minor mode uses selective display to hide text so it displays three
  ;; dots (ellipsis) like `selective-display'
  ;; https://www.emacswiki.org/emacs/OutlineMode
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector " ... ")))

(use-package hideshow
  :defer t
  :diminish hs-minor-mode
  :bind ( :map hs-minor-mode-map
          ([backtab] . hs-toggle-hiding)
          ("C-c @ a"  . my-toggle-hideshow-all)
          ("C-c @ s"  . hs-show-block)
          ("C-c @ h"  . hs-hide-block))
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode)

  :config
  (setq hs-set-up-overlay #'display-code-line-counts)

  :preface
  (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
  (defun my-toggle-hideshow-all ()
    "Toggle hideshow all."
    (interactive)
    (setq my-hs-hide (not my-hs-hide))
    (if my-hs-hide
        (hs-hide-all)
      (hs-show-all)))

  (defun display-code-line-counts (ov)
    (when-let ((line-count (and (eq 'code (overlay-get ov 'hs))
                                (count-lines (overlay-start ov)
                                             (overlay-end ov)))))
      (overlay-put ov
                   'display
                   (propertize (format "  +%s " line-count)
                               'face 'highlight))
      (overlay-put ov
                   'before-string
                   (propertize " "
                               'display
                               '(left-fringe right-arrow highlight))))))

(use-package wrap-region
  :doc
  "Wrap region with custom chars."
  :ensure t
  :hook (after-init . wrap-region-mode)
  :diminish
  :config
  (wrap-region-add-wrappers
   '(("=" "=" nil (org-mode))
     ("~" "~" nil (org-mode))))
  (wrap-region-global-mode +1)
  (wrap-region-remove-wrapper "<" 'org-mode))

(use-package region-bindings-mode
  :diminish region-bindings-mode
  :ensure t
  :config
  (region-bindings-mode-enable))

(use-package repeat
  :init
  ;; If a symbol property named `repeat-map' exists for a command and it's a
  ;; keymap, it's activate as a transient-map after command is executed.
  (let ((inhibit-message t))
    (repeat-mode +1))

  :config
  (setq repeat-exit-timeout 30)

  :preface
  (defmacro define-repeat-map (&rest bindings)
    (declare (indent 0))
    (let ((m (make-sparse-keymap))
          (name (gensym "repeat-map--")))
      (dolist (binding bindings)
        (define-key m (kbd (car binding)) (cdr binding))
        (put (cdr binding) 'repeat-map name))
      (list 'defvar name (list 'quote m)))))

(use-package select :init (setq select-enable-clipboard t))
(use-package simple
  :doc "The great simple.el"
  :demand t
  :diminish auto-fill-function
  :bind (("M-q"   . fill-or-unfill)
         ("M-["   . backward-delete-dwim)
         ("S-SPC" . upcase-last-symbol-and-space)

         :map ctl-period-map
         ("C-u" . delete-indentation)

         :map ctl-quote-map
         (":"   . set-variable)
         ("s >" . shell-command-on-region)
         ("s |" . shell-command-on-region)
         ("s s" . shell-command)
         ("s !" . shell-command)
         ("s a" . async-shell-command)
         ("s &" . async-shell-command)
         ("s ." . shell-command-from-region) )
  :hook ((prog-mode hledger-mode)  . comment-auto-fill)
  :preface
  (defun exchange-point-and-mark* (arg)
    (interactive "P")
    (exchange-point-and-mark (not arg)))

  (defun shell-command-from-region (beg end)
    "Runs selected region as a shell command."
    (interactive "r")
    (shell-command (buffer-substring-no-properties beg end)))

  :config
  (define-repeat-map ("C-u" . delete-indentation))

  ;; Multiple-cursors changes transient-mark-mode to (only only .. t),
  ;; if shift-select-mode is enabled.
  (setq shift-select-mode nil)

  ;; Make text copied/cut from outside Emacs part of Emacs kill-ring on first
  ;; kill inside Emacs.
  (setq kill-ring-max 1000
        save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t)

  ;; Temporary
  ;; ----------------------------------------------------------------------------
  (defvar kill-ring-modifiers-freq-hashmap
    (make-hash-table))

  (add-variable-watcher 'kill-ring
                        (lambda (&rest _args)
                          (when this-command
                            (puthash this-command
                                     (1+ (gethash this-command kill-ring-modifiers-freq-hashmap 0))
                                     kill-ring-modifiers-freq-hashmap))))

  (defun summarize-kill-ring-modifiers ()
    (interactive)
    (let ((items ()))
      (maphash (lambda (k v) (push (cons k v) items)) kill-ring-modifiers-freq-hashmap)
      (message-or-box (mapconcat (lambda (command-freq)
                                   (format "%s: %s" (car command-freq) (cdr command-freq)))
                                 (sort items (lambda (a b) (> (cdr a) (cdr b))))
                                 "\n"))))
  ;; ----------------------------------------------------------------------------

  (setq suggest-key-bindings t)

  (setq async-shell-command-buffer 'new-buffer
        set-mark-command-repeat-pop t
        column-number-mode t
        size-indication-mode t)

  (diminish 'visual-line-mode)

  ;; This BROKE multiple cursors! Multiple-cursor uses
  ;; `exchange-point-and-mark' and creates overlays for active region around
  ;; fake cursors. This advice was deactivating region after multiple-cursors
  ;; called `exchange-point-and-mark' for the first time which resulted in
  ;; strange behaviour when used after activating region.
  ;;
  ;; (add-function :after
  ;;               (symbol-function #'exchange-point-and-mark)
  ;;               (lambda (&rest _args) (deactivate-mark nil)))
  (define-key global-map
              [remap exchange-point-and-mark]
              #'exchange-point-and-mark*)

  (add-hook 'activate-mark-hook (lambda () (setq cursor-type (cons 'bar 4))))
  (add-hook 'deactivate-mark-hook (lambda () (setq cursor-type t))))

(use-package apropos :config (setq apropos-do-all t))
(use-package shortdoc :bind ( :map help-map ("g" . shortdoc-display-group) ))

(use-package misc
  :doc "Where simple ends, maybe misc.el begins"
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . copy-from-above-command)))

(use-package savehist
  :demand t
  :config
  (setq savehist-file (expand-file-name "var/savehist.el"
                                        user-emacs-directory))
  (setq savehist-save-minibuffer-history t
        savehist-additional-variables
        '( kill-ring command-history limit-usage kill-ring-modifiers-freq-hashmap ))
  (savehist-mode +1)
  ;; https://emacs.stackexchange.com/a/4191/14967
  ;; Prevent `kill-ring' values from causing very long pauses while
  ;; shutting down Emacs by removing text properties from `kill-ring'
  ;; entries.
  (add-hook 'kill-emacs-hook
            (lambda ()
              (setq kill-ring (mapcar 'substring-no-properties kill-ring)))))

(use-package vcursor
  :bind ( :map ctl-m-map
          ("v" . vcursor-use-vcursor-map) )
  :init
  (defvar vcursor-use-vcursor-map--wc)
  (add-hook 'vcursor-use-vcursor-map-hook
            (lambda ()
              (if vcursor-use-vcursor-map
                  (progn (fringe-set-louder)
                         (setq vcursor-use-vcursor-map--wc
                               (current-window-configuration))
                         (save-excursion
                           (previous-line)
                           (vcursor-move (point))))
                (fringe-restore-default)
                (set-window-configuration vcursor-use-vcursor-map--wc)))))

(use-package multiple-cursors
  :doc "A minor mode for editing with multiple cursors."
  :ensure t
  :init
  (setd mc/list-file "var/mc-lists.el")
  (add-hook 'multiple-cursors-mode-hook
            (lambda ()
              (if multiple-cursors-mode
                  (fringe-set-louder)
                (fringe-restore-default))))

  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c >" . mc/mark-all-like-this)

   :map mc/keymap
   ("M-y" . yank-pop)

   :map region-bindings-mode-map
   ("C-x $" . mc-hide-unmatched-lines-mode)
   ("C-x !" . mc/mark-all-like-this)

   :map ctl-quote-map
   ("m" . mc/mark-more-like-this-extended)
   ("M" . mc/edit-lines)))

(use-package narrow-indirect
  :doc
  "Edit region in an indirect buffer."
  :load-path "packages/lisp/"
  :bind (:map goto-map   ("i" . ni-narrow-to-region-indirect-other-window))
  :config
  (setq ni-narrowed-buf-name-max 40))

(use-package goto-last-change
  :ensure t
  :bind ( :map ctl-x-map ("C-SPC" . goto-last-change) )
  :init
  (define-repeat-map ("C-SPC" .  goto-last-change)))

(use-package goto-line-preview
  :ensure t
  :disabled t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package follow-mode
  :bind ("C-c |" . follow-delete-other-windows-and-split))

(use-package footnote
  :doc
  "For footnotes. On a side note [maybe footnote], I notices
  that with a keymap bound to a key sequence, it doesn't work the
  first time you press the key sequence. @TODO"
  :config
  :bind (:map ctl-quote-map
              ("C-f a" . footnote-add-footnote)
              ("C-f b" . footnote-back-to-message)
              ("C-f d" . footnote-delete-footnote)
              ("C-f g" . footnote-goto-footnote)))

(use-package csv-mode      :defer t :ensure t)
(use-package markdown-mode :defer t :ensure t)
(use-package cdlatex :ensure t :hook (Latex-Mode-Hook . turn-on-cdlatex))
(use-package auctex
  :pin gnu
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :ensure t
  :init
  (use-package company-math
    :ensure t
    :defer t
    :init
    (add-hook 'TeX-mode-hook
              (lambda ()
                (setq-local company-backends
                            (append '((company-math-symbols-latex company-latex-commands))
                                    company-backends)))))

  (use-package tex
    :defer t
    :init
    (setq-default TeX-master nil
                  TeX-auto-save t
                  TeX-parse-self t
                  TeX-command-Show "LaTeX")
    ;; Credits:
    ;; https://emacs.stackexchange.com/questions/19472/how-to-let-auctex-open-pdf-with-pdf-tools
    ;; Use pdf-tools to open PDF files
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
          TeX-source-correlate-start-server t)
    ;; Update PDF buffers after successful LaTeX runs
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)))

;; ─────────────────────────────────────────────────────────────────

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :hook (after-init . volatile-highlights-mode))

(use-package info
  :config
  ;; Use a variable-pitched font face if using a light theme.
  (add-hook 'info-mode-hook (lambda ()
                              (when (eq (frame-parameter nil 'background-mode)
                                        'light)
                                (variable-pitch-mode +1)))))

(use-package which-key
  :doc "Get quick emacs key binding suggestions"
  :ensure t
  :demand 2
  :diminish which-key-mode
  :config
  (setq which-key-max-description-length nil)
  (which-key-mode +1))

(use-package project
  :bind ( :map project-prefix-map
          ("o" . git-ls-files-find-file)
          ("m" . magit-status))
  :init
  (setq project-list-file
        (expand-file-name "var/project-list" user-emacs-directory))

  :config
  (autoload #'magit-status "magit" nil t)

  :preface
  (defun git-ls-files-find-file ()
    (interactive)
    (->> (shell-command-to-string "git ls-files")
         (s-lines)
         (seq-filter #'s-present?)
         (completing-read "Find File: ")
         (find-file))))

(use-package project-x
  :load-path "etc/"
  :after project
  :custom (project-x-local-identifier ".project-x")
  :config
  (add-hook 'project-find-functions 'project-x-try-local -100))

;;; SESSIONS and BOOKMARKS
;; ──────────────────────────────────────────────────────────────────

(use-package bookmark
  :bind ( :map ctl-x-r-map ("u" . bookmark-set-url*) )
  :defer 5
  :config
  (setq bookmark-save-flag 1
        bookmark-default-file (expand-file-name "~/miscellany/assets/bookmarks.el")
        bookmark-set-fringe-mark nil)
  :preface
  (defun bookmark-url-handler (url-bookmark)
    "Handle parameterized URL links.

     Parameters are placed inside the URL string as
     {{Prompt for the param}}."
    (let ((url (bookmark-prop-get url-bookmark 'url))
          (start-index 0))
      (while (and (< start-index (length url))
                  (string-match "{{\\([^}]+\\)}}" url start-index))
        (setq start-index (match-end 0)
              url (string-replace (match-string 0 url)
                                  (if (region-active-p)
                                      (buffer-substring (region-beginning) (region-end))
                                    (read-string (concat (match-string 1 url) ": ")))
                                  url)))
      (browse-url url)))

  (defun bookmark-set-url* (url description)
    (interactive "sBookmark URL: \nsDescription: ")
    (push (cons description
                `((handler . bookmark-url-handler)
                  (filename . ,url)     ; for display
                  (url . ,url)))
          bookmark-alist)))

(use-package saveplace
  :init
  (save-place-mode +1)
  :config
  (setq save-place-file
        (locate-user-emacs-file "var/saved-places")))

;;; Buffers, Windows and Frame
;; ――――――――――――――――――――――――――――――――――――――――

(use-package pixel-scroll
  :disabled t
  :init
  (pixel-scroll-mode +1)
  (pixel-scroll-precision-mode +1))

(use-package window
  ;; There is no mnemonic here, it's just convenient to type.
  :bind (:map ctl-period-map ("e" . fit-window-to-buffer*) )
  :init
  ;; Adds an entry to `emulation-mode-map-alists' which take precedence over all
  ;; active minor-mode keymaps and the active major-mode keymap in a buffer.
  (bind-key* [C-return] #'other-window)

  (setq fit-window-to-buffer-horizontally t
        window-resize-pixelwise t)

  (dolist (display-spec
           '( ("\\`\\*e?shell" display-buffer-at-bottom)
              ("\\*Calendar\\*" display-buffer-at-bottom)
              ("\\*Async Shell Command\\*" display-buffer-no-window)
              ("\\`\\*Flycheck errors\\*\\'" (display-buffer-reuse-window
                                              display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 10))) )
    (add-to-list 'display-buffer-alist display-spec))

  (dolist (buffer-regex '("\\`\\*eldoc\\*\\'" "\\`magit: .*\\'" "\\`\\*cider-doc\\*\\'"))
    (add-to-list 'display-buffer-alist
                 `(,buffer-regex display-buffer-in-direction
                                 (window . main)
                                 (direction . right)
                                 (window-width . 0.5))))

  :preface
  (defun fit-window-to-buffer* (arg)
    (interactive "P")
    (fit-window-to-buffer (if arg (other-window 1) (selected-window)))))


(use-package winner
  :bind ( :map ctl-m-map ("<" . winner-undo ) )
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode +1)
  :config
  (define-key winner-repeat-map ">" #'winner-redo)
  (define-key winner-repeat-map "<" #'winner-undo))

(use-package exwm
  :disabled t
  :ensure t
  :config
  (use-package exwm-configuration
    :disabled t
    :load-path "etc/"
    :config
    (exwm-config-default)))


;;; NAVIGATION
;; ――――――――――――――――――――――――――――――――――――――――
(use-package mwim
  :ensure t
  :bind ( :map goto-map ("TAB" . mwim) )
  :init
  (define-repeat-map ("TAB" . mwim)))

(use-package isearch
  :doc "Search for the string in the active region, if there is any."
  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window)
         :map isearch-mode-map
         ("C-S-W"     . isearch-yank-symbol)
         ("<tab>"     . isearch-repeat-forward)
         ("<backtab>" . isearch-repeat-backward))

  :config
  (advice-add 'isearch-mode
              :after (lambda (&rest _rest)
                       (when (region-active-p)
                         (isearch-yank-selection))))

  (setq isearch-lazy-count t
        lazy-highlight-initial-delay 1.0
        lazy-count-prefix-format "(%s/%s) ")

  :preface
  (defun isearch-yank-symbol ()
    "Yank symbol from buffer into `minibuffer'."
    (interactive)
    (when-let ((sym-name (thing-at-point 'symbol)))
      (isearch-yank-string sym-name)))

  (defun isearch-exit-other-end ()
    "Exit isearch, at the opposite end of the string."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))

  (defun isearch-yank-selection ()
    "Put selection from buffer into search string."
    (interactive)
    (when (region-active-p)
      (deactivate-mark))
    (isearch-yank-internal 'mark))

  (defun isearch-backward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-backward))

  (defun isearch-forward-other-window ()
    (interactive)
    (split-window-vertically)
    (call-interactively 'isearch-forward)))

(use-package avy
  :ensure t
  :bind (("M-g w" . avy-goto-word-1)
         :map ctl-period-map
         ("C-." . avy-goto-char-timer))
  :config
  (setq avy-style 'words)
  (avy-setup-default))

(use-package windmove
  :bind (("C-M-h" . windmove-left)
         ("C-M-j" . windmove-down)
         ("C-M-k" . windmove-up)
         ("C-M-l" . windmove-right))
  :config
  (setq windmove-wrap-around t))

(use-package "window"
  :preface

  :config
  ;; I think I will mostly want the pointer to go to the end with M-r
  ;; And then I would do a M-l to re-center it. Since both of them use
  ;; `recenter-positions'. I am using advices.
  (setq recenter-positions '(bottom top middle))
  (advice-add 'recenter-top-bottom
              :around (lambda (f &rest args)
                        (let ((recenter-positions '(middle top bottom)))
                          (apply f args))))

  (def-echoing next-buffer)
  (def-echoing previous-buffer)

  (setq fit-window-to-buffer-horizontally t)

  (advice-add #'split-window-below :filter-return #'select-window)
  (advice-add #'split-window-right :filter-return #'select-window))

(use-package ace-window
  :doc
  "This should come after `window's use-package
  declaration. Otherwise, `window' would overwrite the binding for \\[ace-window]]."
  :ensure t
  :doc "Use `ace-window' instead of `other-window'."
  :custom (aw-keys (list ?s ?d ?f ?j ?k ?l))
  :bind ("C-x o" . ace-window))

;; ――――――――――――――――――――――――――――――――――――――――

(use-package recentf
  :custom ( (recentf-auto-cleanup 'never)
            (recentf-keep '(file-remote-p file-readable-p))
            (recentf-exclude '("\.gpg$"))
            (recentf-max-saved-items 1000)
            (recentf-save-file (expand-file-name "var/recentf"
                                                 user-emacs-directory)) )
  :init
  (recentf-mode +1))


;; ――――――――――――――――――――――――――――――――――――――――

(use-package mouse
  :init
  (context-menu-mode +1))

(use-package dictionary
  :doc "`dictionary' is a built-in package. It uses
  `dictionary-connection' that provides nice utility functions for
  talking to any TCP server."
  :commands dictionary-word-at-mouse-event
  :bind ( :map global-map
          ([double-down-mouse-1] . dictionary--word-def*)

          :map ctl-quote-map
          ("l d" . dictionary--word-def)
          ("l D" . dictionary-search ) )

  :config
  (setq dictionary-server "dict.org")
  (add-to-list 'context-menu-functions
               'context-menu-dictionary)

  :preface
  (defun dictionary--word-def* (event)
    (interactive "e")
    (message "Called with: %s" event)
    (dictionary--word-def (dictionary-word-at-mouse-event event)))

  (defun dictionary--word-def (word)
    (interactive (list (read-string "Word: " (word-at-point))))
    (if-let ((meaning (and word (dictionary-definition word))))
        ;; I need this delay so that the tooltip isn't immediately removed
        ;; because of the mouse event itself.
        (run-with-timer 1 nil (lambda ()
                                (let ((x-gtk-use-system-tooltips nil))
                                  (tooltip-show meaning)))))))

(use-package flyspell
  :diminish flyspell-mode
  :bind (:map ctl-period-map
              ("!" . flyspell-buffer))
  :preface
  (defun enable-flyspell ()
    "Unbind C-. from `flyspell-mode-map'."
    (flyspell-mode +1)
    (unbind-key "C-." flyspell-mode-map))

  :hook ( (( markdown-mode latex-mode TeX-mode message-mode
             org-mode)
           . enable-flyspell)
          (prog-mode . flyspell-prog-mode))
  :init
  (setq flyspell-auto-correct-binding (kbd "C-:"))

  :config
  (setq flyspell-delay 5))

(use-package ispell
  :bind (:map ctl-period-map
              ("f" . ispell-word))
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary personal-dictionary-file))

;; ──────────────────────────────────────────────────────────────────

(use-package popup :defer t :ensure t)

;;; Completion at Point
;; ――――――――――――――――――――――――――――――――――――――――

(use-package company
  :ensure t
  :bind ( :map ctl-m-map
          (("i"   . company-complete)
           ("C-c" . company-complete)
           ("u"   . company-complete-unicode)))
  :hook (after-init . global-company-mode)
  :diminish company-mode
  :config
  (define-key company-mode-map [remap indent-for-tab-command]
              #'company-indent-or-complete-common)

  (bind-keys :map company-active-map
             ("C-j"   . company-complete-selection)
             ("<tab>" . company-complete-common-or-cycle)
             ("C-n"   . company-select-next-or-abort)
             ("C-p"   . company-select-previous-or-abort))

  (setq company-idle-delay 2.0
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-offset-display 'lines
        company-format-margin-function nil)

  ;; https://www.reddit.com/r/emacs/comments/nichkl/how_to_use_different_completion_styles_in_the/
  (advice-add 'company-capf
              :around
              (lambda (capf-fn &rest args)
                (let ((completion-styles '(basic partial-completion)))
                  (apply capf-fn args))))

  :preface
  (defun company-mode-quicker ()
    "Makes `company-mode' show completions quickly in a buffer."
    (interactive)
    (make-local-variable 'company-idle-delay)
    (setq-local company-idle-delay 0.1)
    (company-mode +1)))


(use-package company-statistics
  :ensure t
  :hook (after-init . company-statistics-mode)
  :custom (company-statistics-file (expand-file-name "var/company-statistics.el"
                                                     user-emacs-directory)))

;; ──────────────────────────────────────────────────────────────────

(use-package dired :custom (dired-kill-when-opening-new-dired-buffer t))
(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("C-c u" . dired-up-directory)
         ("M-<"   . dired-to-first-entry)
         ("M->"   . dired-to-last-entry)
         ("a"     . consult-grep-dwim)
         ("r"     . consult-ripgrep)
         ("z"     . kill-buffer-delete-window)
         ("j"     . dired-x-find-file)
         ("f"     . project-find-file)
         ("~"     . dired-go-home))
  :hook ((dired-after-readin . dired-hide-details-mode)
         (dired-mode         . hl-line-mode)
         (dired-mode         . dired-omit-mode))
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-hide-details-hide-information-lines nil)

  :config
  (define-repeat-map ("u" . dired-up-directory))

  (setq dired-auto-revert-buffer t)

  ;; Hide all files that start with a dot when `dired-omit-mode' is active.
  (setq dired-omit-files "^\\..*$"
        dired-omit-verbose nil)

  (use-package dired-narrow
    :ensure t
    :bind (:map dired-mode-map
                ("/" . dired-narrow)))

  (use-package dired-subtree
    :ensure t
    :bind (:map dired-mode-map
                ("I" . dired-subtree-toggle)))
  :preface
  (defun dired-to-first-entry ()
    "Move point to the first interesting entry in a dired buffer."
    (interactive)
    (goto-char (point-min))
    (dired-goto-next-file))

  (defun dired-to-last-entry ()
    "Move point to the last interesting entry in a dired buffer."
    (interactive)
    (goto-char (point-max))
    (dired-previous-line 1))

  (defun dired-go-home ()
    "Switch current directory to ~/."
    (interactive)
    (dired-jump nil (expand-file-name "~/"))))

(use-package dired-recent
  :ensure t
  :bind (
         :map global-map
         ("C-x C-d" . dired-recent-open)
         :map dired-mode-map
         ("C-x C-d" . dired-recent-open)
         )
  :hook (dired-mode . dired-recent-mode)
  :config
  (advice-add 'dired-recent-save-list
              :before
              (lambda (&rest _args)
                (require 'f)
                (setq dired-recent-directories
                      (->> (and (boundp 'projectile-known-projects)
                                projectile-known-projects)
                           (-concat dired-recent-directories)
                           -distinct
                           (-filter #'stringp)
                           (-filter #'f-directory-p))))))



(use-package dired-collapse
  :ensure t
  :hook (dired-mode . dired-collapse-mode))


(use-package discover
  :ensure t
  :hook (dired-mode . dired-turn-on-discover))


;;; SNIPPETS and ABBREVS
;; ――――――――――――――――――――――――――――――――――――――――

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          ;; Placed these at the end.
          try-expand-list
          try-expand-line)))

(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq-default abbrev-mode t
                save-abbrevs t
                abbrev-file-name (expand-file-name "lib/abbrevs/abbrev-defs"
                                                   user-emacs-directory))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))


  :config
  (advice-add 'abbrev-insert
              :after-while
              (lambda (abbrev-symbol _ start _end)
                (let ((end (+ start (length (symbol-value abbrev-symbol)))))
                  (pulse-momentary-highlight-region start end)))))



(use-package yasnippet
  :defer 2
  :ensure t
  :diminish yas-minor-mode
  :bind (("C-c & C-h" . yas-describe-tables)
         :map yas-keymap
         ("<tab>" . my-yas-next-field-or-maybe-expand))
  :preface
  (defun my-yas-next-field-or-maybe-expand ()
    "When in company, stay in company."
    (interactive)
    (if (company-tooltip-visible-p)
        (company-complete-common-or-cycle)
      (yas-next-field-or-maybe-expand)))
  :init
  (setq yas-verbosity 2)

  :config
  (setq yas-key-syntaxes '(yas-try-key-from-whitespace))

  ;; Keep personal snippets away from existing default snippets.
  (push (expand-file-name "snippets/" user-emacs-directory)
        yas-snippet-dirs)
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :doc
  "Extra snippets for yas-snippet!"
  :ensure t
  :after yasnippet)

(use-package yankpad
  :doc
  "Keep yas-snippets in an org file."
  :ensure t
  :bind (:map ctl-quote-map
              ("a i" . yankpad-insert)
              ("a k" . yankpad-capture-snippet)
              ("a c" . yankpad-set-category-and-insert))
  :preface
  (defun yankpad-set-category-and-insert ()
    (interactive)
    (yankpad-set-category)
    (yankpad-insert))

  :config
  (setd yankpad-file "etc/yankpad.org"))

;;; Personal Finance
;; ――――――――――――――――――――――――――――――――――――
(use-package ledger-mode
  :ensure t
  :defer 5
  :mode "\\.journal\\'"
  :config
  (setq-default ledger-master-file
                (expand-file-name "~/miscellany/personal/finance/accounting.journal"))
  (setq ledger-default-date-format "%Y-%m-%d")

  (mapc (lambda (r)
          (unless (-contains? ledger-reports r)
            (push r ledger-reports)))
        '(("balancesheet" "%(binary) -f %(ledger-file) b ^assets ^liabilities --depth 3"))))

(use-package hledger-mode
  :pin manual
  :load-path "packages/rest/hledger-mode/"
  :mode ("\\.hledger\\'")
  :bind (("C-c j" . hledger-run-command)

         :map hledger-view-mode-map
         ("s" . chart-numbers-on-line)
         ("z" . calc-store-numbers-on-line))
  :config

  (setq hledger-jfile
        (expand-file-name "~/miscellany/personal/finance/accounting.journal"))

  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company)))

  (add-hook 'hledger-view-mode-hook #'hl-line-mode)
  (add-hook 'hledger-view-mode-hook
            (lambda ()
              (run-with-timer 0.5
                              nil
                              (lambda ()
                                (when (get-buffer-window hledger-reporting-buffer-name)
                                  (with-current-buffer hledger-reporting-buffer-name
                                    (center-text-for-reading))))))))


(use-package hledger-input
  :pin manual
  :load-path "packages/rest/hledger-mode/"
  :bind ( ("C-c e" . hledger-capture)
          :map hledger-input-mode-map
          ("C-c +"     . hledger-increment-amount)
          ("C-c <tab>" . ledger-input-expand-xact)
          ("C-c C-b"   . popup-balance-at-point))

  :hook ((hledger-input-post-commit . hledger-show-new-balances)
         (hledger-input-mode        . auto-fill-mode))

  :config
  (setq hledger-input-buffer-height 20)
  (add-hook 'hledger-input-mode-hook #'company-mode-quicker)

  :preface
  (defun popup-balance-at-point ()
    "Show balance for account at point in a popup."
    (interactive)
    (if-let ((account (thing-at-point 'hledger-account)))
        (message (hledger-shell-command-to-string (format " balance -N %s "
                                                          account)))
      (message "No account at point")))

  (defun ledger-input-expand-xact ()
    (interactive)
    (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
      (delete-region (point-at-bol) (point-at-eol))
      (insert (with-current-buffer (ledger-exec-ledger nil nil "xact" (shell-quote-argument s))
                (buffer-substring (point-min) (point-max)))))))

;;; Language Server Protocol
;;  ------------------------

(use-package lsp-java
  :doc "Install for some convenience functions but keep it disabled, use
`eglot' instead."
  :ensure t
  :defer t)

(use-package eglot
  :ensure t
  :defer t
  :bind ( :map eglot-mode-map
          ("C-c C-j" . eglot-reconnect)
          ("C-c r g" . eglot-code-actions)
          ("C-c r r" . eglot-rename)
          ("C-c d"   . toggle-eldoc-doc-buffer) )
  :init
  ;; Default: 4KB is too low for LSP servers.
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024))

  (hook-into-modes #'eglot-ensure
                   'clojure-mode
                   'java-mode 'rust-mode 'python-mode
                   'go-mode 'c-mode 'c++-mode)
  :config
  (setq eglot-connect-timeout 300)
  (setq eglot-autoshutdown t)

  (dolist (lang-server-spec `((rust-mode         . ("rustup" "run" "stable" "rust-analyzer"))
                              ((c-mode c++-mode) . ("clangd"))
                              (java-mode         . ,#'java-eclipse-jdt-launcher)))
    (add-to-list 'eglot-server-programs lang-server-spec))

  ;; :preface
  ;; (cl-defmethod eglot-handle-notification
  ;;   (_server (_method (eql language/status)) &key _type message &allow-other-keys)
  ;;   (message "%s" message))
  )


(use-package treesit
  :doc
  "[2023-02-08 Wed 22:48] Tree-sitter support is now built into Emacs.
   The directory where shared libraries for language grammars are
   installed is not configurable yet (fixed to tree-sitter under
   `user-emacs-directory'."
  :defer t
  :custom (treesit-)
  :config
  (dolist (grammar
           '((css "https://github.com/tree-sitter/tree-sitter-css")
             (rust "https://github.com/tree-sitter/tree-sitter-rust")
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
             (python "https://github.com/tree-sitter/tree-sitter-python")
             (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
             (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
    (add-to-list 'treesit-language-source-alist grammar)

    :preface
    (defun install-tree-sitter-grammer-if-required (language &optional quiet)
      "Given a language symbol install tree-sitter grammer if not
       already avaialble."
      (if (treesit-language-available-p language)
          (unless quiet
            (message "Tree-sitter grammar for %s already installed." language))
        (treesit-install-language-grammar language)))))


(use-package combobulate :git "https://github.com/mickeynp/combobulate")


;;; ----------------------------------------------------------------------------

(use-package display-line-numbers
  :bind ( :map ctl-period-map
          ([\C-m] . display-line-numbers-and-column-indicator) )
  :preface
  (defun display-line-numbers-and-column-indicator (arg)
    (interactive "P")
    (display-line-numbers-mode (if display-line-numbers-mode -1 +1))
    (when (not arg)
      (display-fill-column-indicator-mode (if display-fill-column-indicator-mode -1 +1)))))

(use-package type-break
  :disabled t
  :bind (:map ctl-quote-map
              ("b" . type-break))
  :init
  (setq type-break-file-name nil)
  (setq type-break-mode-line-message-mode t)

  (type-break-mode +1))

(use-package compile
  :defer t
  :preface
  (defun compilation-escape-colors-to-ansi-colors ()
    "Colorize from `compilation-filter-start' to `point'.
  https://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html"
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  :init
  (add-hook 'compilation-filter-hook
            #'compilation-escape-colors-to-ansi-colors)
  :config
  (require 'ansi-color)

  (setq compilation-scroll-output t))

(use-package eldoc
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode +1)
  :preface
  (defun toggle-eldoc-doc-buffer ()
    "Depends upon internal details of `eldoc-mode'."
    (interactive)
    (if-let ((w (some-window (lambda (w) (eq (window-buffer w)
                                             eldoc--doc-buffer)))))
        (delete-window w)
      (display-buffer (eldoc-doc-buffer)))))

(use-package which-func
  :disabled t
  :doc "Display the current function in the mode line."
  :config
  (setq which-func-modes '(java-mode))
  (setq which-func-unknown "<λ>")
  (which-function-mode +1))

(use-package gud
  :config
  (setq gdb-show-main t
        ;; Change this to `t' or use command `gdb-many-windows' to
        ;; have a fancier IDE like UI.
        gdb-many-windows nil))

(use-package realgud
  :doc "`gud' with bells and whistles."
  :defer t
  :ensure t)

(use-package "indent"
  :init
  (setq-default indent-tabs-mode nil
                tab-always-indent 'complete))

(use-package xref
  :doc "Find definitions like the coolest kid."
  :ensure t
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-go-back)))

(use-package subword
  ;; :hook (prog-mode . subword-mode)
  :diminish subword-mode
  :init
  (global-subword-mode +1))


(use-package help-mode
  :bind ( :map help-mode-map
          ("C-c C-j" . jump-to-major-mode-section) )
  :custom
  (help-window-select  t)

  :init
  (defvar --help-mode-last-major-mode major-mode)
  (advice-add 'describe-bindings
              :before
              (lambda (&rest _args)
                (setq --help-mode-last-major-mode major-mode)))

  (defun jump-to-major-mode-section ()
    (interactive)
    (search-forward (symbol-name --help-mode-last-major-mode))
    (beginning-of-line)
    (recenter 0 t)))

(use-package help-at-pt
  :bind ( :map ctl-m-map ("." . display-help-at-pt-dwim) )

  :config
  (require 'popup)
  (define-repeat-map ("<" . scan-buf-previous-region)
                     (">" . scan-buf-next-region)
                     ("." . display-help-at-pt-dwim))

  :preface
  (defun display-help-at-pt-dwim (&optional prefix)
    (interactive "P")
    (when-let ((help (or (help-at-pt-kbd-string)
                         (progn (scan-buf-next-region (if prefix -1 +1))
                                (help-at-pt-kbd-string)))))
      (popup-tip help
                 :point (point)
                 :around t
                 :margin 2))))

(use-package flymake
  :diminish flymake-mode
  :bind ( :map flymake-mode-map
          ("M-g n" . flymake-goto-next-error)
          ("M-g p" . flymake-goto-prev-error) ))

(use-package flycheck
  :ensure t
  :bind-keymap ("C-x C-." . flycheck-command-map)
  :bind ( :map flycheck-command-map
          ("C-." . list-linter-errors)

          :map ctl-m-map
          ("!" . list-linter-errors) )
  :init
  (setq flycheck-indication-mode 'left-margin)
  (setq flycheck-mode-line-prefix "")

  (hook-into-modes #'flycheck-mode 'clojure-mode)

  :preface
  (defun list-linter-errors ()
    (interactive)
    (cond
     ((memq 'flymake-mode local-minor-modes)
      (flymake-show-diagnostics-buffer))

     (memq 'flycheck-mode local-minor-modes)
     (flycheck-list-errors)

     t
     (message "No minor mode to run a linter."))))

(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :bind ( :map ctl-period-map
          ("C-l" . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive nil

        highlight-indent-guides-auto-odd-face-perc 3
        highlight-indent-guides-auto-even-face-perc 3
        highlight-indent-guides-auto-character-face-perc 3))

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :hook ((emacs-lisp-mode clojure-mode racket-mode scheme-mode cc-mode rust-mode) . aggressive-indent-mode))

(use-package comint
  :defer t
  :config
  (setq comint-scroll-show-maximum-output nil))

(use-package shell
  :bind ( "s-<return>" . shell )
  :init
  (add-hook 'shell-mode-hook #'--shell-mode-kill-buffer-on-exit )

  :preface
  (defun --shell-mode-kill-buffer-on-exit ()
    "Augments the existing sentinal function for a buffer process
     with buffer and window clean up on exit."
    (let* ((p (get-buffer-process (current-buffer)))
           (original-sentinal (process-sentinel p)))
      (set-process-sentinel p
                            (lambda (process signal)
                              (funcall original-sentinal process signal)
                              (when (and (memq (process-status process) '(exit signal))
                                         (buffer-live-p (process-buffer process)))
                                (kill-buffer (process-buffer process))
                                (when (< 1 (count-windows))
                                  (delete-window))))))))

(use-package eshell
  :bind ( :map ctl-quote-map
          ("C-p" . eshell-toggle) )

  :preface
  (defun eshell-toggle (arg)
    (interactive "P")
    (let ((directory default-directory)
          (text-in-region
           (when (region-active-p)
             (buffer-substring (region-beginning) (region-end)))))
      (if (eq major-mode 'eshell-mode)
          (set-window-configuration (get this-command :window-config))
        (put this-command :window-config (current-window-configuration))
        (eshell)
        (when arg
          (insert (format "; cd %s ; " (shell-quote-argument directory)))
          (eshell-send-input))
        (when text-in-region
          (insert text-in-region)))))

  :init
  (setq eshell-modules-list
        '( eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs
           eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt
           eshell-script eshell-term eshell-tramp eshell-unix eshell-xtra ))

  (setq eshell-prompt-regexp "([^#$]*) [#$] "
        eshell-prompt-function
        (lambda ()
          (concat "(" (file-name-nondirectory (eshell/pwd)) ")"
                  (if (= (user-uid) 0) " # " " $ ")))

        eshell-aliases-file
        (expand-file-name "./etc/eshell-aliases" user-emacs-directory))

  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; `orderless' doesn't make much sense for shell completion.
              (setq-local completion-styles '(basic partial-completion emacs22))))

  ;; (eval-after-load 'em-cmpl
  ;;   '(define-key eshell-cmpl-mode-map [tab] #'company-indent-or-complete-common))

  ;; ANSI colors in Eshell buffers.
  (add-hook 'eshell-preoutput-filter-functions
            ;; Or filter ANSI escape sequences with 'ansi-color-filter-apply
            'ansi-color-apply))

;;; DevOps
;; ──────────────────────────────────────────────────────────────────

(use-package sh-script
  :init
  (add-hook 'sh-mode
            (lambda ()
              (when (not (executable-find "shellcheck"))
                (message "Not installed on system: `shellcheck'!")))))


(use-package jinja2-mode :defer t :ensure t)

;;; HASKELL-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package haskell-mode
  :ensure t
  :defer t
  :bind (:map haskell-mode-map
              ("C-c C-k" . haskell-compile) )
  :hook ((haskell-mode . haskell-doc-mode)
         (haskell-mode . haskell-indentation-mode)))

(use-package proof-general :defer t :ensure t)

;;; GRAPHICS
;; ──────────────────────────────────────────────────────────────────

(use-package gnuplot
  :defer t
  :ensure t
  :custom (gnuplot-program "gnuplot-wx"))

(use-package graphviz-dot-mode
  :ensure t
  :defer t)

;;; Notes, Journal and Task Manager
;;  ─────────────────────────────────────────────────────────────────

(use-package timeclock
  :init
  (setq timeclock-file
        (expand-file-name "timelog" emacs-assets-directory)))

(use-package org-config
  :load-path "etc/"
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-config-capture)
         ("C-c a" . org-agenda)
         ("C-c n" . org-next-link)
         ("C-c p" . org-previous-link)

         ;; Bindings for using the Timer
         :map ctl-m-map
         ("x s" . org-timer-start)
         ("x S" . org-timer-stop)
         ("x c" . org-timer-set-timer)
         ("x ." . org-timer)

         :map ctl-quote-map
         ("C-n" . open-org-file)
         ("C-d" . search-notes-files))
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (let ((typo-abbrevs-file (expand-file-name "lib/abbrevs/typos-defs"
                                                         user-emacs-directory)))
                (when (file-exists-p typo-abbrevs-file)
                  (make-thread (lambda ()
                                 (quietly-read-abbrev-file typo-abbrevs-file)))))))

  (bind-key "C-c a" #'org-agenda)
  (eval-after-load "org" '(require 'org-config))

  ;; ;; `org-agenda-get-restriction-and-command' ignores rules for
  ;; ;; displaying buffers (i.e. `display-buffer-alist'). This advice
  ;; ;; tries causes `split-window-sensibly' to always split vertically
  ;; ;; and show the " *Agenda Commands*" buffer below the current buffer.
  ;; (advice-add 'org-agenda-get-restriction-and-command
  ;;             :around
  ;;             (lambda (original &rest args)
  ;;               (let ((split-window-preferred-function #'split-window-below))
  ;;                 (apply original args))))
  )


(use-package pomodoro
  :load-path "packages/rest/pomodoro"
  :defer 5
  :diminish pomodoro-mode
  :commands (pomodoro-mode pomodoro-append-to-org-agenda pomodoro-jump-to-org-heading)
  :bind ( :map ctl-m-map
          ("x <return>" . pomodoro-jump-to-org-heading)
          ("x SPC"      . pomodoro-remove-notifications)
          ("x i"        . pomodoro-summarize)
          ("x p"        . pomodoro-start)
          ("x e"        . pomodoro-edit-title)
          ("x b"        . pomodoro-start-break)
          ("x B"        . pomodoro-start-long-break) )
  ;; :init
  ;; (add-hook 'org-agenda-finalize-hook #'pomodoro-append-to-org-agenda)
  :config
  (pomodoro-mode +1))

(use-package thingatpt+
  :load-path "packages/lisp/"
  :commands tap-bounds-of-string-contents-at-point)


;;; Sessions
;;; ----------------------------------------------------------------------------

(use-package desktop
  :disabled t
  :init
  (desktop-save-mode +1)
  :config
  (desktop-auto-save-enable))


;;; Regions
;;; ----------------------------------------------------------------------------
(use-package delsel :init (delete-selection-mode +1))
(use-package expand-region
  :ensure t
  :doc
  "Hyperbole provides similar functionality through
  hui-select.el but I have found `expand-region' to be more
  intuitive."
  :bind (:map ctl-period-map
              ("@" . er/expand-region)))

(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setq plantuml-jar-path
        (expand-file-name "~/miscellany/assets/plantuml.jar"))

  (use-package ob-plantuml
    :custom (org-plantuml-jar-path plantuml-jar-path)))

;;; Managing Secrets
;; ──────────────────────────────────────────────────────────────────
(use-package password-management
  :bind  (:map ctl-period-map
               ("p c" . password-store-copy)
               ("p g" . password-store-generate)
               ("p i" . password-store-insert)
               ("p u" . pass))
  :load-path "/etc"
  :init
  (setenv "PASSWORD_STORE_DIR"
          (expand-file-name "~/miscellany/personal/secrets")))

;; W3M
;; ──────────────────────────────────────────────────────────────────
(use-package shr
  :doc "The HTML parser that is used at multiple places in Emacs"
  :defer t
  :init
  (setq shr-width 90
        shr-use-fonts nil
        ;; Gnus Article buffers look better with this.
        shr-use-colors nil
        ;; These are not used when `shr-use-colors' is `nil'.
        shr-color-visible-distance-min 10
        shr-color-visible-luminance-min 60))

;;; Minibuffer Completion
;;; ----------------------------------------------------------------------------

(use-package minibuffer
  :bind ( :map minibuffer-mode-map
          ("C-w" . yank-symbol-to-minibuffer-or-kill-region ) )
  :config
  (setq enable-recursive-minibuffers t
        history-delete-duplicates t
        history-length 1000)
  (minibuffer-depth-indicate-mode +1)

  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (when (< 10 (minibuffer-depth))
                (top-level))))

  :init
  ;; `partial-completion' is very useful for `completion-at-point'
  (advice-add 'completion-at-point
              :around
              (lambda (compl-at-point &rest args)
                (let ((completion-styles '(basic partial-completion)))
                  (apply compl-at-point args))))
  :preface
  (defvar yank-symbol-to-minibuffer-or-kill-region)
  (defun yank-symbol-to-minibuffer-or-kill-region (&optional arg)
    (interactive "P")
    (if (region-active-p)
        (call-interactively #'kill-region)
      (insert (with-minibuffer-selected-window
                (let ((starting-point (if (eq last-command this-command)
                                          yank-symbol-to-minibuffer-or-kill-region
                                        (setf yank-symbol-to-minibuffer-or-kill-region (point))))
                      (p (point)))
                  (if arg
                      (forward-word 1)
                    (forward-symbol 1))
                  (pulse-momentary-highlight-region starting-point (point))
                  (buffer-substring p (point))))))))

(use-package minibuffer-command-history
  :load-path "etc/"
  :commands minibuffer-command-history
  :hook (after-init . minibuffer-command-history-enable)
  :config
  (add-to-list 'savehist-additional-variables 'minibuffer-command-history))

(use-package orderless
  :doc
  "TODO: Opt-in this completion style when needed instead of making it
   the default because it breaks completion of commands and filenames
   in minibuffer and eshell.

   If anything with completions breaks, this is usually the culprit."
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package vertico
  :doc "https://github.com/minad/vertico/wiki"
  :ensure t
  :bind ( :map ctl-m-map ("r" . vertico-repeat) )
  :hook (minibuffer-setup . vertico-repeat-save)
  :init (vertico-mode +1))

(use-package embark
  :ensure t
  :bind (("C-c C-." . embark-act)))

(use-package consult
  :ensure t
  :custom (consult-preview-key "M-.")
  :bind ( :map global-map
          ("M-s a" . consult-grep-dwim)
          ("C-x b" . consult-buffer)
          ("M-y"   . yank-pop)

          :map isearch-mode-map
          ("C-." . consult-line)

          :map minibuffer-local-map
          ("M-r" . consult-history)

          :map ctl-m-map
          ("z" . consult-complex-command)

          :map ctl-quote-map
          ("C-'" . consult-imenu)

          :map ctl-period-map
          ("C-s" . consult-line))
  :preface
  (defun consult-grep-dwim ()
    (interactive)
    (let ((p (project-current)))
      (cond
       ;; No project
       ((not p)
        (consult-grep))

       ;; A git project
       ((eq 'Git (vc-responsible-backend (project-root p)))
        (consult-git-grep (project-root p)))

       ;; A non-git project
       (t (consult-grep (project-root p)))))))


;;; Programming Languages
;;; ──────────────────────────────────────────────────────────────────

(defun java-eclipse-jdt-launcher (_arg)
  "Returns a command to start Eclipse JDT launcher script `jdtls'."
  (let ((launcher-script (expand-file-name "org.eclipse.jdt.ls.product/target/repository/bin/jdtls" "~/code/eclipse.jdt.ls/"))
        (root-directory (project-root (project-current))))
    (if (file-exists-p launcher-script)
        (list launcher-script
              "-data"
              (expand-file-name (format "%s-%s" (md5 root-directory)
                                        (file-name-base (directory-file-name root-directory )))
                                "~/code/jdt-workspace/"))
      (message "Failed to find any JDT jar files.")
      nil)))

(use-package java-mode
  :defer t
  :hook ( (java-mode . company-mode-quicker) )

  :config
  (cl-defmethod eglot-handle-notification
    (server (_method (eql language/status)) &key type message &allow-other-keys)
    (when (equal type "ServiceReady")
      (message "LSP server ready: %s" (eglot-project-nickname server)))))

(use-package javadoc-lookup
  :ensure t
  :init
  (setq javadoc-lookup-completing-read-function completing-read-function)
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-local browse-url-browser-function 'eww-browse-url)
              (define-key java-mode-map (kbd "C-c ; d")  #'javadoc-lookup)
              (define-key java-mode-map (kbd "C-c ; i")  #'javadoc-add-import)
              (define-key java-mode-map (kbd "C-c ; s")  #'javadoc-sort-imports))))

(use-package gradle-mode
  :doc "Gradle integration in Emacs."
  :ensure t
  :hook (java-mode . gradle-mode)
  :diminish gradle-mode
  :config
  (setq gradle-use-gradlew t
        gradle-gradlew-executable "./gradlew"))

(use-package scala-mode
  :ensure t
  :pin melpa
  :defer t
  :config
  (use-package sbt-mode
    :ensure t
    :pin melpa)

  (use-package ensime
    :ensure t
    :pin melpa))

(use-package cc-mode
  :bind ( :map c-mode-base-map
          ("<return>" . newline-and-indent)
          ("C-c C-k"  . compile)
          ("C-c C-t"  . c-toggle-comment-style) )
  :init
  (setq c-default-style '((java-mode . "*java*")
                          (awk-mode . "awk")
                          (other . "gnu")))
  :config
  (setq-default c-block-comment-flag t
                c-auto-newline t
                c-basic-offset 4)

  (c-add-style "*java*" '("java"
                          ;; https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
                          (c-hanging-semi&comma-criteria
                           c-semi&comma-no-newlines-for-oneline-inliners
                           c-semi&comma-inside-parenlist
                           c-semi&comma-no-newlines-before-nonblanks)
                          (c-hanging-braces-alist
                           (defun-open after)
                           (defun-close before after)
                           (class-open after)
                           (class-close before after)
                           (inexpr-class-open after)
                           (inexpr-class-close before)
                           (namespace-open after)
                           (inline-open after)
                           (inline-close before after)
                           (block-open after)
                           (block-close . c-snug-do-while)
                           (extern-lang-open after)
                           (extern-lang-close after)
                           (statement-case-open after)
                           (substatement-open after)))))

(use-package info-lookmore
  :doc "Adds extra info manuals for `info-lookup-symbol'."
  :after info
  :load-path "packages/lisp/"
  :config
  (info-lookmore-coreutils-index)
  (info-lookmore-makefile-derivatives)
  (info-lookmore-c++-use-c)
  (info-lookmore-c-gsl)
  (info-lookmore-c-readline)
  (info-lookmore-c-gcc)
  (info-lookmore-apropos-elisp))

(use-package dtrt-indent
  :doc "Automatically guess offset and tabs-indent for opened file."
  :ensure t
  :disabled t
  :diminish dtrt-indent-mode
  :hook (java-mode . dtrt-indent-mode))

(use-package pos-tip
  :doc "Racer needs this package and for some reason, it is not
  installed."
  :ensure t)

(use-package rust-mode
  :defer t
  :ensure t
  :bind ( :map rust-mode-map ("RET" . newline-and-indent) )
  :hook ((rust-mode . eldoc-mode)
         (rust-mode . cargo-minor-mode)
         (rust-mode . combobulate))
  :init
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))

  :config
  (install-tree-sitter-grammer-if-required 'rust t))

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup)
  :ensure t)

(use-package cargo
  :ensure t
  :after rust-mode
  :diminish cargo-minor-mode
  :config
  (define-key cargo-mode-map (kbd "C-. C-k") #'cargo-process-check))


(use-package python-mode
  :ensure t
  :defer t
  :config
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil
        python-indent-offset 2))

(use-package py-autopep8
  :ensure t
  :hook (python-mode . py-autopep8-mode))

(use-package prolog
  :mode ("\\.pl\\'" . prolog-mode)
  :custom (prolog-system 'gnu))

(use-package emacs-lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-k" . eval-buffer*))
  :preface
  (defun eval-buffer* ()
    "Eval current buffer with some logging for feedback."
    (interactive)
    (eval-buffer (current-buffer))
    (message "%s" (propertize "=> Buffer Evaluation Complete"
                              'face
                              'compilation-mode-line-exit)))
  :init
  (use-package highlight-defined
    :ensure t
    :hook (emacs-mode . highlight-defined-mode)))

(use-package plisp-mode
  :doc "https://picolisp.com/wiki/?Documentation"
  :ensure t)

(use-package xray :load-path "package/lisp/" :defer t)
(use-package macrostep  :defer t :ensure t)

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((( emacs-lisp-mode inferior-emacs-lisp-mode
            lisp-mode clojure-mode clojurescript-mode
            cider-repl-mode racket-mode scheme-mode
            eval-expression-minibuffer-setup)
          . enable-paredit-mode )

         (prog-mode . sub-paredit-mode))

  :init
  (advice-add 'paredit-forward-kill-word :around #'preserve-kill-ring)

  :bind ( :map paredit-mode-map
          ("M-S" . paredit-splice-sexp)
          ("M-R" . paredit-raise-sexp) )
  :preface
  (define-minor-mode sub-paredit-mode
    "A subset of paredit for all `prog-mode' buffers."
    :keymap (let ((k (make-sparse-keymap)))
              (define-key k (kbd "C-)") #'paredit-forward-slurp-sexp)
              (define-key k (kbd "C-}") #'paredit-forward-barf-sexp)
              (define-key k (kbd "M-S") #'paredit-splice-sexp)
              (define-key k (kbd "M-R") #'paredit-raise-sexp)
              (define-key k (kbd "C-k") #'paredit-kill)
              k))

  :config
  (dolist (k (list "M-r" "M-s" "M-?" "RET" "C-d"))
    (define-key paredit-mode-map (kbd k) nil)))

(use-package clojure-mode
  :ensure t
  :pin melpa
  :defer t
  :mode "\\.clj\\'"
  :config
  (setq clojure-indent-style :always-align
        clojure-align-forms-automatically t)

  (add-hook 'project-find-functions #'project-find-clojure-root)

  (add-hook 'clojure-mode-hook
            (lambda ()
              ;; Default completion is completely broken because of
              ;; `lisp-indent-line'.
              (company-mode-quicker)
              (setq-local completion-at-point-functions
                          '(cider-complete-at-point eglot-completion-at-point t))))

  :preface
  (defun project-find-clojure-root (dir)
    (when-let ((r (clojure-project-root-path (or dir default-directory))))
      (cons 'clojure-project r)))

  (cl-defmethod project-root ((project (head clojure-project)))
    (cdr project)))

(use-package clj-refactor
  :pin melpa
  :ensure t
  :diminish clj-refactor-mode
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :custom ((cljr-insert-newline-after-require . nil)
           (cljr-auto-clean-ns . nil))
  :bind (:map clj-refactor-map
              ("C-; c <SPC>" . clojure-top-level-spacing))
  :config
  (cljr-add-keybindings-with-prefix "C-;")
  (add-hook 'clojure-mode-hook
            (lambda ()
              (add-hook #'before-save-hook
                        (lambda ()
                          (when (eq major-mode 'clojure-mode)
                            (make-thread (lambda ()
                                           (cljr--clean-ns nil :no-pruning)))))))))

(use-package cider
  :doc
  :ensure t
  :diminish cider-mode
  :hook (clojure-mode . cider-mode)
  :bind ( :map cider-mode-map
          ("M-." . cider-find-var-dwim) )
  :config
  (setq nrepl-log-messages t
        nrepl-use-ssh-fallback-for-remote-hosts t)

  (setq cider-repl-use-pretty-printing t
        cider-auto-jump-to-error nil
        cider-prompt-for-symbol nil
        cider-repl-history-file (expand-file-name "var/cider-repl-history.el"
                                                  user-emacs-directory)
        cider-eldoc-display-for-symbol-at-point nil
        cider-eldoc-display-context-dependent-info t

        cider-repl-use-content-types t
        cider-use-xref t
        cider-xref-fn-depth 100)

  ;; (advice-add 'cider-repl-indent-and-complete-symbol
  ;;             :around
  ;;             (lambda (&rest _args)
  ;;               (completion-at-point)))

  (define-clojure-indent (for-all 1))


  :preface
  (defun cider-find-var-dwim ()
    (interactive)
    (if (cider-connected-p)
        (cider-find-var)
      (call-interactively #'xref-find-definitions))))


(use-package flycheck-clojure
  :ensure t
  :after clojure-mode
  :config (flycheck-clojure-setup))

(use-package flycheck-clj-kondo
  :ensure t
  :after clojure-mode
  :config
  (unless (executable-find "clj-kondo")
    (async-shell-command
     (concat "cd /tmp;"
             "curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo;"
             "chmod +x install-clj-kondo;"
             "./install-clj-kondo --dir ~/.local/bin/;"))))

(use-package geiser
  :defer t
  :ensure t
  :hook (racket-mode . geiser-mode)
  :init
  (use-package geiser-racket :ensure t)
  (use-package geiser-mit    :ensure t)

  :config
  (advice-add 'run-geiser
              :after
              (lambda (&rest _args)
                (message "Set the scheme impl for buffer with `geiser-set-scheme`.''"))))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :defer t
  :ensure t
  :init
  (use-package scribble-mode :ensure t))

(use-package slime
  :ensure t
  :commands slime-connected-p
  :preface
  (defun start-slime ()
    (when (not (slime-connected-p))
      (save-window-excursion (slime))))
  :config
  (require 'slime-editing-commands)

  (setq inferior-lisp-program "sbcl")
  (add-to-list 'slime-contribs
               'slime-autodoc))

(use-package tuareg
  :doc "For Ocaml, whenever I start learning more."
  :defer t
  :ensure t)

;;; [WO]MAN-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package man
  :bind ( :map ctl-quote-map ("C-m" . man) )
  :config
  (setq Man-notify-method 'aggressive)
  (add-hook 'Man-mode-hook
            (lambda ()
              (set-face-attribute 'Man-overstrike nil
                                  :inherit font-lock-type-face
                                  :bold t)
              (set-face-attribute 'Man-underline nil
                                  :inherit font-lock-keyword-face
                                  :underline t))))

;;; WHITESPACE-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package whitespace
  :diminish whitespace-mode
  :hook ((message-mode . whitespace-mode))
  :bind (("C-x C-y" . whitespace-mode)
         ("C-x y"   . whitespace-toggle-options))
  :init
  (setq whitespace-display-mappings
        '( (space-mark   ?\xA0  [?¤]     [?_])
           (newline-mark ?\n    [?↵ ?\n] [?$ ?\n])
           (tab-mark     ?\t    [?» ?\t] [?\\ ?\t]) ))

  ;; Cleanup whitespace before saving files
  (add-hook 'before-save-hook
            (lambda ()
              ;; Exclude whitespace-sensitive modes that I know of.
              ;; 1. Graphics in `ob-ipython' do not work if we remove trailing
              ;;    newlines. So, this should be excluded for buffers in
              ;;    `image-mode'.
              (when (not (memq major-mode '(markdown-mode image-mode)))
                (whitespace-cleanup)
                (delete-trailing-whitespace)))))

;;; WEB DEVELOPMENT
;; ──────────────────────────────────────────────────────────────────
(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.phtml\\'"
         "\\.html\\'"
         "\\.tpl\\|.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'" )
  :config
  (setq web-mode-auto-close-style 2)

  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-markup-indent-offset 2
                    web-mode-css-indent-offset 2
                    web-mode-code-indent-offset 2)))

  (use-package emmet-mode
    :ensure t
    :hook (web-mode . emmet-mode)
    :config
    (define-key emmet-mode-keymap (kbd "C-c TAB") 'emmet-expand-line)
    (define-key emmet-mode-keymap (kbd "C-j") nil)
    (define-key emmet-mode-keymap (kbd "<C-return>") nil)))

(use-package skewer-mode
  :ensure t
  :hook ((css-mode  . skewer-css-mode)
         (html-mode . skewer-html-mode)
         (js2-mode  . skewer-mode)))

(use-package js2-mode        :defer t :ensure t)
(use-package typescript-mode :ensure t)
(use-package coffee-mode     :defer t :ensure t)
(use-package elm-mode        :defer t :ensure t)

;;; ERLANG AND ELIXIR
;; ──────────────────────────────────────────────────────────────────
(use-package erlang :defer t :ensure t)
(use-package elixir-mode
  :ensure t
  :defer t
  :config
  (bind-keys :map elixir-mode-map
             ("C-M-e" . elixir-end-of-defun)
             ("C-M-a" . elixir-beginning-of-defun)))

(use-package alchemist
  :ensure t
  :after elixir-mode
  :hook (elixir-mode . alchemist-mode)
  :bind (:map elixir-mode-map
              ("C-x C-e" . alchemist-eval-current-line))
  :init
  (setq alchemist-key-command-prefix (kbd "C-;")
        alchemist-goto-elixir-source-dir (expand-file-name "~/code/elixir/")
        alchemist-goto-erlang-source-dir (expand-file-name "~/code/otp/"))
  (add-hook 'alchemist-help-minor-mode-hook
            (lambda ()
              (bind-keys :map alchemist-help-minor-mode-map
                         ("RET" . alchemist-help-search-at-point)
                         ("l" . alchemist-help-last)
                         ("TAB" . alchemist-next-overlay)))))

;;; RUBY MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package ruby-mode
  :defer t
  :preface
  (defun insert-latest-gemspec (gem)
    "Print the latest version of GEM.
     This function also kills the gemspec so that it can be yanked
     immediately."
    (interactive "sGem: ")
    (message "Talking to RubyGems...")
    (request
     (format "https://rubygems.org/api/v1/versions/%s/latest.json" gem)
     :parser #'json-read
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((full-version-str (assoc-default 'version data))
               (minor-version-str (replace-regexp-in-string "\\.[0-9]+$"
                                                            ""
                                                            full-version-str))
               (gemspec (format "gem '%s', '~> %s', '>= %s'"
                                gem
                                minor-version-str
                                full-version-str)))
          (insert gemspec)
          (message "--> %s" gemspec))))
     :error
     (cl-function
      (lambda (&rest args &key error-thrown &allow-other-keys)
        (message "Failed with : %s" error-thrown)))))

  (defun search-ruby-gems (search-term)
    "Search for GEM at Rubygems."
    (interactive "sSearch: ")
    (message "Talking to RubyGems...")
    (request
     "https://rubygems.org/api/v1/search.json"
     :params `(("query" . ,search-term))
     :parser #'json-read
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let* ((completions-alist
                (mapcar
                 (lambda (gem)
                   (let ((gem-name (assoc-default 'name gem))
                         (gem-info (assoc-default 'info gem)))
                     (cons (format "%s : %s"
                                   gem-name
                                   (truncate-string-to-width gem-info 80 0 nil t))
                           gem-name)))
                 data))
               (chosen-gem (assoc-default (completing-read "Select: "
                                                           completions-alist)
                                          completions-alist)))
          (insert-latest-gemspec chosen-gem))))
     :error (cl-function
             (lambda (&rest args &key error-thrown &allow-other-keys)
               (message "Error: %s" error-thrown)))))
  :bind (:map ruby-mode-map
              ("C-c C-g" . search-ruby-gems))

  :config
  (eval-after-load 'ruby-mode
    '(progn
       (define-key ruby-mode-map (kbd "C-x t") nil)
       (define-key ruby-mode-map (kbd "C-x T") nil))))

(use-package rinari
  :ensure t
  :after ruby-mode
  :bind (:map ruby-mode-map
              ("C-c i" . rinari-insert-erb-skeletion))
  :config
  (global-rinari-mode))

(use-package robe
  :ensure t
  :defer t
  :init
  (add-hook 'ruby-mode-hook #'robe-mode))

(use-package haml-mode :ensure t :defer t)
(use-package rvm
  :doc "RVM doesn't work out of the box with eshell."
  :ensure t
  :defer t)

;;; GO MODE
;; ──────────────────────────────────────────────────────────────────
(use-package go-mode
  :ensure t
  :config
  (use-package go-guru :ensure t)
  (unless (getenv "GOPATH")
    (setenv "GOPATH" (shell-command-to-string "echo -n $GOPATH")))

  (require 'project)
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module))

;;; NIX
;; ──────────────────────────────────────────────────────────────────
(use-package nix-mode
  :ensure t
  :defer t
  :config
  (use-package nix-sandbox   :disabled t :ensure t)
  (use-package nixos-options :disabled t :ensure t)
  (use-package company-nixos-options
    :doc "I have disabled it as there are exceptions in eshell
    because of the backend."
    :disabled t
    :ensure t
    :config
    (add-to-list 'company-backends 'company-nixos-options)))

;;; Grammars, Parsers and Compilers
;; ──────────────────────────────────────────────────────────────────
(use-package antlr-mode
  :ensure t
  :mode ("\\.ag\\'"
         "\\.g4\\'"))

;;; Emacs Speaks Statistics
;; ──────────────────────────────────────────────────────────────────
(use-package ess :ensure t :defer t)

;;; LUA MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package lua-mode :defer t :ensure t)

;;; APL
;; ──────────────────────────────────────────────────────────────────
(use-package gnu-apl-mode
  :ensure t
  :hook ((gnu-apl-interactive-mode gnu-apl-mode) . em-gnu-apl-init)
  :init
  (setq gnu-apl-mode-map-prefix "C-' i "
        gnu-apl-interactive-mode-map-prefix "C-' i ")
  :preface
  (defun em-gnu-apl-init ()
    (require 'face-remap)
    (activate-input-method 'APL-Z)
    (setq buffer-face-mode-face 'gnu-apl-default)
    (buffer-face-mode)))

;;; YAML
;; ──────────────────────────────────────────────────────────────────
(use-package flycheck-yamllint
  :ensure t
  :hook ((yaml-mode . flycheck-yamllint-setup)
         (yaml-mode . flycheck-mode))
  :config
  (when (not (executable-find "yamllint"))
    (message "Please install yamllint with `pip install yamllint'.")))

(use-package yaml-mode
  :defer t
  :ensure t
  :mode "\\.yml\\'")

;;; Version Control
;;  ─────────────────────────────────────────────────────────────────

(use-package vc
  :bind ("C-x v C-s" . vc-log-search)
  :config
  (setq vc-display-status t))

(use-package diff-mode
  :config
  (setq diff-font-lock-prettify t
        diff-font-lock-syntax nil))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode +1))

(use-package transient
  :disabled t
  :ensure t
  :defer 10
  :config
  (use-package transient-defs :load-path "etc/"))

(use-package magit
  :ensure t
  :defer t
  :preface
  (defun magit-change-repository (directory)
    "Change to git repository in DIRECTORY."
    (interactive "D")
    (let ((default-directory directory))
      (call-interactively #'magit-status)))

  :bind ( :map magit-mode-map
          ("C-c C-r" . magit-change-repository)
          :map ctl-period-map
          ("C-x" . magit-status) )

  :init
  (setq magit-define-global-key-bindings nil)

  :config
  (magit-auto-revert-mode -1)

  (setq magit-completing-read-function completing-read-function
        ;; Showing diffs during commits is currently slow.
        magit-commit-show-diff nil
        ;; More granular diffs for all visible hunks
        magit-diff-refine-hunk 'all)

  (add-hook 'git-commit-mode-hook 'turn-on-flyspell))

(use-package magit-annex :disabled t :ensure t :after magit-mode)

(use-package add-log
  :defer t
  :init
  (setq change-log-default-name "CHANGELOG.md"))

(use-package git-timemachine
  :ensure t
  :preface
  (defun git-timemachine-show-diff ()
    "Display diff of changes introduced by current commit."
    (interactive)
    (vc-version-diff (list git-timemachine-file)
                     (car git-timemachine-revision)
                     (car (git-timemachine--next-revision
                           (git-timemachine--revisions)))))
  :config
  (bind-key "D" #'git-timemachine-show-diff git-timemachine-mode-map))


(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

;; TRAMP-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package password-cache
  :init
  (setq password-cache t
        password-cache-expiry (* 12 3600)))

(use-package tramp
  :defer t
  :init
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name "var/tramp"
                                                      user-emacs-directory))
  :config
  ;; Make backups for tramp files in their original locations
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  ;; Honor remote PATH variable
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Assume directories are changed only through tramp in a session
  (setq tramp-completion-reread-directory-timeout nil)

  ;; Use control master options for ssh_config file
  (setq tramp-use-ssh-controlmaster-options nil)

  ;; Cache file attributes for remote files for 5 minutes.
  (setq remote-file-name-inhibit-cache 300)
  ;; Make `vc-mode' ignore remote files
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ;; https://lists.gnu.org/archive/html/emacs-devel/2018-11/msg00326.html
  ;; (setq tramp-methods
  ;;       (-filter (lambda (m)
  ;;                  (not (member (car m) (list "sudo" "su"))))
  ;;                tramp-methods))
  )

;;; BBDB
;; ──────────────────────────────────────────────────────────────────
(use-package bbdb
  :ensure t
  :defer t
  :preface
  (defun bbdb/contacts-filter (addr)
    "Filter out ADDR if no need to save in BBDB."
    (cond
     ((null addr) addr)
     ((string-match-p ".*no.*reply.*@.*" addr) nil)
     (t addr)))

  :init
  (setq bbdb-file (expand-file-name "bbdb" emacs-assets-directory))

  :config
  (setq bbdb-silent t)

  (use-package bbdb-mua
    :defer 10
    :config
    (setq bbdb-mua-auto-update-p 'update
          bbdb-mua-pop-up nil)
    (bbdb-mua-auto-update-init 'gnus 'message))

  (setq bbdb-add-aka t
        bbdb-add-name t
        bbdb-add-mails t
        bbdb-new-mails-primary t
        bbdb-canonicalize-mail-function #'bbdb/contacts-filter
        bbdb-complete-mail-allow-cycling t)

  (add-hook 'bbdb-after-change-hook
            (lambda (&rest _)
              (bbdb-save nil nil))))

;;; NEWS
;; ──────────────────────────────────────────────────────────────────

(use-package elfeed
  :ensure t
  :bind ("C-x w e" . elfeed)
  :hook ((elfeed-mode . display-line-numbers-mode)
         (elfeed-mode . hl-line-mode))
  :preface
  (defun elfeed-show-entry-switch (buffer)
    "Function to switch to elfeed show BUFFER."
    (switch-to-buffer buffer)
    (center-text-for-reading))
  :config
  (setq elfeed-show-entry-switch #'elfeed-show-entry-switch)
  (load-file (locate-user-emacs-file "etc/elfeed-config.el")))

;;; EMAIL
;; ──────────────────────────────────────────────────────────────────
(use-package gnus
  :defer t
  :hook ((gnus-article-mode . goto-address-mode))
  :init
  (setd gnus-init-file "etc/gnus-config.el"
        gnus-home-directory "~/miscellany/gnus"
        gnus-startup-file "~/miscellany/gnus/.newsrc.eld"
        gnus-directory "~/miscellany/gnus/News"
        gnus-kill-files-directory gnus-directory)

  (hook-into-modes #'hl-line-mode 'gnus-summary-mode 'gnus-group-mode)

  ;; So that I have SMTP settings even if Gnus config hasn't been loaded yet.
  (add-hook 'message-mode-hook
            (lambda ()
              (load-file gnus-init-file)
              (footnote-mode +1))))

;;; DOCUMENT VIEWING
;; ──────────────────────────────────────────────────────────────────
(use-package pdf-tools
  :doc "The images produced by this mode are crisper and clearer
  compared to those by `docview-mode'."
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . pdf-tools-enable-minor-modes)
  :bind (:map pdf-view-mode-map
              ("j" . image-next-line)
              ("k" . image-previous-line)
              ("J" . pdf-view-next-line-or-next-page)
              ("K" . pdf-view-previous-line-or-previous-page))
  :config
  (eval-after-load 'pdf-view
    '(progn
       (define-key pdf-view-mode-map (kbd "<") #'image-scroll-down)
       (define-key pdf-view-mode-map (kbd ">") #'image-scroll-up)
       (define-key pdf-view-mode-map (kbd "n") #'--pdf-view-next-page-top-edge)))

  (defvar --pdf-last-page-number nil)
  (add-hook 'pdf-view-after-change-page-hook #'display-pdf-page-number)
  (add-hook 'pdf-view-before-change-page-hook
            (lambda () (setq --pdf-last-page-number (pdf-view-current-page))))


  :init
  ;; `abbreviate-file-name' doesn't handle `nil' values. For buffer that do not
  ;; have associated files, this fails. I had been facing this while opening PDF
  ;; files in Gnus.
  (advice-add 'abbreviate-file-name
              :around (lambda (orig-f file-name)
                        (if file-name
                            (funcall orig-f file-name)
                          "<buffer-with-no-file>")))

  :preface
  (defun --pdf-view-next-page-top-edge ()
    (interactive)
    (pdf-view-next-page)
    (image-scroll-down))

  (defun display-pdf-page-number ()
    (let* ((message-width 20)
           (tooltip-y (/ (frame-pixel-height) 2))
           (tooltip-x (- (frame-pixel-width)
                         (string-pixel-width (make-string message-width ? ))))
           (tooltip-hide-delay 2)
           (tooltip-frame-parameters
            (cons (cons 'left tooltip-x)
                  (cons (cons 'top tooltip-y) tooltip-frame-parameters))))
      (tooltip-show (format "Page: %s -> %s"
                            --pdf-last-page-number
                            (pdf-view-current-page))
                    nil 'tooltip))))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :config
  (setd pdf-view-restore-filename "var/pdf-view-restore.el")
  (add-hook 'pdf-view-mode-hook #'pdf-view-restore-mode))

;;; ERC
;;  ─────────────────────────────────────────────────────────────────
(use-package erc-config :commands erc-start! :load-path "etc/" :defer 10)

;;; EMACS-SERVER
;;  ─────────────────────────────────────────────────────────────────
(use-package server
  :demand t
  :config
  (unless (server-running-p)
    (server-start))

  (pcase system-type
    (`windows-nt
     ;; Default directory on windows isn't ~/
     (setq default-directory (expand-file-name "~/"))
     (setq interprogram-paste-function 'x-selection-value)
     ;; for some reason, selection highlight isn't turned on by default
     (transient-mark-mode t))
    (`darwin
     ;; Modify the CMD key to be Meta key
     (setq mac-command-modifier 'meta)
     ;; This is very stupid of Apple keyboards
     (setq mac-right-option-modifier 'ctrl)
     ;; I don't need a fn
     (setq mac-function-modifier 'ctrl)
     (when (< emacs-major-version 25)
       (setq visible-bell nil)))
    (`gnu/linux
     (midnight-delay-set 'midnight-delay -7200)
     (midnight-mode +1)
     ;; Start Redshift if it's not already running on the system.
     (unless (seq-find (lambda (pid)
                         (when-let ((args (assoc-default 'args (process-attributes pid))))
                           (string-match "^redshift .*" args)))
                       (list-system-processes))
       ;; Redshift doesn't work on Wayland.
       ;; =================================
       ;; (async-shell-command (format "nohup redshift -l %s:%s > /tmp/redshift.log"
       ;;                              (number-to-string calendar-latitude)
       ;;                              (number-to-string calendar-longitude)))
       ))))

;;; ──────────────────────────────────────────────────────────────────
(use-package highlight :ensure t :defer t)
(use-package tldr
  :load-path "packages/lisp/"
  :preface
  (defun take-line-to-eshell ()
    "Move current line to `eshell'."
    (interactive)
    (let ((text (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position))))
      (unless (buffer-live-p (and (boundp 'eshell-buffer-name)
                                  eshell-buffer-name))
        (split-window-sensibly)
        (eshell))
      (pop-to-buffer eshell-buffer-name)
      (require 's)
      (insert (s-trim text))))

  :bind (("C-h t" . tldr))

  :init
  ;; There should be no trailing / here!
  (setd tldr-directory-path "var/tldr")
  (add-hook 'tldr-mode-hook #'utils-easy-move-mode)
  (add-hook 'tldr-mode-hook 'hl-line-mode)

  :config
  (bind-keys :map tldr-mode-map
             ("e" . take-line-to-eshell)
             ("c" . cheat-sh)))

(use-package cheat-sh
  :doc "Open cheat-sh if `tldr' isn't "
  :bind (("C-h c" . cheat-sh)
         ;; `cheat-sh' shows information in a `help-mode' buffer.
         :map help-mode-map
         ("e" . take-line-to-eshell))
  :after tldr
  :ensure t)

(use-package ace-link
  :ensure t
  :bind ("C-c M-o" . ace-link-addr)
  :config
  (ace-link-setup-default))

(use-package know-your-http-well
  :ensure t
  :commands (http-header
             http-method
             http-relation
             http-status-code
             media-type))

;;; UTILITIES
;; ──────────────────────────────────────────────────────────────────

(use-package chart
  :preface
  (defun chart-numbers-on-line (&optional arg)
    (interactive "P")
    (require 'chart)
    (save-excursion
      (let (numbers)
        (beginning-of-line)
        (while (re-search-forward "-?[0-9,]+\\.?[0-9]*" (point-at-eol) t)
          (push (string-to-number (string-replace "," "" (match-string 0)))
                numbers))
        (setq numbers (reverse numbers))
        (let* ((current-month (decoded-time-month (decode-time (current-time))))
               (names-list (if arg
                               (mapcar #'number-to-string
                                       (number-sequence 1 (length numbers)))
                             (mapcar (lambda (n) (calendar-month-name n t))
                                     (reverse (-take (length numbers)
                                                     (-concat (number-sequence current-month 1 -1)
                                                              (-cycle (number-sequence 12 1 -1))))))))
               (chart-face-color-list (list "antiquewhite")))
          (require 'calc-ext)
          (setq var-q0
                (seq-concatenate 'list
                                 (list 'vec)
                                 (mapcar (lambda (n)
                                           (math-read-expr (number-to-string n)))
                                         numbers)))
          (momentary-string-display
           (save-window-excursion
             (let* ((inhibit-read-only t)
                    (result (progn (chart-bar-quickie 'vertical "Plot"
                                                      names-list "X"
                                                      numbers "Y")
                                   (add-face-text-property (point-min) (point-max) '(:height 0.8))
                                   (buffer-substring (point-min) (point-max)))))
               (kill-buffer)
               result))
           (point)
           ?q))))))

(use-package gnuplot-mode
  :ensure t
  :mode ("\\.\\(gp\\|gnuplot\\)$"))

(use-package calc
  :bind ("C-z" . calc-dispatch)
  :init
  (setq calc-settings-file (expand-file-name "calc.el" user-emacs-directory))

  :config
  ;; http://www.gnuplotting.org/plotting-functions/
  (setq calc-gnuplot-name "gnuplot-wx")
  (setq calc-gnuplot-default-device "wxt size 350,262 enhanced font 'Verdana,10' persist")

  (require 'calc-ext)                   ; Modifies the bindings below.
  (define-key calc-mode-map [M-return] #'calc-last-args)
  (define-key calc-mode-map "gP" #'calc-graph-plot)

  (advice-add 'calc :around (lambda (original-calc &rest args)
                              (let ((inhibit-message t))
                                (apply original-calc args))))

  ;; This is a bug in `calc-graph-add-curve' that causes it to treat
  ;; `nil' as a number.
  (require 'calc-misc)
  (advice-add 'calc-graph-view-commands :after #'calc-enable-gnuplot-mode)

  :preface
  (defun calc-enable-gnuplot-mode (&rest _args)
    (with-current-buffer calc-gnuplot-input
      (unless (and (boundp 'gnuplot-mode)
                   (not gnuplot-mode))
        (gnuplot-mode))))


  (defun calc-store-numbers-on-line ()
    (interactive)
    (require 'calc-ext)
    (let ((numbers '(vec)))
      (save-excursion
        (while (re-search-forward "-?[0-9,]+\\.?[0-9]*" (point-at-eol) t)
          (push (math-read-expr (string-replace "," "" (match-string 0)))
                numbers)))
      (setq var-q0 (reverse numbers))
      (calc)
      (calc-recall 'var-q0))))

(use-package mule-cmds
  :bind (("C-x \\" . set-input-method)
         ("C-\\"   . toggle-input-method))
  :init
  (setq default-input-method "german-prefix"))


;;; Network Security Manager
;;; -----------------------------------------------------------------

(use-package nsm
  :init
  (setq network-security-level 'paranoid
        nsm-save-host-names t))

;;; -----------------------------------------------------------------

(use-package ipcalc        :ensure t :commands ipcalc)

(use-package memory-report
  :doc "Built-in package to print information about Emacs memory usage."
  :defines memory-report)

(use-package backlight
  :load-path "packages/lisp"
  :commands backlight)

(use-package proced
  :bind (
         :map ctl-quote-map
         ("s t" . proced)
         :map proced-mode-map
         ("w"   . delete-other-windows)
         ("q"   . kill-buffer-and-window))

  :config
  (setq-default proced-auto-update-flag t)

  (define-key proced-mode-map
    [remap proced-toggle-tree] #'proced-toggle-marks)
  (define-key proced-mode-map
    [remap proced-toggle-marks] #'proced-toggle-tree))

(use-package forecast
  :load-path "packages/lisp"
  :bind (:map ctl-quote-map
              ("c w" . forecast))
  :config
  (add-hook 'forecast-mode-hook (lambda () (text-scale-decrease 1))))

(use-package gif-screencast
  :defer t
  :doc
  "A better alternative to my `start-recording-window' command."
  :load-path "packages/lisp")

(use-package command-log-mode
  :disabled t
  :doc "Useful to displaying currently executing Emacs commands."
  :commands (clm/open-command-log-buffer global-company-mode)
  :load-path "packages/lisp"
  :preface
  (defun show-emacs-commands ()
    (interactive)
    (clm/open-command-log-buffer)
    (global-command-log-mode +1)))

(use-package keycast
  :doc
  "This package provides three modes that display the current command and
   its key or mouse binding.  `keycast-mode' shows the current binding in
   the mode-line while `keycast-tab-bar-mode' displays it in the tab-bar.
   `keycast-log-mode' displays a list of recent bindings in a dedicated
   frame."
  :ensure t)

(provide 'init)
;;; init.el ends here
