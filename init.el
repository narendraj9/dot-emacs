;;; init.el --- narendraj9's Emacs configuration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2016  Narendra Joshi

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
(defvar var-gc-statistics '(vec)
  "A (`calc') matrix of gc timestamps and `gc-elapsed'.
It stores cumulative time spent on each GC.  Consecutive
`gc-elapsed' values (as integers representing milliseconds) can
be substracted to get time spent during a specific GC.  Uses
internal representation used by calc to store matrices.  So this
might break in future.")

(add-hook 'post-gc-hook
          (lambda ()
            (setq var-gc-statistics
                  (cons 'vec
                        (cons (list 'vec
                                    (time-convert nil 'integer)
                                    (floor (* 1000 gc-elapsed)))
                              (cdr var-gc-statistics))))))

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 10 1024 1024))))

;; Try to make `.emacs.d` relocatable
(setq user-emacs-directory
      (file-name-directory (or load-file-name
                               "~/.emacs.d/init.el")))

;;; PACKAGE ARCHIVES
;;  ─────────────────────────────────────────────────────────────────
(require 'package)

(eval-and-compile
  (mapc (lambda (archive)
          (add-to-list 'package-archives archive))
        (list (cons "melpa" "https://melpa.org/packages/")
              (cons "melpa-stable" "https://stable.melpa.org/packages/")
              (cons "org"  "https://orgmode.org/elpa/")))

  (setq package-enable-at-startup nil
        package-user-dir (expand-file-name "packages/elpa/"
                                           user-emacs-directory))
  (package-initialize))

;;; USE-PACKAGE
;; ──────────────────────────────────────────────────────────────────
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish :ensure t :demand t)
(use-package bind-key :ensure t)

(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
Currently, the value for this keyword is being ignore.  In the
future, I might want to add its value to name-symbol's
documentation string.

Argument NAME-SYMBOL is the first argument to `use-package' in a declaration.
Argument KEYWORD here is simply :doc.
Argument DOCSTRING is the value supplied for :doc keyword.
Argument REST is the list of rest of the  keywords.
Argument STATE is maintained by `use-package' as it processes symbols."
    (let ((body (use-package-process-keywords name-symbol rest state)))
      body)))

;;; Emacs initialization benchmarking
(use-package benchmark-init
  :doc "Use `benchmark-init/show-durations-tree' after startup."
  :ensure t
  :init
  (benchmark-init/activate)
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;;; Byte-compilation
(setq load-prefer-newer t)
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-save-mode))

;; LIBRARIES
;;  ─────────────────────────────────────────────────────────────────
(use-package ht        :defer t :ensure t)
(use-package fsm       :defer t :ensure t)
(use-package websocket :defer t :ensure t)
(use-package memoize   :defer t :ensure t)
(use-package s         :defer t :ensure t)
(use-package dash      :defer t :ensure t)
(use-package request   :defer t :ensure t)
(use-package mustache  :defer t :ensure t)
(use-package anaphora  :defer t :ensure t)
(use-package ctable    :defer t :ensure t)
(use-package posframe  :ensure t)

(use-package htmlfontify
  :defer t
  :doc "Keeping it here to remember that it exists. This converts
  a buffer into its equivalent HTML page with inlined CSS to
  mimick the original Emacs buffer faces and text properties.")

(use-package tracking
  :doc
  "Track changes in a buffer and get notified.  Call
   `tracking-add-buffer' to start tracking a buffer. Then calling
   `tracking-next-buffer' lets you switch to buffers that you have
   been tracking. Once a buffer is made visible, it is automatically
   removing from `tracking-buffers'."
  :ensure t
  :defer t)

(use-package alert
  :defer t
  :ensure t
  :config
  (setq alert-default-style 'libnotify)

  :config
  (alert-define-style
   'header-line
   :title "Header Line alert."
   :notifier
   (lambda (info)
     (with-current-buffer (plist-get info :buffer)
       (setq header-line-format (plist-get info :message))))
   :remover
   (lambda (info)
     (run-with-timer 5 nil
                     (lambda ()
                       (with-current-buffer (plist-get info :buffer)
                         (setq header-line-format nil)))))))

(use-package async
  :defer t
  :ensure t
  :config
  (use-package async-bytecomp
    :config
    (setq async-byte-compile-log-file
          (expand-file-name "var/async-bytecomp.log" user-emacs-directory))))

(use-package levenshtein
  :defines levenshtein-sort
  :defer t
  :load-path "lib/")

(use-package frecency
  :doc
  "Record scores for times based on frequency and recency.
   https://slack.engineering/a-faster-smarter-quick-switcher-77cbc193cb60"
  :defer t
  :ensure t)

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


  (bind-keys* :prefix "C-'"   :prefix-map ctl-quote-map)
  (bind-keys* :prefix "C-."   :prefix-map ctl-period-map)
  (bind-keys* :prefix "C-;"   :prefix-map ctl-semicolon-map)
  (bind-keys* :prefix "C-h x" :prefix-map ctl-h-x-map)

  :bind (("C-c m" . switch-to-minibuffer)
         ("C-c 0" . quick-switch-themes)
         ("<print>" . snap-it)
         :map ctl-quote-map
         ("c g" . google-it)
         ("c !" . insert-date-time-at-point)
         ("c e" . vicarie/eval-print-last-sexp)
         ("c =" . vicarie/eval-replace-last-sexp)
         ("c r" . rename-file-and-buffer)
         ("C-a" . emacspeak-wizards-execute-asynchronously)
         ("M-x" . async-M-x)
         :map ctl-period-map
         ("k"   . compile)
         ("K"   . recompile)
         ("$"   . selective-display-beyond-col)
         ("u"   . underline-text)
         ("d"   . duplicate-current-line)
         ("s"   . surround-symbol-with)))

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
  :preface
  (setq emacs-init-end-info ())
  (defun emacs-init-end ()
    "Function to be called at the end of Emacs init process."
    (run-with-timer 1
                    nil
                    (lambda ()
                      ;; Emacs init took (actual-emacs-up-time, uptime-felt-like)
                      (alert (format "Emacs init took (%s, %.2f seconds).\n\n %s"
                                     (emacs-init-time)
                                     (1- (time-to-seconds
                                          (time-subtract (current-time)
                                                         emacs-start-time)))
                                     (mapconcat #'identity
                                                emacs-init-end-info
                                                "\n"))))))
  :hook (after-init . emacs-init-end)
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
        (expand-file-name "var/autosaves/" user-emacs-directory))

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
  (setq ;; initial-scratch-message ""
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
  (add-to-list 'default-frame-alist
               `(font . ,(font-xlfd-name (face-attribute 'default :font))))

  ;; Load secrets if available.
  (when (file-exists-p secrets-file)
    (load secrets-file))

  ;; Misc
  (setq-default tab-width 4)
  (setq-default fill-column 70)

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

  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package appearance
  :doc "`use-package' doesn't throw an error for non-existent packages"
  :load-path "themes/"
  :defines quick-switch-themes
  :preface
  (defun font-availablep (font)
    "Return true if FONT is available on system.
     This is written to avoid calling `find-font' repeatedly."
    (let ((favailablep (intern (concat font "-availablep"))))
      (if (boundp favailablep)
          (symbol-value favailablep)
        (customize-save-variable favailablep
                                 (not (null (find-font (font-spec :name font))))))))

  :init
  (add-to-list 'custom-theme-load-path
               (expand-file-name "themes/"
                                 user-emacs-directory))
  (load-theme 'jazz)
  (use-package mode-line-config :demand t :load-path "etc/")

  ;; Setup my favorite fonts [if available]
  (if (font-availablep "Symbola")
      (set-fontset-font "fontset-default" nil
                        (font-spec :name "Symbola" :size 15)
                        nil 'append)
    (add-to-list 'emacs-init-end-info
                 "! You do not have Symbola font installed."))

  ;; Font for reading news
  (cond
   ((font-availablep "Carlito")
    ;; It would have been great if I could set the background to white
    ;; while reading anything other than code. Emacs doesn't support
    ;; buffer-local themes and doing this would require nasty tricks
    ;; with hooks.
    (set-face-attribute 'variable-pitch nil
                        :family "Carlito"
                        :height 130
                        :weight 'normal
                        :width 'ultraexpanded)))

  ;; Show long lines as continuations.
  (setq-default truncate-lines nil)
  (setq-default cursor-type '(bar . 4)
                cursor-in-non-selected-windows nil)

  ;; Maximize emacs on startup
  (when (window-system)
    (add-to-list 'default-frame-alist
                 '(fullscreen . maximized)))

  ;; Diminish some minor modes
  (diminish 'hi-lock-mode))

(use-package ibuffer
  :bind (
         :map global-map
         ("C-<return>" . other-window)
         :map ctl-x-map
         ("C-b" . ibuffer-other-window))
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

(use-package wtf
  :load-path "packages/lisp/"
  :commands wtf-is)

(use-package emlib
  :load-path "packages/rest/emlib/"
  :defer t)

(use-package livemacs
  :defer t
  :commands livemacs-begin
  :load-path "packages/rest/livemacs/")

(use-package lively
  :commands lively
  :load-path "etc/")

;;; Utilities
;; ──────────────────────────────────────────────────────────────────
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
  :bind (
         :map ctl-quote-map ("s p" . prodigy)
         :map view-mode-map ("o" . delete-other-windows)
         )
  :ensure t
  :config
  (load-file (expand-file-name "etc/prodigy-service-defs.el"
                               user-emacs-directory))

  ;; Load definitions if they are kept in assets directory too
  (let ((extra-service-defs-path (expand-file-name "prodigy-service-defs.el"
                                                   emacs-assets-directory)))
    (when (file-exists-p extra-service-defs-path)
      (load-file extra-service-defs-path))))

(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'pcre))

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

;;; Thanks to https://github.com/Wilfred
(use-package ag :ensure t :bind ("M-s M-a" . ag))
(use-package suggest :defer t :ensure t :commands suggest)
(use-package helpful
  :ensure t
  :bind (("C-h v" . helpful-variable)
         ("C-h f" . helpful-function)
         ("C-h k" . helpful-key)
         ("C-h o" . helpful-symbol)
         :map helpful-mode-map
         ("<tab>" . forward-button)
         ("<backtab>" . backward-button))
  :config
  (use-package elisp-demos
    :ensure t
    :init
    (advice-add 'helpful-update
                :after
                (lambda ()
                  (make-thread #'elisp-demos-advice-helpful-update)))))

(use-package outline-minor-mode
  :defer t
  :init
  (add-hook 'outline-minor-mode-hook
            (lambda ()
              (diminish 'outline-minor-mode))))

(use-package hs-minor-mode
  :defer t
  :init
  (add-hook 'hs-minor-mode-hook
            (lambda ()
              (diminish 'hs-minor-mode))))

(use-package fringe
  :config
  (defvar default-fringe-style (cons (* 1 (frame-char-width)) (frame-char-width)))
  (fringe-mode default-fringe-style))

(use-package tool-bar   :config (tool-bar-mode -1))
(use-package scroll-bar :config (scroll-bar-mode -1))

(use-package feebleline
  :ensure t
  :bind (("C-x w f" . feebleline-mode))
  :hook (feebleline-mode . window-divider-toggle)
  :preface
  (defun window-divider-toggle ()
    (window-divider-mode (if feebleline-mode +1 -1))))

(use-package menu-bar
  :doc "The menu bar is useful for discovering features that
   exist in some modes, e.g Gnus, SQLi."
  :bind (:map ctl-x-map
              ("w m" . menu-bar-open))
  :config (menu-bar-mode +1))


;; KEY BINDINGS
;; ──────────────────────────────────────────────────────────────────

(ffap-bindings)
(bind-keys :map ctl-period-map
           ("C-o" . goto-address-at-point)
           ("C-f" . ffap))

;; ──────────────────────────────────────────────────────────────────
(use-package minibuffer
  :config
  (setq enable-recursive-minibuffers t
        history-delete-duplicates t
        history-length 1000)
  (minibuffer-depth-indicate-mode +1))

(use-package tab-bar
  :doc
  "This built-in package provides a way to keep a set of window
   configurations around that can be switched to easily."
  :config
  (tab-bar-history-mode +1)
  (setq tab-bar-show nil
        tab-bar-tab-name-function #'tab-bar-tab-name-all))

(use-package minibuffer-command-history
  :load-path "etc/"
  :config
  (minibuffer-command-history-enable))

(use-package calendar
  :defer t
  :bind (:map ctl-quote-map
              ("c c" . calendar))
  :init
  (setq diary-file (expand-file-name "diary" emacs-assets-directory)
        ;; Weeks start on Monday.
        calendar-week-start-day 1)
  (add-to-list 'display-buffer-alist '("\\*Calendar\\*" display-buffer-at-bottom)))

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
    (alert (format "In %s minutes: %s" (first args) (third args)) :title "Reminder")
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

(use-package browse-url
  :defer t
  :doc "Make chromium the default browser if it is installed."
  :init
  (cond
   ((executable-find "chromium")
    (setq browse-url-browser-function 'browse-url-chromium))
   ((executable-find "google-chrome")
    (setq browse-url-browser-function 'browse-url-chrome))
   ((executable-find "firefox")
    (setq browse-url-browser-function 'browse-url-firefox)))
  :config
  (advice-add 'browse-url-chromium
              :before
              (lambda (url &optional _window)
                (message "Opening with Chromium: %s" url))))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package rainbow-mode :ensure t :defer t)
(use-package color-identifiers-mode
  :doc
  "The default colors aren't as loud as `rainbow-identifiers-mode'."
  :ensure t
  :defer t)

(use-package uniquify
  :doc "Unique buffer names"
  :diminish t
  :init
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator " • "))

;;; TEXT-EDITING, FOLDING and NAVIGATION
;; ─────────────────────────────────────────────────────────────────
(use-package undo-tree
  :doc
  "evil-mode turns on `global-undo-tree-mode'. I don't want it to
  override default Emacs undo bindings so this `use-package'
  declaration must be placed before that of `evil-mode'."
  :ensure t
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  :init
  (setq undo-tree-map (make-sparse-keymap))

  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-relative-timestamps t))

(use-package evil
  :ensure t
  :bind (:map evil-motion-state-map
              ("SPC" . counsel-M-x))
  :init
  (setq evil-toggle-key "C-; C-;"
        evil-default-state 'emacs
        evil-insert-state-modes (list)
        evil-motion-state-modes (list)
        evil-default-cursor (list cursor-type))
  (evil-mode +1))

(use-package typo
  :doc
  "Guesses the correct quotation marks"
  :ensure t
  :defer 20
  :commands typo-global-mode
  :config (typo-global-mode))

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

  (wrap-region-global-mode +1))

(use-package wgrep :defer t :ensure t)
(use-package region-bindings-mode
  :diminish region-bindings-mode
  :ensure t
  :config
  (region-bindings-mode-enable))

(use-package hydra
  :ensure t
  :demand t
  :commands defhydra
  :config
  ;; (setq hydra-hint-display-type 'posframe)
  )

(use-package select :init (setq select-enable-clipboard t))
(use-package simple
  :doc "The great simple.el"
  :demand t
  :diminish auto-fill-function
  :bind (("M-q" . fill-or-unfill)
         ("M-[" . backward-delete-word)
         :map ctl-period-map
         ("C-u" . repeated-delete-indentation)
         :map ctl-quote-map
         ("s >" . shell-command-on-region)
         ("s |" . shell-command-on-region)
         ("s s" . shell-command)
         ("s !" . shell-command)
         ("s a" . async-shell-command)
         ("s &" . async-shell-command)
         ("s ." . shell-command-from-region))
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
  ;; Multiple-cursors changes transient-mark-mode to (only only .. t),
  ;; if shift-select-mode is enabled.
  (setq shift-select-mode nil)

  ;; Make text copied/cut from outside Emacs part of Emacs kill-ring on first
  ;; kill inside Emacs.
  (setq kill-ring-max 1000
        save-interprogram-paste-before-kill t
        kill-do-not-save-duplicates t)

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
    #'exchange-point-and-mark*))

(use-package ialign
  :doc "Very useful to get quick feedback for alignment with
  `align.el'."
  :ensure t
  :bind (:map ctl-period-map
              ("C-a" . ialign)))

(use-package symbol-overlay
  :ensure t
  :bind (
         :map global-map
         ("M-n" . symbol-overlay-put*)
         ("M-p" . symbol-overlay-put*)

         :map symbol-overlay-map
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev))
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
          (pulse-delay 0.01))
      (pulse-momentary-highlight-region (car bounds)
                                        (cdr bounds))
      (symbol-overlay-remove-all-timer (current-buffer))))

  :init
  (advice-add 'symbol-overlay-jump-call :after #'flash-current-symbol))

(use-package crux
  :ensure t
  :bind (("C-<backspace>" . crux-kill-line-backwards)
         ("S-<return>"    . crux-switch-to-previous-buffer))
  :hook (after-init . crux-reopen-as-root-mode))

(use-package misc
  :doc "Where simple ends, maybe misc.el begins"
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . copy-from-above-command)))

(use-package picture :defer t)

(use-package savehist
  :demand t
  :config
  (setq savehist-file (expand-file-name "var/savehist.el"
                                        user-emacs-directory))
  (setq savehist-additional-variables '(kill-ring
                                        limit-usage
                                        minibuffer-history
                                        minibuffer-command-history
                                        ;; ---
                                        command-history
                                        ;; ---
                                        define-word-archives
                                        define-word-list
                                        ;; ---
                                        ivy-views))
  (savehist-mode +1))

(use-package vcursor
  :doc
  "Adds a virtual cursor for editing/copying text."
  :preface
  (defun vcursor-avy ()
    "Move with `avy-goto-char-timer' and add a virtual cursor"
    (interactive)
    (require 'vcursor)
    (save-excursion
      (avy-goto-char-timer)
      (vcursor-move (point))))

  :config
  (defhydra hydra-vcursor-avy (ctl-period-map "C-v")
    "vcursor"
    ("j"   vcursor-avy       "avy-set-cursor")
    ("C-w" vcursor-copy-word "copy-word")
    ("q"   vcursor-disable   "quit"))

  ;; This is a workaround for a bug that exists in vcursor. Copying a word
  ;; causes point to move to virtual cursors position.
  (advice-add 'vcursor-get-char-count
              :around
              (lambda (original-fn &rest args)
                (save-excursion
                  (apply original-fn args)))))

(use-package iedit
  :disabled t
  :ensure t
  :diminish iedit-mode
  :bind (:map ctl-quote-map ("C-;" . iedit-mode)))

(use-package beacon
  :ensure t
  :bind (:map ctl-quote-map ("c p" . beacon-blink)))

(use-package multiple-cursors
  :doc "A minor mode for editing with multiple cursors."
  :ensure t
  :init
  (setd mc/list-file "var/mc-lists.el")
  (add-hook 'multiple-cursors-mode-hook
            (lambda ()
              (if multiple-cursors-mode
                  (progn (set-face-attribute 'fringe nil :inverse-video t)
                         (fringe-mode 50))
                (set-face-attribute 'fringe nil :inverse-video nil)
                (fringe-mode default-fringe-style))))

  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c >" . mc/mark-all-like-this)

   ;; Use the plain yank-pop when multiple cursors are
   ;; active. `counsel-yank-pop' doesn't work as expected.
   :map mc/keymap
   ("M-y" . yank-pop)

   :map region-bindings-mode-map
   ("C-x $" . mc-hide-unmatched-lines-mode)
   ("C-x !" . mc/mark-all-like-this)
   ("M-k"   . mc/mmlte--up)
   ("M-j"   . mc/mmlte--down)
   ("M-h"   . mc/mmlte--left)
   ("M-l"   . mc/mmlte--right)

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

(use-package bicycle
  :ensure t
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab]   . bicycle-cycle)
              ([backtab] . bicycle-cycle-global)
              ([S-tab]   . bicycle-cycle-global)))


(use-package goto-last-change
  :ensure t
  :bind ("C-x C-SPC" . repeatable-goto-last-change)
  :preface
  (defun repeatable-goto-last-change ()
    (interactive)
    (repeat-command #'goto-last-change)))

(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package boxquote
  :doc "For nice boxes for quoting text."
  :commands boxquote-region
  :ensure t)

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
              ("C-f d" . footnote-delete-footnote)
              ("C-f g" . footnote-goto-footnote)))



(use-package markdown-mode :defer t :ensure t)
(use-package csv-mode      :defer t :ensure t)

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

(use-package olivetti
  :doc "Similar to `writeroom-mode' but a little less hard-core."
  :ensure t
  :diminish olivetti-mode
  :hook (nov-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 80))

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

(use-package discover
  :doc "Discovering more about Emacs"
  :ensure t
  :hook (dired-mode . dired-turn-on-discover))

(use-package which-key
  :doc "Get quick emacs key binding suggestions"
  :ensure t
  :demand 2
  :diminish which-key-mode
  :config
  (setq which-key-max-description-length nil)
  (which-key-mode +1))

(use-package project
  :init
  (setq project-list-file
        (expand-file-name "var/project-list" user-emacs-directory)))

(use-package projectile
  :doc
  "All of the projectile provided functionality that I use
  is already provided by the built-in `project.el'
  "
  :disabled t
  :ensure t
  :defer 10
  :diminish projectile-mode
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p)
  :bind-keymap ("C-c p" . projectile-command-map)
  :preface
  (defun projectile-get-project-root ()
    "Return root directory path for project or ask for project."
    (let ((projectile-switch-project-action (lambda ())))
      (or (projectile-project-p)        ; return path if in a project
          (projectile-switch-project))))

  (defun projectile-find-file! ()
    "Switch to a project (if required) before `projectile-find-file'."
    (interactive)
    (let ((projectile-require-project-root nil)
          (default-directory (projectile-get-project-root)))
      (call-interactively #'projectile-find-file)))

  :init
  (setd projectile-known-projects-file "var/projectile-projects.eld"
        projectile-cache-file "var/projectile.cache")
  (setq projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s ")
  (setq projectile-completion-system 'ivy)

  :config
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)

  (mapc (lambda (d) (push d projectile-globally-ignored-directories))
        '("vendor/bundle"))
  (mapc (lambda (f) (push f projectile-globally-ignored-files))
        '("GTAGS" "GRTAGS" "GPATH"))

  (setq projectile-switch-project-action
        ;; Open project's root directory in `dired' after switching to
        ;; project.
        (lambda () (dired default-directory)))

  (projectile-mode +1))

;;; SESSIONS and BOOKMARKS
;; ──────────────────────────────────────────────────────────────────
(use-package bookmark
  :defer 5
  :config
  (setq bookmark-save-flag 1
        bookmark-default-file (expand-file-name "~/miscellany/assets/bookmarks.el")))

(use-package saveplace
  :init
  (save-place-mode +1)
  :config
  (setq save-place-file
        (locate-user-emacs-file "var/saved-places")))

;;; Buffers, Windows and Frame
;; ――――――――――――――――――――――――――――――――――――――――

(use-package winner
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode +1)

  (defhydra hydra-winner (global-map "C-x w")
    "winner> "
    ("/"  winner-undo "undo")
    ("\\"  winner-redo "redo")
    ("q"  nil "quit")))

(use-package exwm
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
  :bind (:map goto-map
              ("TAB" . mwim-repeating))
  :ensure t
  :preface
  (defun mwim-repeating ()
    "Repeating form of `mwim'."
    (interactive)
    (repeat-command 'mwim)))

(use-package isearch
  :doc "Search for the string in the active region, if there is any."
  :bind (("C-M-r" . isearch-backward-other-window)
         ("C-M-s" . isearch-forward-other-window)
         :map isearch-mode-map
         ("C-S-W" . isearch-yank-symbol))

  :config
  (advice-add 'isearch-mode
              :after (lambda (&rest _rest)
                       (when (region-active-p)
                         (isearch-yank-selection))))

  (setq isearch-lazy-count t
        lazy-highlight-initial-delay 1.0
        lazy-count-prefix-format "(%s/%s) ")

  (defhydra hydra-isearch (:color pink)
    "isearch> "
    ("C-<return>" isearch-exit-other-end    "other end" :exit t)
    ("C-="        isearch-toggle-case-fold  "toggle case")
    ("C-t"        isearch-toggle-regexp     "toggle regexp")
    ("C-^"        isearch-edit-string       "edit string")
    ("C-i"        isearch-complete          "complete"))

  (bind-key "C-o" #'hydra-isearch/body isearch-mode-map)

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
  (advice-add #'split-window-right :filter-return #'select-window)

  (defhydra hydra-next-prev-buffer (global-map "C-c")
    "buffer> "
    (">" echoing-next-buffer "next")
    ("<" echoing-previous-buffer "previous"))

  (defhydra hydra-enlarge-window (global-map "C-x" :timeout 1.0)
    "window>"
    ("|" fit-window-to-buffer "fit-to-buffer")
    ("^" enlarge-window "enlarge-vertically")
    ("{" enlarge-window-horizontally "enlarge-horizontally")
    ("}" shrink-window-horizontally "shrink-horizontally")))

(use-package ace-window
  :doc
  "This should come after `window's use-package
   declaration. Otherwise, `window' would overwrite the binding for \\[ace-window]]."
  :ensure t
  :doc "Use `ace-window' instead of `other-window'."
  :bind ("C-x o" . ace-window))

;; ――――――――――――――――――――――――――――――――――――――――
(use-package recentf
  :defer t
  :config
  (setq recentf-auto-cleanup 'never
        recentf-keep '(file-remote-p file-readable-p)
        recentf-exclude '("\.gpg$")
        recentf-max-saved-items 1000
        recentf-save-file (expand-file-name "var/recentf"
                                            user-emacs-directory))
  (recentf-mode +1))


;; ――――――――――――――――――――――――――――――――――――――――
(use-package flyspell
  :diminish flyspell-mode
  :bind (:map ctl-period-map
              ("!" . flyspell-buffer))
  :preface
  (defun enable-flyspell ()
    "Unbind C-. from `flyspell-mode-map'."
    (flyspell-mode +1)
    (unbind-key "C-." flyspell-mode-map))
  :hook (((markdown-mode latex-mode TeX-mode message-mode)
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
(use-package pcache
  :defer t
  :ensure t
  :init
  (setd pcache-directory "var/pcache/"))

(use-package popup :defer t :ensure t)

(use-package wordly
  :doc "Lookup word definition and synonyms."
  :load-path "packages/rest/wordly/"
  :defer t
  :pin manual
  :bind (:map ctl-quote-map
              ("c l" . wordly-define-word-local)
              ("c D" . wordly-define-word-at-point)
              ("c s" . wordly-show-synonyms-for-word-at-point))
  :preface
  (defun wordly-define-word-local ()
    (interactive)
    (->> (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
           (read-string "Word: "))
         (format "define %s")
         shell-command-to-string
         ansi-color-apply
         display-message-or-buffer)))

(use-package define-word
  :ensure t
  :bind (:map ctl-quote-map
              ("c d" . define-word))
  :commands define-word-review
  :config
  (advice-add 'define-word
              :around
              (lambda (original &rest args)
                (let* ((result (apply original args))
                       (result (if (windowp result)
                                   (with-current-buffer (window-buffer result)
                                     (buffer-string))
                                 result)))
                  (define-word-record-definition (car args) result))))
  (push '(wordnik . display-message-or-buffer) define-word-displayfn-alist)

  :preface
  (defvar define-word-list ()
    "List of words looked up so far.")

  (defvar define-word-archives ()
    "List of words looked up and deleted.")

  (defun define-word-delete-word ()
    (interactive)
    (let* ((phrase (buffer-substring-no-properties (point-at-bol)
                                                   (point-at-eol)))
           (definition (assoc-default phrase define-word-list #'equal))
           (inhibit-read-only t))
      (if (not phrase)
          (message "No word at point.")
        (setq define-word-list (assoc-delete-all phrase define-word-list))
        (unless (assoc-default phrase define-word-archives #'equal)
          (setq define-word-archives
                (cons (cons phrase definition)
                      define-word-archives)))
        (put-text-property (point-at-bol)
                           (point-at-eol)
                           'face '((t (:strike-through t)))))))

  (defun define-word-record-definition (word definition)
    (unless (assoc word define-word-list #'equal)
      (push (cons word definition) define-word-list)))

  (defun define-word-archives ()
    (interactive)
    (let ((define-word-list define-word-archives))
      (define-word-review)))

  (defun define-word-review ()
    "Go through words that I looked up in the past."
    (interactive)
    (with-current-buffer (get-buffer-create "Review Words")
      (local-set-key (kbd "C-k") #'define-word-delete-word)
      (local-set-key (kbd "g")  #'define-word-review)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (word-def define-word-list)
          (let* ((word (car word-def))
                 (definition (format "\n%s\n" (cdr word-def))))
            (insert-button word
                           'face 'widget-button
                           'action
                           (lambda (&rest args)
                             (momentary-string-display definition
                                                       (point-at-eol)
                                                       ?q))))
          (insert "\n"))
        (beginning-of-buffer))
      (help-mode)
      (display-line-numbers-mode +1)
      (switch-to-buffer (current-buffer)))))

;;; Completion at Point
;; ――――――――――――――――――――――――――――――――――――――――
(use-package company
  :ensure t
  :hook (emacs-init . global-company-mode)
  :diminish company-mode
  :config
  (define-key company-mode-map [remap indent-for-tab-command]
    #'company-indent-or-complete-common)

  (bind-keys :map company-active-map
             ("<tab>" . company-complete-common-or-cycle)
             ("C-n"   . company-select-next-or-abort)
             ("C-p"   . company-select-previous-or-abort))

  (setq company-idle-delay 2.0
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-offset-display 'lines)

  :preface
  (defun company-mode-quicker ()
    "Makes `company-mode' show completions quickly in a buffer."
    (make-local-variable 'company-idle-delay)
    (setq-local company-idle-delay 0.1)
    (company-mode +1)))

(use-package company-statistics
  :doc "Sort `company-mode' completions by frequency."
  :ensure t
  :hook (after-init . company-statistics-mode)
  :init
  (setd company-statistics-file "var/company-statistics-cache.el"))

;; ──────────────────────────────────────────────────────────────────

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g ." . dumb-jump-go)
         ("M-g ," . dumb-jump-back))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package beginend
  :ensure t
  :defer 5
  :diminish beginend-global-mode
  :config
  (beginend-global-mode +1)
  (mapc (lambda (hook-mode-cons)
          (diminish (cdr hook-mode-cons)))
        beginend-modes))

(use-package dired-x
  :after dired
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("M-<"   . dired-to-first-entry)
         ("M->"   . dired-to-last-entry)
         ("a"     . counsel-ag)
         ("r"     . counsel-rg)
         ("z"     . kill-buffer-delete-window)
         ("j"     . ido-find-file)
         ("f"     . project-find-file)
         ("~"     . dired-go-home)
         ("C-c u" . dired-up-repeatedly))
  :hook ((dired-after-readin . dired-hide-details-mode)
         (dired-mode         . hl-line-mode)
         (dired-mode         . dired-omit-mode))
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-hide-details-hide-information-lines nil)

  :config
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

  (defun dired-up-repeatedly ()
    "Go up repeatedly in a Dired buffer."
    (interactive)
    (repeat-command 'dired-up-directory #'message))

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
                (setq dired-recent-directories
                      (->> (and (boundp 'projectile-known-projects)
                                projectile-known-projects)
                           (-concat dired-recent-directories)
                           -distinct
                           (-filter #'stringp)
                           (-filter #'f-directory?))))))

;;; SNIPPETS and ABBREVS
;; ――――――――――――――――――――――――――――――――――――――――
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
  (defun ivy-yankpad-set-category (_)
    (setq yankpad-category
          (ivy-read "Category: "
                    (yankpad--categories)
                    :require-match t)))

  (defun yankpad-set-category-and-insert ()
    (interactive)
    (yankpad-set-category)
    (yankpad-insert))

  :config
  (setd yankpad-file "etc/yankpad.org")

  ;; Redefine `yankpad-insert-from-current-category' with help from
  ;; https://github.com/abo-abo/swiper/issues/1736#issuecomment-419730497
  (defun yankpad-insert-from-current-category (&optional name)
    "Insert snippet NAME from `yankpad-category'.  Prompts for NAME unless set.
     Does not change `yankpad-category'."
    (ivy-read "Snippet: " (yankpad-active-snippets)
              :action (lambda (x)
                        (let* ((name (car x))
                               (snippet (assoc name (yankpad-active-snippets))))
                          (if snippet
                              (yankpad--run-snippet snippet)
                            (message (concat "No snippet named " name))
                            nil)))
              :caller 'yankpad-insert))

  (ivy-set-actions 'yankpad-insert
                   '(("S"  ivy-yankpad-set-category "Switch Category"))))

(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq-default abbrev-mode t
                save-abbrevs nil
                abbrev-file-name (expand-file-name "lib/abbrev_defs"
                                                   user-emacs-directory))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;; Personal Finance
;; ――――――――――――――――――――――――――――――――――――
(use-package ledger-mode
  :ensure t
  :defer t
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
  :bind (("C-c j" . hledger-run-command))
  :config
  (setq hledger-jfile
        (expand-file-name "~/miscellany/personal/finance/accounting.journal"))
  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company)))
  (add-hook
   'hledger-view-mode-hook
   (lambda ()
     (let ((b (current-buffer)))
       (hl-line-mode +1)
       (run-at-time 1
                    nil
                    (lambda ()
                      (when (buffer-live-p b)
                        (with-current-buffer b
                          (center-text-for-reading)
                          (when (equal hledger-last-run-command "balancesheet")
                            (highlight-regexp "^.*credit.*$" 'hledger-warning-face)
                            (highlight-regexp "^.*\\(savings\\|cash\\).*$" nil))))))))))


(use-package hledger-input
  :pin manual
  :load-path "packages/rest/hledger-mode/"
  :bind (("C-c e" . hledger-capture)
         :map hledger-input-mode-map
         ("C-c C-b" . popup-balance-at-point))
  :preface
  (defun popup-balance-at-point ()
    "Show balance for account at point in a popup."
    (interactive)
    (if-let ((account (thing-at-point 'hledger-account)))
        (message (hledger-shell-command-to-string (format " balance -N %s "
                                                          account)))
      (message "No account at point")))
  :hook ((hledger-input-post-commit . hledger-show-new-balances)
         (hledger-input-mode        . auto-fill-mode))
  :config
  (setq hledger-input-buffer-height 20)
  (add-hook 'hledger-input-mode-hook #'company-mode-quicker))

;;; Programming in general
;; ――――――――――――――――――――――――――――――――――――

(use-package type-break
  :bind (:map ctl-quote-map
              ("b" . type-break))
  :init
  (setq type-break-file-name nil)
  (setq type-break-mode-line-message-mode t)

  (type-break-mode +1))


(use-package keyfreq
  :load-path "packages/lisp"
  :config
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

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
  (require 'ansi-color))

(use-package eldoc :diminish eldoc-mode :config (global-eldoc-mode +1))

(use-package which-func
  :doc "Display the current function in the mode line."
  :config
  (setq which-func-modes '(java-mode))
  (setq which-func-unknown "<λ>")
  (which-func-mode +1))

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
         ("M-," . xref-pop-marker-stack)))

(use-package smart-jump
  :ensure t
  :config
  ;; Adds modes that do not provide jump to definition functionality
  ;; themselves.
  (setq smart-jump-default-mode-list '(web-mode))
  (smart-jump-setup-default-registers))

(use-package subword
  :hook (prog-mode . subword-mode)
  :diminish subword-mode)

(use-package flycheck
  :ensure t
  :bind (:map flycheck-command-map
              ("!" . hydra-flycheck/body)
              ("f" . hydra-flycheck/body))
  :bind-keymap ("C-' f" . flycheck-command-map)
  :init
  (setq flycheck-mode-line-prefix ""
        flycheck-global-modes
        '(emacs-lisp-mode clojure-mode clojurescript-mode yaml-mode sh-mode java-mode rust-mode))

  (global-flycheck-mode +1)

  ;; Suggestion from: https://www.flycheck.org/en/latest/user/error-list.html
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 10)))
  :config
  (when (not (executable-find "shellcheck"))
    (message "Not installed on system: `shellcheck'!"))

  (defhydra hydra-flycheck (:pre (flycheck-list-errors))
    "Flycheck"
    ("c"  flycheck-buffer                                           "Run")
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("n"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("p"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil)))

(use-package highlight-indent-guides
  :defer t
  :diminish highlight-indent-guides-mode
  :init
  (hook-into-modes #'highlight-indent-guides-mode
                   'prog-mode-hook)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\x2502))

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :hook ((emacs-lisp-mode clojure-mode racket-mode scheme-mode) . aggressive-indent-mode))

(use-package comint
  :defer t
  :config
  (setq comint-scroll-show-maximum-output nil))

(use-package eshell
  :bind (:map ctl-quote-map ("C-p" . eshell-toggle))
  :preface
  (defun eshell-toggle ()
    (interactive)
    (if (eq major-mode 'eshell-mode)
        (jump-to-register ?e)
      (window-configuration-to-register ?e)
      (eshell)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*e?shell" display-buffer-at-bottom))

  (setq eshell-prompt-function
        (lambda ()
          (concat "(" (file-name-nondirectory (eshell/pwd)) ")"
                  (if (= (user-uid) 0) " # " " $ ")))

        eshell-aliases-file
        (expand-file-name "./etc/eshell-aliases" user-emacs-directory)))

(use-package bpfcc-tools
  :load-path "etc/"
  :commands bpfcc-tools-man-page)


;;; DevOps
;; ──────────────────────────────────────────────────────────────────
(use-package jinja2-mode
  :defer t
  :ensure t)

;;; HASKELL-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package haskell-mode
  :ensure t
  :defer t
  :bind (:map haskell-mode-map
              ("C-c C-k" . haskell-compile))
  :hook ((haskell-mode . haskell-doc-mode)
         (haskell-mode . haskell-indentation-mode)))

(use-package intero
  :doc
  "It works! It just takes a lot of time to install intero. Be
   patient."
  :ensure t
  :defer t
  :hook (haskell-mode . intero-mode))

;;; GRAPHICS
;; ──────────────────────────────────────────────────────────────────
(use-package gnuplot-mode      :defer t :ensure t)
(use-package graphviz-dot-mode :ensure t :defer t)

;;; Notes, Journal and Task Manager
;;  ─────────────────────────────────────────────────────────────────

(use-package calfw :ensure t :defer t)
(use-package org-config
  :load-path "etc/"
  :bind (("C-c c" . org-config-capture)
         ("C-c a" . org-agenda)
         :map ctl-quote-map
         ("C-n" . open-org-file)
         ("C-d" . search-notes-files))
  :init
  (bind-key "C-c a" #'org-agenda)

  (eval-after-load "org"
    '(progn
       (require 'org-config)
       (setq org-default-notes-file
             (expand-file-name "notes.org" org-directory))
       (set-register ?o (cons 'file org-default-notes-file))))

  ;; `org-agenda-get-restriction-and-command' ignores rules for
  ;; displaying buffers (i.e. `display-buffer-alist'). This advice
  ;; tries causes `split-window-sensibly' to always split vertically
  ;; and show the " *Agenda Commands*" buffer below the current buffer.
  (advice-add 'org-agenda-get-restriction-and-command
              :around
              (lambda (original &rest args)
                (let ((split-window-preferred-function #'split-window-below))
                  (apply original args)))))

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


;;; Hyperbole
;;; ──────────────────────────────────────────────────────────────────

(use-package delsel :init (delete-selection-mode +1))
(use-package expand-region
  :ensure t
  :doc
  "Hyperbole provides similar functionality through
  hui-select.el but I have found `expand-region' to be more
  intuitive."
  :bind (:map ctl-period-map
              ("@" . er/expand-region)))

(use-package hyperbole
  :disabled t
  :ensure t
  :init
  (require 'hyperbole)
  :config
  (use-package hyrolo
    :config
    (setq hyrolo-entry-regexp "^\\*+ "
          hyrolo-kill-buffers-after-use t)
    (remove-hook 'hyrolo-add-hook #'hyrolo-set-date)
    (remove-hook 'hyrolo-edit-hook #'hyrolo-set-date)))

;;; ---
(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (setq plantuml-jar-path
        (expand-file-name "~/miscellany/assets/plantuml.jar")))

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
  (setq shr-width 80
        shr-use-fonts nil))

;;; Ivy and Friends
;; ──────────────────────────────────────────────────────────────────

(use-package ivy-posframe
  :doc "Custom positions for ivy buffers."
  :ensure t
  :disabled t
  :config
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . nil)
          (swiper-isearch . nil)
          (counsel-rg . nil)
          (t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 10)))
  (ivy-posframe-mode 1))

(use-package helm
  :doc
  "LSP mode uses `helm` directly. But install `lsp-mode' didn't
   install `helm' automatically last time. It probably didn't
   require `helm' before usage."
  :ensure t
  :after lsp-mode)

(use-package smex
  :doc "Used by `ivy-M-x' for sorting based on frequency + recency."
  :ensure t
  :config
  (setd smex-save-file "var/smex-items"))

(use-package ivy
  :demand t
  :ensure t
  :diminish ivy-mode
  :bind ( :map ivy-occur-grep-mode-map
          ("q"     . kill-buffer-delete-window)
          :map global-map
          ("C-x b" . ivy-switch-buffer)
          ("C-x B" . ivy-switch-buffer-other-window)
          ("M-H"   . ivy-resume) )

  :config
  (bind-keys :map ivy-minibuffer-map
             ("C-x C-f" . fallback-to-find-file)
             ("C-S-W"   . ivy-yank-symbol*)
             ("C-w"     . ivy-yank-word)
             ("C-. @"   . ivy-yank-expanded-region)
             ("C-r"     . ivy-previous-line-or-history)
             ("M-r"     . ivy-reverse-i-search))

  (setq ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-use-virtual-buffers t
        ;; ivy provides better completion compared to default, company mode
        ;; doesn't work nicely with `completion-in-region-function'.
        ivy-do-completion-in-region t)

  (setq ivy-ignore-buffers `("\\` "
                             "\\`\\*git-monitor:"
                             "\\`magit-process:"
                             "\\`magit:"
                             "\\`\\*helpful:"
                             "\\.elc$"
                             "\\`\\.newsrc-dribble\\'"
                             "\\`\\.newsrc.eld\\'"
                             ;; Check these buffers with \C-x \C-b
                             "\\`##?[a-z+@-]*\\'"))

  ;; Disable sorting on input collection
  (mapc (lambda (cmd)
          (push (list cmd) ivy-sort-functions-alist))
        '(hledger-run-command))

  (ivy-mode +1)

  :preface
  (defun fallback-to-find-file ()
    (interactive)
    (ivy-set-action
     (lambda (_x)
       (let ((completing-read-function 'completing-read-default))
         (call-interactively 'find-file))))
    (ivy-done))

  (defun ivy-yank-symbol* ()
    "Yank symbol at point into ivy minibuffer."
    (interactive)
    (if-let ((symbol (with-ivy-window (thing-at-point 'symbol))))
        (insert symbol)
      (message "No symbol at point"))))


(use-package counsel
  :ensure t
  :commands counsel-ag
  :bind (("M-x"       . counsel-M-x)
         ("C-x 8 RET" . counsel-unicode-char)
         ("M-y"       . counsel-yank-pop)
         ("M-s a"     . counsel-project-ag)
         :map minibuffer-local-map
         ("M-r" . counsel-minibuffer-history)

         :map ctl-quote-map
         ("C-'" . counsel-imenu))
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq counsel-yank-pop-separator
        (format "\n%s\n" (make-string 60 ?┅)))

  (setq counsel-find-file-ignore-regexp
        (concat "\\(\\`\\.[^.]\\|"
                (regexp-opt completion-ignored-extensions)
                "\\)"))

  (ivy-set-actions 'counsel-ag '(("f"  counsel-add-ag-flag "Flags")))

  ;; `org-mode-map' and `org-agenda-mode-map' might not be loaded yet.
  (eval-after-load "org"
    '(bind-keys :map org-mode-map
                ("C-c C-j" . counsel-org-goto)
                ("C-c C-q" . counsel-org-tag)))

  (eval-after-load "org-agenda"
    '(bind-keys :map org-agenda-mode-map
                (":" . counsel-org-tag-agenda)))
  :preface
  (defun counsel-add-ag-flag (_args)
    "Insert a new flag into the `counsel-ag-command' being executed.
       This is intended to be invoked with `ivy-dispatching-call' `C-M-m'."
    (setq counsel-ag-command
          (concat (format counsel-ag-command
                          (ivy-read "Flags: " '()))
                  " %s")))

  (defun counsel-project-ag ()
    "Start `counsel-ag' for current project."
    (interactive)
    (counsel-ag (or (and (symbol-at-point)
                         (symbol-name (symbol-at-point)))
                    "")
                (project-root (project-current)))))

(use-package counsel-bbdb
  :ensure t
  :after bbdb
  :config
  ;; This will feel like a hack until I get used to this being here or I
  ;; forget about its existence.
  (setf (symbol-function 'bbdb-complete-mail)
        #'counsel-bbdb-complete-mail))

(use-package ivy-hydra :ensure t)
(use-package swiper
  :ensure t
  :commands swiper-from-isearch
  :bind (
         :map global-map
         ("M-s s" . swiper*)
         :map ctl-period-map
         ("C-s"   . swiper*)
         ("C-S-s" . swiper-all)
         )
  :init
  (bind-key "C-." #'swiper-from-isearch isearch-mode-map)
  :config
  (setq ivy-initial-inputs-alist nil)

  (bind-keys :map swiper-map
             ("M-%" . swiper-query-replace)
             ("C-w" . ivy-yank-word)
             ("M-h" . swiper-avy))
  :preface
  (defun swiper* ()
    (interactive)
    (swiper (and (region-active-p)
                 (progn (deactivate-mark)
                        (buffer-substring-no-properties (mark)
                                                        (point)))))))

;;; JAVA
;; ----------------------------------------------------------------------------
(use-package java-mode
  :defer t
  :hook ((java-mode . company-mode-quicker))
  :init
  (use-package lsp-java
    :ensure t
    :config
    (setq lsp-java-save-actions-organize-imports t)

    (unless (file-exists-p lsp-java-lombok-jar-path)
      (require 'url-handlers)
      (url-copy-file "https://projectlombok.org/downloads/lombok.jar"
                     lsp-java-lombok-jar-path))

    (setq lsp-java-vmargs
          (list "-noverify"
                "-Xmx1G"
                "-XX:+UseG1GC"
                "-XX:+UseStringDeduplication"
                (format "-javaagent:%s" lsp-java-lombok-jar-path)))

    :preface
    (defvar lsp-java-lombok-jar-path
      (expand-file-name "var/lombok.jar" user-emacs-directory)))

  (use-package dap-mode
    :doc "For debugging Java applications."
    :ensure t
    :defer t
    :hook ((java-mode . dap-mode)
           (java-mode . dap-ui-mode))
    :config
    (require 'dap-java)))

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

;;; Language Server Protocol
;;  ------------------------

(use-package lsp-mode
  :ensure t
  :hook ((java-mode . lsp)
         (rust-mode . lsp)
         ;; (c-mode    . lsp)
         ;; (c++-mode  . lsp)
         )
  :diminish lsp-mode
  :bind (:map lsp-mode-map
              ("C-c x" . lsp-execute-code-action))
  :init
  (setq lsp-keymap-prefix "C-;"
        lsp-eldoc-render-all nil)
  :config
  (unless (f-file-p lsp-session-file)
    (f-touch lsp-session-file))

  (define-key lsp-mode-map
    (kbd lsp-keymap-prefix) lsp-command-map)

  (setq lsp-file-watch-threshold 3000)
  ;; GOPATH needs to be set properly for `gopls' to work.
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))

  ;; LSP Clients
  (setq lsp-clients-clangd-executable "clangd"))

(use-package company-lsp
  :after  lsp-mode
  :ensure t
  :config
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :preface
  (defun lsp-ui-doc-toggle ()
    "Toggle document display for symbol at point."
    (interactive)
    ;; Not using `this-command' here to make sure that doc can stay displayed as
    ;; I move around in the buffer.
    (if (get 'lsp-ui-doc-toggle 'doc-visible-p)
        (progn (put 'lsp-ui-doc-toggle 'doc-visible-p nil)
               (lsp-ui-doc-hide))
      (lsp-ui-doc-show)
      (put 'lsp-ui-doc-toggle 'doc-visible-p t)))

  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'point)

  (setq lsp-ui-sideline-show-hover t)

  (setq lsp-ui-doc-enable nil         ; Disable auto doc show on hover
        lsp-ui-doc-header t
        lsp-ui-doc-position 'at-point ; When asked to, display doc at point.
        lsp-ui-doc-use-webkit t)

  (bind-keys :map lsp-ui-mode-map
             :prefix "C-c C-d"
             :prefix-map lsp-ui-doc-map
             ("C-t" . lsp-ui-doc-show)
             ("C-d" . lsp-describe-thing-at-point)))


;;; C/C++-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package cc-mode
  :bind (:map c-mode-base-map
              ("<return>" . newline-and-indent)
              ("C-c C-k"  . compile)
              ("C-c C-t"  . c-toggle-comment-style))
  :config
  (setq-default c-block-comment-flag t)

  (use-package cc-config
    :demand t
    :pin manual
    :load-path "etc/")

  (add-hook 'c-mode-hook #'custom-c-mode-common-hook)
  (add-hook 'c++-mode-hook #'custom-c-mode-common-hook))

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

(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :hook ((c-mode   . ggtags-mode)
         (c++-mode . ggtags-mode))
  :init
  (setq ggtags-enable-navigation-keys nil))

(use-package dtrt-indent
  :doc "Automatically guess offset and tabs-indent for opened file."
  :ensure t
  :disabled t
  :diminish dtrt-indent-mode
  :hook (java-mode . dtrt-indent-mode))

;;; RUST
;; ──────────────────────────────────────────────────────────────────
(use-package pos-tip
  :doc "Racer needs this package and for some reason, it is not
  installed."
  :ensure t)

(use-package rust-mode
  :defer t
  :ensure t
  :bind (:map rust-mode-map
              ("RET" . newline-and-indent))
  :config
  (use-package cargo
    :ensure t
    :config
    (unless (executable-find "cargo")
      (message "Please install cargo or modify PATH to include it.")))

  (use-package racer
    :ensure t
    :config
    (setq racer-rust-src-path
          (expand-file-name "~/code/rust/src/")))

  (use-package flycheck-rust
    :ensure t)

  (add-hook 'rust-mode-hook
            (lambda ()
              (lsp)
              (racer-mode +1)
              (eldoc-mode +1)
              (cargo-minor-mode +1)
              (flycheck-rust-setup))))

;;; PYTHON-MODE
;;  ─────────────────────────────────────────────────────────────────

(use-package py-autopep8
  :ensure t
  :hook (elpy-mode . py-autopep8-enable-on-save)
  :init
  (when (not (executable-find "autopep8"))
    (message "AutoPEP8 not installed!")))

(use-package elpy
  :ensure t
  :defer t
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt")
    (message "IPython not found!")))

(use-package python-mode
  :defer t
  :hook ((python-mode . elpy-mode))
  :config
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil
        python-indent-offset 2)

  (when (not (executable-find "flake8"))
    (message "Please install flake8")))

;;; LISP MODE
;;  ─────────────────────────────────────────────────────────────────
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

(use-package xray :load-path "package/lisp/" :defer t)

(use-package macrostep  :defer t :ensure t)

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((emacs-lisp-mode lisp-mode
                          clojure-mode
                          clojurescript-mode
                          cider-repl-mode
                          racket-mode
                          scheme-mode
                          eval-expression-minibuffer-setup)
         . enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("M-S" . paredit-splice-sexp)
              ("M-R" . paredit-raise-sexp)
              ("C-M-)" . utils-paredit-slurp-all-the-way-forward)
              ("C-M-(" . utils-paredit-slurp-all-the-way-backward)
              ("C-M-}" . utils-paredit-barf-all-the-way-forward)
              ("C-M-{" . utils-paredit-barf-all-the-way-backward))
  :preface
  (defun utils-paredit-barf-all-the-way-backward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-backward-down)
    (paredit-splice-sexp))

  (defun utils-paredit-barf-all-the-way-forward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-forward-down)
    (paredit-splice-sexp)
    (if (eolp) (delete-horizontal-space)))

  (defun utils-paredit-slurp-all-the-way-backward ()
    (interactive)
    (catch 'done
      (while (not (bobp))
        (save-excursion
          (paredit-backward-up)
          (if (eq (char-before) ?\()
              (throw 'done t)))
        (paredit-backward-slurp-sexp))))

  (defun utils-paredit-slurp-all-the-way-forward ()
    (interactive)
    (catch 'done
      (while (not (eobp))
        (save-excursion
          (paredit-forward-up)
          (if (eq (char-after) ?\))
              (throw 'done t)))
        (paredit-forward-slurp-sexp))))
  :config
  (mapcar (lambda (key-seq)
            (unbind-key key-seq paredit-mode-map))
          (list "M-r" "M-s" "M-?")))

(use-package smartparens
  :ensure t
  :defer 2
  :bind (:map smartparens-mode-map
              ("M-S"   . sp-splice-sexp)
              ("M-R"   . sp-raise-sexp)
              ("C-M-)" . utils-paredit-slurp-all-the-way-forward)
              ("C-M-(" . utils-paredit-slurp-all-the-way-bacward)
              ("C-M-}" . utils-paredit-barf-all-the-way-forward)
              ("C-M-{" . utils-paredit-barf-all-the-way-bacward))
  :diminish smartparens-mode
  :config
  (setq sp-highlight-pair-overlay nil)
  (setq sp-ignore-modes-list
        (append sp-ignore-modes-list
                '(emacs-lisp-mode
                  lisp-mode
                  clojure-mode
                  cider-repl-mode
                  eshell-mode
                  eval-expression-minibuffer-setup)))

  (sp-use-paredit-bindings)

  (dolist (key-seq (list "M-r" "M-s" "M-?"))
    (unbind-key key-seq smartparens-mode-map))

  ;; Remove this remapping
  (define-key smartparens-strict-mode-map [remap kill-region] nil)
  (define-key smartparens-strict-mode-map [remap delete-region] nil)

  ;; Enable `smartparens-global-mode';
  (smartparens-global-mode +1))

(use-package javadoc-lookup :bind ("C-h j" . javadoc-lookup) :ensure t)
(use-package clojure-mode
  :ensure t
  :pin melpa
  :defer t
  :mode "\\.clj\\'"
  :config
  (setq clojure-indent-style :always-align
        clojure-align-forms-automatically t))

(use-package clj-refactor
  :pin melpa
  :ensure t
  :diminish clj-refactor-mode
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :bind (:map clj-refactor-map
              ("C-; c <SPC>" . clojure-top-level-spacing))
  :config
  (cljr-add-keybindings-with-prefix "C-;")
  (setq cljr-ignore-analyzer-errors t
        cljr-warn-on-eval nil)

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
  :bind (:map cider-repl-mode-map
              ("C-S-t" . cider-repl-toggle-pretty-printing))
  :config
  (setq nrepl-log-messages t
        nrepl-use-ssh-fallback-for-remote-hosts t)

  (setq cider-repl-use-pretty-printing t
        cider-auto-jump-to-error nil
        cider-prompt-for-symbol nil
        cider-repl-history-file (expand-file-name "var/cider-repl-history.el"
                                                  user-emacs-directory)
        cider-eldoc-display-for-symbol-at-point nil
        cider-eldoc-display-context-dependent-info t)

  (advice-add 'cider-repl-indent-and-complete-symbol
              :around
              (lambda (&rest _args)
                (completion-at-point)))

  (define-clojure-indent (for-all 1)))


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
             "curl -s \"https://api.github.com/repos/borkdude/clj-kondo/releases\""
             "| grep browser"
             "| grep linux-amd64.tar.gz"
             "| head -n 1 | cut -d '\"' -f4 | xargs curl -sL | tar -xz;"
             "mv /var/clj-kondo ~/.local/bin;"))))

(use-package geiser
  :defer t
  :ensure t
  :hook (racket-mode . geiser-mode)
  :config
  (advice-add 'run-geiser
              :after
              (lambda (&rest _args)
                (message "Set the scheme impl for buffer with `geiser-set-scheme`.''"))))

(use-package racket-mode
  :defer t
  :ensure t
  :mode "\\.rkt\\'"
  :bind (:map racket-mode-map
              ("C-x C-e" . racket-eval-last-sexp))
  :config
  (use-package scribble-mode :ensure t)
  :preface
  (defun racket-eval-last-sexp ()
    "Eval the previous sexp asynchronously and `message' the result."
    (interactive)
    (require 'cider-overlays)
    (racket--cmd/async
     `(eval
       ,(buffer-substring-no-properties (racket--repl-last-sexp-start)
                                        (point)))
     (lambda (v)
       (cider--make-result-overlay (substring v nil -1) :duration 'command)))))

(use-package slime
  :ensure t
  :commands slime-connected-p
  :preface
  (defun start-slime ()
    (when (not (slime-connected-p))
      (save-window-excursion (slime))))
  :hook ((lisp-mode . start-slime)
         (lisp-mode . slime-mode))
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
  :bind (:map ctl-quote-map
              ("C-m" . man))
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
  :hook ((prog-mode    . whitespace-mode)
         (message-mode . whitespace-mode))
  :bind (("C-x C-y" . whitespace-mode)
         ("C-x y"   . whitespace-toggle-options))
  :init
  (setq whitespace-style
        '(face tabs spaces trailing lines-tail newline empty newline-mark))
  (setq whitespace-display-mappings
        '((space-mark   ?\     [?·]     [?.])
          (space-mark   ?\xA0  [?¤]     [?_])
          (newline-mark ?\n    [?↵ ?\n] [?$ ?\n])
          (tab-mark     ?\t    [?» ?\t] [?\\ ?\t])))

  ;; Cleanup whitespace before saving files
  (add-hook 'before-save-hook
            (lambda ()
              ;; Exclude whitespace-sensitive modes that I know of.
              ;; 1. Graphics in `ob-ipython' do not work if we remove trailing
              ;;    newlines. So, this should be excluded for buffers in
              ;;    `image-mode'.
              (when (not (memq major-mode '(markdown-mode
                                            image-mode)))
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
    :hook (web-mode . emmet-mode)))

(use-package skewer-mode
  :ensure t
  :hook ((css-mode  . skewer-css-mode)
         (html-mode . skewer-html-mode)
         (js2-mode  . skewer-mode)))

(use-package js2-mode    :defer t :ensure t)
(use-package coffee-mode :defer t :ensure t)
(use-package elm-mode    :defer t :ensure t)

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
              ("C-c C-g" . search-ruby-gems)))

(use-package rinari
  :ensure t
  :after ruby-mode
  :bind (:map ruby-mode-map
              ("C-c i" . rinari-insert-erb-skeletion))
  :config
  (global-rinari-mode)
  (add-hook 'ruby-mode-hook 'electric-pair-mode))

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
  :hook ((go-mode . lsp)
         (go-mode . electric-pair-mode)
         (go-mode . lsp-go-install-save-hooks))
  :config
  (use-package go-guru :ensure t)

  (unless (getenv "GOPATH")
    (setenv "GOPATH" (shell-command-to-string "echo -n $GOPATH")))

  :preface
  ;; Use `gopls' to format buffer and clean up imports.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)))

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

(use-package vc :config (setq vc-display-status t))
(use-package diff-mode
  :config
  (setq diff-font-lock-prettify t
        diff-font-lock-syntax nil))

(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode +1))

(use-package transient
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

  :bind (;; So that indentation is sane
         :map magit-mode-map
         ("C-c C-r" . magit-change-repository)
         :map ctl-period-map
         ("C-x" . magit-status))

  :init
  ;; Disable `global-magit-file-mode' until it can replace `git-gutter-mode'.
  ;; This needs to happen before the package is loaded because these minor
  ;; modes are enabled at load-time.
  (setq global-magit-file-mode nil)

  :config
  (magit-auto-revert-mode -1)
  (setq magit-completing-read-function 'ivy-completing-read
        ;; Showing diffs during commits is currently slow.
        magit-commit-show-diff nil
        ;; More granular diffs for the hunk under point
        magit-diff-refine-hunk t)

  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (mapc (lambda (mode)
          (add-hook mode
                    (lambda ()
                      ;; I want to use C-RET solely for switching buffers.
                      (unbind-key "C-RET" (intern (format "%s-map" mode))))))
        '(magit-mode magit-status-mode)))

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
        password-cache-expiry 3600))

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

(use-package docker-tramp :ensure t)


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
    (setq bbdb-mua-auto-update-p 'create
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
  :hook ((gnus-group-mode . olivetti-mode)
         (gnus-article-mode . goto-address-mode))
  :init
  (setd gnus-init-file "etc/gnus-config.el")
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
              ("K" . pdf-view-previous-line-or-previous-page)
              ("R" . reopen-pdf-file))
  :preface
  (defun reopen-pdf-file ()
    "Reopen PDF file."
    (interactive)
    (let ((file-path (buffer-file-name (current-buffer)))
          (current-page (pdf-view-current-page))
          (interleave-enabledp interleave-pdf-mode))
      (kill-buffer)
      (find-file file-path)
      (pdf-view-goto-page current-page)
      (when interleave-enabledp
        (interleave-pdf-mode +1))))
  :init
  ;; `abbreviate-file-name' doesn't handle `nil' values. For buffer that do not
  ;; have associated files, this fails. I had been facing this while opening PDF
  ;; files in Gnus.
  (advice-add 'abbreviate-file-name :around (lambda (orig-f file-name)
                                              (if file-name
                                                  (funcall orig-f file-name)
                                                "<buffer-with-no-file>"))))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :config
  (setd pdf-view-restore-filename "var/pdf-view-restore.el")
  (add-hook 'pdf-view-mode-hook #'pdf-view-restore-mode))

;;; ERC
;;  ─────────────────────────────────────────────────────────────────
(use-package erc-config
  :commands erc-connect
  :load-path "etc/"
  :defer 10
  :config
  ;; (erc-connect)
  )

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
     (midnight-mode +1)
     (midnight-delay-set 'midnight-delay -7200)
     (add-hook 'midnight-hook
               (lambda ()
                 (when (and (boundp 'commit-org-files)
                            commit-org-files)
                   (commit-org-files))
                 (when (member (format-time-string "%A")
                               '("Sunday" "Saturday"))
                   (alert "You might want to rebuild Emacs today.")))))))

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

(use-package gnuplot-mode
  :ensure t
  :mode ("\\.\\(gp\\|gnuplot\\)$"))

(use-package calc
  :bind ("C-z" . calc-dispatch)
  :init
  (setq calc-settings-file (expand-file-name "calc.el" user-emacs-directory))

  :config
  (setq calc-gnuplot-default-device "wxt")

  (require 'calc-ext)                   ; Modifies the bindings below.
  (define-key calc-mode-map [M-return] #'calc-last-args)
  (define-key calc-mode-map "x"  #'calc-counsel-M-x)
  (define-key calc-mode-map "gP" #'calc-graph-plot)

  (advice-add 'calc :around (lambda (original-calc &rest args)
                              (let ((inhibit-message t))
                                (apply original-calc args))))

  ;; This is a bug in `calc-graph-add-curve' that causes it to treat
  ;; `nil' as a number.
  (require 'calc-misc)
  (advice-add 'calc-graph-view-commands :after #'calc-enable-gnuplot-mode)

  :preface
  (defun calc-counsel-M-x ()
    (interactive)
    (counsel-M-x "calc-"))

  (defun calc-enable-gnuplot-mode (&rest _args)
    (with-current-buffer calc-gnuplot-input
      (unless (and (boundp 'gnuplot-mode)
                   (not gnuplot-mode))
        (gnuplot-mode)))))

(use-package sql
  :defer t
  :preface
  (defun upcase-last-keyword ()
    (when (and (memql last-input-event  (list ?  ?\())
               (save-excursion
                 (backward-char 2)
                 (and (memq (face-at-point) '(font-lock-keyword-face font-lock-builtin-face))
                      (< 1 (length (word-at-point))))))
      (upcase-word -1)))
  :init
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (make-local-variable 'post-self-insert-hook)
              (add-hook 'post-self-insert-hook #'upcase-last-keyword))))

(use-package mule-cmds
  :bind (("C-x \\" . set-input-method)
         ("C-\\"   . toggle-input-method))
  :init
  (setq default-input-method "german-prefix"))

;;; -----------------------------------------------------------------
(use-package ipcalc       :ensure t :commands ipcalc)
(use-package memory-usage :ensure t :commands memory-usage)
(use-package proof-general :defer t :ensure t)

(use-package langtool
  :ensure t
  :bind (:map ctl-quote-map
              ("c k" . langtool-check))
  :config
  (setq langtool-disabled-rules "WHITESPACE_RULE")

  :init
  (setq langtool-language-tool-jar
        (expand-file-name "var/LanguageTool-Stable/languagetool-commandline.jar"
                          user-emacs-directory))
  (unless (file-exists-p langtool-language-tool-jar)
    (let ((default-directory user-emacs-directory))
      (async-shell-command
       "rm -rf var/LanguageTool*;
        wget -O var/languagetool.zip https://languagetool.org/download/LanguageTool-stable.zip;
        unzip -o var/languagetool.zip -d var/;
        rm var/languagetool.zip;
        mv var/LanguageTool-* var/LanguageTool-Stable")
      (add-to-list 'emacs-init-end-info "> Started download for languagetool."))))

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

(use-package google-translate
  :ensure t
  :bind (
         :map ctl-quote-map
         ("l p" . google-translate-listen)
         :map ctl-x-map
         ("/" . google-translate-dwim)
         )
  :init
  (setq google-translate-show-phonetic t
        google-translate-listen-program "mplayer")

  :config
  (advice-add 'google-translate-json-suggestion
              :override
              (lambda (json)
                (when-let ((info (and (< (length json) 7)
                                      (aref json 7))))
                  (aref info 1))))
  :preface
  (defun google-translate-record-pair ()
    "Return the source word and the translated word (from the
     *Google Translate* buffer as a cons pair."
    (if (string= (buffer-name) "*Google Translate*")
        (let* ((trim (lambda (s) (->> s s-trim (s-replace-all '(("[Listen]" . ""))))))
               (text-start (text-property-any (point-min)
                                              (point-max)
                                              'face
                                              'google-translate-text-face))
               (middle (text-property-any (point-min)
                                          (point-max)
                                          'face
                                          'google-translate-translation-face)))
          (define-word-record-definition (funcall trim (buffer-substring-no-properties text-start middle))
            (funcall trim (buffer-substring middle (point-max)))))
      (message "*Google Translate* is not the current buffer.")))

  (defun google-translate-listen ()
    (interactive)
    (google-translate-listen-translation
     google-translate-default-source-language
     (cond
      ((use-region-p) (buffer-substring (region-beginning) (region-end)))
      ((word-at-point))
      (t (message "Couldn't find anything to translate.")))))

  (defun google-translate-dwim (arg)
    "Quickly translate default source <-> default destination languages."
    (interactive "p")
    (let ((translate-buffer "*Google Translate*")
          (translate-fn (if (use-region-p) #'google-translate-at-point
                          #'google-translate-query-translate)))
      (case arg
        (1 (funcall translate-fn))
        (4 (let ((google-translate-default-source-language google-translate-default-target-language)
                 (google-translate-default-target-language google-translate-default-source-language))
             (funcall translate-fn)))
        (16 (funcall translate-fn t)))
      (dolist (w (window-list))
        (when (equal translate-buffer (buffer-name (window-buffer w)))
          (select-window w)))
      (with-current-buffer translate-buffer
        (google-translate-record-pair)))))

(use-package gif-screencast
  :defer t
  :doc
  "A better alternative to my `start-recording-window' command."
  :load-path "packages/lisp")

(use-package command-log-mode
  :doc "Useful to displaying currently executing Emacs commands."
  :commands (clm/open-command-log-buffer global-company-mode)
  :load-path "packages/lisp"
  :preface
  (defun show-emacs-commands ()
    (interactive)
    (clm/open-command-log-buffer)
    (global-command-log-mode +1)))

(provide 'init)
;;; init.el ends here
