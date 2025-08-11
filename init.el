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

(setq byte-compile-warnings '(not cl-functions))

;;; Avoid garbage collection during Emacs startup and garbage collect right
;;; after loading Emacs configuration.
(setq gc-cons-threshold most-positive-fixnum
      garbage-collection-messages t)
(add-hook 'after-init-hook
          (lambda ()
            (setq garbage-collection-messages nil)
            (garbage-collect)
            (setq gc-cons-threshold (* 256 1024 1024))))

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

(setq use-package-compute-statistics t)

(use-package use-package-ensure-system-package)
(use-package system-packages :ensure t)

(use-package package-vc
  :custom (package-vc-allow-build-commands t))

(use-package delight :ensure t :demand t)
(use-package bind-key :ensure t)

(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)

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


;;; Emacs Lisp Compilation

(setq load-prefer-newer t)

(use-package comp
  :if (fboundp 'native-compile)
  :custom (native-comp-async-report-warnings-errors nil))


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

(use-package repeat
  :init
  ;; If a symbol property named `repeat-map' exists for a command and it's a
  ;; keymap, it's activate as a transient-map after command is executed.
  (let ((inhibit-message t))
    (repeat-mode +1))

  :config
  (setq repeat-exit-timeout 30)

  :preface
  (defmacro define-repeat-map (name &rest bindings)
    ;; TODO: add a keyword argument to provide a name for the keymap instead of
    ;; the auto-generated name.
    (declare (indent 1))
    ;; Use gensym just for the name of the symbol, let the generated
    ;; symbol be garbage collected. `intern' then creates a new symbol
    ;; that is used and remains valid globally.
    (let ((keymap-symbol (or name (intern (symbol-name (gensym "repeat-map--"))))))
      `(let* ((m (make-sparse-keymap)))
         (dolist (binding (quote ,bindings))
           (define-key m (kbd (car binding)) (cdr binding))
           (put (cdr binding) 'repeat-map (quote ,keymap-symbol)))
         (defvar ,keymap-symbol m)))))


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

  :config
  (define-repeat-map repeat/url-navigation
    ("f" . jump-to-next-url)
    ("b" . jump-to-previous-url))

  :bind ( ("C-c m" . switch-to-minibuffer)
          ("C-c 0" . quick-switch-themes)
          ("C-c b" . switch-to-buffer-with-mode)
          ("<print>" . snap-it)

          ("M-g s" . switch-to-scratch-new-tab)
          ("M-g f" . jump-to-next-url)
          ("M-g b" . jump-to-previous-url)

          :map ctl-m-map
          ("o" . run-in-other-window)
          ("S" . macos-fix-keyboard-modifiers)

          :map ctl-quote-map
          ("g"   . websearch-it)
          ("l l" . search-linguee)
          ("l t" . translate-with-linguee)
          ("."   . insert-date-time-at-point)
          ("c e" . vicarie/eval-print-last-sexp)
          ("c =" . vicarie/eval-replace-last-sexp)
          ("c r" . rename-file-and-buffer)

          ;; ("C-a" . emacspeak-wizards-execute-asynchronously)
          ("M-x" . async-M-x)

          :map ctl-period-map
          ("k"   . project-compile)
          ("C-k" . project-compile)
          ("K"   . recompile)
          ("$"   . selective-display-beyond-col)
          ("u"   . underline-text)
          ("s"   . surround-symbol-with) ))


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

(progn
  ;; I just want to group these somehow and I don't want to use a fake
  ;; use-package package name because it messes up with `use-package-report'.

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
        (* 100 1024 1024))

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
        kept-new-versions 10
        kept-old-versions 10)

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
  (let ((appearance-enable-time-based-theme-switching nil))
    (appearance-init))
  (load-theme 'ef-dream))

(use-package mode-line-config
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
      (sit-for 5)
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
  (setq world-clock-timer-enable nil
        world-clock-time-format "\n──────────────\n\t%A %d %B %R %Z\n")

  (setq display-time-string-forms
        '((propertize
           (format " %s.%s.%s %s %s:%s "
                   (string-trim day) (string-trim month) (substring year -2) dayname 24-hours minutes)
           'face 'bold))
        display-time-default-load-average 1     ; 5 minute load avg
        display-time-load-average-threshold 0.8 ; >80%
        display-time-mail-string "")

  (display-time-mode +1)

  :preface
  (defun world-clock* (&optional arg)
    (interactive "P")
    (if arg
        (with-selected-date-time (world-clock))
      (world-clock))))

(use-package battery
  :disabled t
  :demand t
  :config
  (setq battery-mode-line-format
        (propertize "%b%p%% " 'face 'mode-line-battery-face))
  (display-battery-mode +1)

  ;; (run-with-timer 60 60 #'battery-protection-notifications)

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

(use-package net-utils
  :bind ( :map net-utils-mode-map ("G" . netutils--revert-buffer) )
  :preface
  (defun netutils--revert-buffer ()
    "Revert buffer and get back to the same line number."
    (interactive)
    (let ((line (1+ (count-lines (point-min) (point))))
          (buffer (buffer-name (current-buffer))))
      (revert-buffer)
      (add-hook 'after-revert-hook
                (lambda ()
                  (forward-line line))))))

(use-package sql
  :defer t
  :bind ( :map sql-interactive-mode-map
          ("S-SPC" . upcase-last-symbol-and-space)

          :map sql-mode-map
          ("S-SPC" . upcase-last-symbol-and-space) ))


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

(use-package verb
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key org-mode-map
                          (kbd "C-c v")
                          verb-command-map))))

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
  :doc
  "This built-in package provides a way to keep a set of window
   configurations around that can be switched to easily."
  :config
  (tab-bar-history-mode +1)
  (setq tab-bar-show nil
        tab-bar-tab-name-function #'tab-bar-tab-name-all))

(use-package calendar
  :defer t
  :custom (calendar-date-style 'iso)
  :bind (:map ctl-quote-map
              ("c c" . calendar))
  :init
  (add-hook 'calendar-today-visible-hook #'calendar-mark-today)

  (setq
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


(use-package xwidget
  :custom (xwidget-webkit-cookie-file
           (expand-file-name "webkit-cookies.txt" emacs-assets-directory))
  :config
  (ensure-windows-on-right "\\*xwidget-webkit:"))

(use-package eww
  :defer t
  :custom ((eww-auto-rename-buffer 'title)
           (eww-browse-url-new-window-is-tab nil))
  :config
  (add-hook 'eww-after-render-hook #'eww-readable)
  :custom (eww-bookmarks-directory emacs-assets-directory))

(use-package browse-url
  :defer t
  :config
  (setq browse-url-new-window-flag nil))


(use-package atomic-chrome
  :ensure t
  :init
  (defvar atomic-chrome--saved-window-configuration nil)
  (add-hook 'atomic-chrome-edit-mode-hook
            (lambda ()
              (setq atomic-chrome--saved-window-configuration (current-window-configuration))
              (delete-other-windows)))
  (add-hook 'atomic-chrome-edit-done-hook
            (lambda ()
              (when (window-configuration-p atomic-chrome--saved-window-configuration)
                (set-window-configuration atomic-chrome--saved-window-configuration)))))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(use-package rainbow-mode :ensure t :defer t)

(use-package uniquify
  :doc "Unique buffer names"
  :delight t
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
  :commands (crux-swap-windows crux-transpose-windows)
  :bind (("C-<backspace>" . crux-kill-line-backwards)
         ("S-<return>"    . crux-switch-to-previous-buffer))
  :hook (after-init . crux-reopen-as-root-mode))

;;; TEXT-EDITING, FOLDING and NAVIGATION
;; ─────────────────────────────────────────────────────────────────

(use-package elec-pair :init (electric-pair-mode +1))

(use-package grep-edit-mode
  :commands grep-change-to-grep-edit-mode
  :bind ( :map ctl-m-map ("e" . grep-change-to-grep-edit-mode) ))

(use-package outline-minor-mode
  :defer t
  :custom ((outline-blank-line t))
  :init
  (delight 'outline-minor-mode "" "outline")
  ;; This minor mode uses selective display to hide text so it displays three
  ;; dots (ellipsis) like `selective-display'
  ;; https://www.emacswiki.org/emacs/OutlineMode
  (set-display-table-slot standard-display-table
                          'selective-display
                          (string-to-vector " ... ")))

(use-package hideshow
  :defer t
  :delight hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :bind ( :map hs-minor-mode-map
          ([backtab] . hs-toggle-hiding)
          ("C-c @ a"  . my-toggle-hideshow-all)
          ("C-c @ s"  . hs-show-block)
          ("C-c @ h"  . hs-hide-block) )

  :custom
  (hs-display-lines-hidden t)

  :preface
  (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
  (defun my-toggle-hideshow-all ()
    "Toggle hideshow all."
    (interactive)
    (setq my-hs-hide (not my-hs-hide))
    (if my-hs-hide
        (hs-hide-all)
      (hs-show-all))))

(use-package wrap-region
  :doc "Wrap region with custom chars."
  :ensure t
  :hook (after-init . wrap-region-mode)
  :delight
  :config
  (wrap-region-add-wrappers
   '(("=" "=" nil (org-mode))
     ("~" "~" nil (org-mode))))
  (wrap-region-global-mode +1)
  (wrap-region-remove-wrapper "<" 'org-mode))

(use-package selected
  :delight selected-minor-mode
  :ensure t
  :config
  (selected-global-mode +1))

(use-package misc
  :bind ( :map ctl-period-map ("d" . duplicate-dwim) )
  :custom ( duplicate-line-final-position -1
            duplicate-region-final-position -1) )

(use-package select :init (setq select-enable-clipboard t))
(use-package simple
  :doc "The great simple.el"
  :demand t
  :delight auto-fill-function
  :bind (("M-q"   . fill-or-unfill)
         ("M-["   . backward-delete-dwim)

         :map ctl-period-map
         ("C-u" . delete-indentation)

         :map ctl-quote-map
         (":"   . set-variable)
         ("s l" . list-processes)
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
  (define-repeat-map repeat/deletion
    ("C-u" . delete-indentation))

  ;; Multiple-cursors changes transient-mark-mode to (only only .. t),
  ;; if shift-select-mode is enabled.
  (setq shift-select-mode nil)

  ;; Make text copied/cut from outside Emacs part of Emacs kill-ring on first
  ;; kill inside Emacs.
  (setq kill-ring-max 1024
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

  (setq suggest-key-bindings t
        extended-command-suggest-shorter nil)

  (setq-default indent-tabs-mode nil)

  (setq async-shell-command-buffer 'new-buffer
        set-mark-command-repeat-pop t
        column-number-mode t
        size-indication-mode t)

  (delight 'visual-line-mode nil "simple")

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

(use-package visual-wrap
  :init
  (global-visual-wrap-prefix-mode +1))

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
          ("v" . vcursor-use-vcursor-map-quietly)
          ("V" . vcursor-toggle-copy)
          ("@" . vcursor-expand-region-expand) )
  :custom (vcursor-key-bindings t)
  :init
  (add-hook 'vcursor-use-vcursor-map-hook
            #'vcursor-toggle-vcursor-config-in-buffer)
  :preface
  (defun vcursor-use-vcursor-map-quietly ()
    (interactive)
    (require 'vcursor)
    (quietly (call-interactively #'vcursor-use-vcursor-map)))

  ;; These two commands interact in a contrived way to fit exactly the workflow
  ;; that I need to copy regions present at a place in the buffer to the
  ;; position from where I start the whole process.
  (defun vcursor-expand-region-expand ()
    (interactive)
    (unless (eq last-command 'vcursor-expand-region)
      (let ((p (point)))
        (vcursor-goto t)
        (vcursor-move p)))
    (call-interactively #'er/expand-region))

  (let ((vcursor-use-vcursor-map--wc nil))
    (defun vcursor-toggle-vcursor-config-in-buffer ()
      (let ((selected-text (when (region-active-p)
                             (buffer-substring (region-beginning)
                                               (region-end)))))
        (if vcursor-use-vcursor-map
            (progn (fringe-set-louder)
                   (setq vcursor-use-vcursor-map--wc
                         (current-window-configuration))
                   (save-excursion
                     (previous-line)
                     (vcursor-move (point))))
          (fringe-restore-default)
          (when selected-text
            (vcursor-swap-point)
            (insert selected-text))
          (vcursor-disable)
          (set-window-configuration vcursor-use-vcursor-map--wc))))))

(use-package multiple-cursors
  :doc "A minor mode for editing with multiple cursors."
  :ensure t
  :custom (mc/always-run-for-all t)
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

   :map selected-keymap
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
  (define-repeat-map repeat/goto
    ("C-SPC" .  goto-last-change)))

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
  :bind ( :map ctl-quote-map
          ("o a" . footnote-add-footnote)
          ("o b" . footnote-back-to-message)
          ("o d" . footnote-delete-footnote)
          ("o g" . footnote-goto-footnote)) )

(use-package csv-mode      :defer t :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t
  :hook ( markdown-mode . visual-line-mode ) )

(use-package cdlatex :ensure t :hook (Latex-Mode-Hook . turn-on-cdlatex))
(use-package auctex
  :pin gnu
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :ensure t
  :demand t
  :custom (TeX-master 'dwim)
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
  :delight volatile-highlights-mode
  :hook (after-init . volatile-highlights-mode)
  :custom (vhl/use-pulsing-visual-effect-p t))

(use-package info
  :config
  ;; Use a variable-pitched font face if using a light theme.
  (add-hook 'info-mode-hook (lambda ()
                              (when (eq (frame-parameter nil 'background-mode)
                                        'light)
                                (variable-pitch-mode +1)))))

(use-package which-key
  :delight which-key-mode
  :init
  (setq which-key-max-description-length nil
        which-key-preserve-window-configuration t)
  (which-key-mode +1))

(use-package project
  :bind ( :map project-prefix-map
          ("o" . git-ls-files-find-file)
          ("m" . magit-status)

          :map global-map
          ("M-RET" . project-find-file)

          :map ctl-m-map
          ("C-f" . git-ls-files-find-file)
          ("f"   . git-ls-files-find-file) )

  :init
  (setq find-program
        (expand-file-name "bin/find-wrapper.sh" user-emacs-directory))

  (setq project-list-file
        (expand-file-name "var/project-list" user-emacs-directory)

        ;; Useful for marking sub-directories inside a version controlled
        ;; project but doesn't work without version control.
        project-vc-extra-root-markers
        (list ".project-x" "Cargo.toml"
              ;; Projects with multiple `pom.xml' files encounter issues with
              ;; the following:
              ;; "pom.xml"
              ))

  :config
  (autoload #'magit-project-status "magit-extras" nil t)

  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit Status" ?m))

  :preface
  (defun git-ls-files-find-file ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (->> (shell-command-to-string "git ls-files")
           (s-lines)
           (seq-filter #'s-present?)
           (completing-read "Find File: ")
           (find-file)))))

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
  (setq save-place-file
        (locate-user-emacs-file "var/saved-places"))
  (save-place-mode +1))

;;; Buffers, Windows and Frame
;; ――――――――――――――――――――――――――――――――――――――――

(use-package frame
  :init
  (undelete-frame-mode +1))

(use-package pixel-scroll
  :bind ( :map ctl-m-map
          ("j" . --scroll-up-other-window)
          ("k" . --scroll-down-other-window) )
  :custom (pixel-scroll-precision-use-momentum t)
  :init
  (pixel-scroll-mode +1)
  (pixel-scroll-precision-mode +1)

  (put '--scroll-down-other-window 'repeat-exit-timeout 4)
  (put '--scroll-up-other-window 'repeat-exit-timeout 4)

  (define-repeat-map repeat/scroll-window
    ("J" . --scroll-up-other-window)
    ("K" . --scroll-down-other-window)
    ("j" . pixel-scroll-up)
    ("k" . pixel-scroll-down))

  :preface
  (defun --scroll-down-other-window ()
    (interactive)
    (with-selected-window (next-window (selected-window))
      (pixel-scroll-down)))

  (defun --scroll-up-other-window ()
    (interactive)
    (with-selected-window (next-window (selected-window))
      (pixel-scroll-up))))

(use-package ultra-scroll
  :vc ( :url "https://github.com/jdtsmith/ultra-scroll"
        :rev :newest )
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode +1))

(use-package window
  :bind ( :map window-prefix-map
          ("+" . fit-window-to-buffer*)
          ("k" . crux-swap-windows)
          ("j" . crux-transpose-windows)
          ("t" . toggle-window-split) )
  :custom ( (fit-window-to-buffer-horizontally t)
            (switch-to-buffer-obey-display-actions t)
            (window-resize-pixelwise t)
            (next-screen-context-lines 2)
            (split-width-threshold 140)
            (split-window-preferred-direction 'longest) )

  :init
  ;; Adds an entry to `emulation-mode-map-alists' which take precedence over all
  ;; active minor-mode keymaps and the active major-mode keymap in a buffer.
  (bind-key* [C-return] #'other-window)
  (bind-key* [C-m C-m] #'delete-window)

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

  (advice-add #'split-window-below :filter-return #'select-window)
  (advice-add #'split-window-right :filter-return #'select-window)

  ;; (advice-add #'scroll-other-window :before #'highlight-context-lines-before-scroll)
  ;; (advice-add #'scroll-up-command :before #'highlight-context-lines-before-scroll)

  (dolist (display-spec
           '(("\\*Async Shell Command\\*" display-buffer-no-window)
             ("\\`\\*Flycheck errors\\*\\'" (display-buffer-reuse-window
                                             display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 10))) )
    (add-to-list 'display-buffer-alist display-spec))

  (ensure-windows-at-bottom "\\`\\*e?shell" "\\*vterm\\*" "\\*Calendar\\*")
  (ensure-windows-on-right "\\` ?\\*eldoc\\*\\'" "\\` ?\\*Dictionary\\*\\'"
                           "\\`magit: .*\\'" "\\`\\*cider-doc\\*\\'")

  :preface
  (defun highlight-context-lines-before-scroll (&rest _args)
    (let ((window (if (eq this-command 'scroll-other-window)
                      (next-window (selected-window))
                    (selected-window))))
      (with-current-buffer (window-buffer window)
        (let* ((region-start (window-end window))
               (region-end (save-excursion
                             (goto-char (window-end window))
                             (forward-line (- (1+ next-screen-context-lines)))
                             (beginning-of-line)
                             (point)))
               (overlay (make-overlay region-start region-end)))
          (overlay-put overlay 'face 'highlight)
          (redisplay t)
          (sleep-for 0.2)
          (run-with-timer 1 nil (lambda () (delete-overlay overlay)))))))

  (defun fit-window-to-buffer* (arg)
    (interactive "P")
    (fit-window-to-buffer (next-window))))

(use-package window-x
  :if (version<= "31.0" emacs-version)
  :doc "https://p.bauherren.ovh/blog/tech/new_window_cmds")

(use-package winner
  :bind ( :map ctl-m-map ("<" . winner-undo ) )
  :init
  (setq winner-dont-bind-my-keys t)
  (winner-mode +1)

  :config
  (define-key winner-repeat-map ">" #'winner-redo)
  (define-key winner-repeat-map "<" #'winner-undo))

(use-package popper
  :disabled t
  :ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          magit-status-mode
          compilation-mode))
  (setq popper-display-function #'popper--display-buffer-on-right)

  (popper-mode +1)
  (popper-echo-mode +1)

  :preface
  (defun popper--display-buffer-on-right (buffer alist)
    (if (popper-popup-p (window-buffer))
        ;; Reuse the popper window for displaying another popup.
        (display-buffer-same-window buffer alist)
      ;; Create a popper window on the right side.
      (-> buffer
          (display-buffer-in-direction (cons (cons 'direction 'right)
                                             alist))
          (select-window)))))

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
  (define-repeat-map repeat/mwim
    ("TAB" . mwim)))

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
  :bind ( ("M-g w" . avy-goto-word-1)

          :map ctl-period-map
          ("C-." . avy-goto-char-timer)

          :map dired-mode-map
          ("j" . avy-goto-char-timer) )
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

(use-package ace-window
  :doc
  "This should come after `window's use-package
  declaration. Otherwise, `window' would overwrite the binding for \\[ace-window]]."
  :ensure t
  :doc "Use `ace-window' instead of `other-window'."
  :custom ( (aw-keys (list ?s ?d ?f ?j ?k ?l))
            (aw-char-position 'left) )
  :bind ("C-x o" . ace-window))

;; ――――――――――――――――――――――――――――――――――――――――

(use-package recentf
  :custom ( (recentf-auto-cleanup 'mode)
            (recentf-keep '(file-remote-p file-readable-p))
            (recentf-exclude '("\.gpg$"))
            (recentf-max-saved-items 10000)
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
  :commands (dictionary-word-at-mouse-event dictionary-definition)
  :bind ( :map global-map
          ;; ([double-down-mouse-1] . dictionary--word-def*)

          :map ctl-quote-map
          ("d" . dictionary-search) )

  :init
  (setq dictionary-server "dict.org")
  (add-to-list 'context-menu-functions 'context-menu-dictionary))

(use-package wiki-summary
  :load-path "etc/"
  :bind ( :map ctl-quote-map ("w" . wiki-summary) )
  :ensure t
  :config
  (ensure-windows-on-right "\\*wiki"))

(use-package flyspell
  :delight flyspell-mode
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

          ;; (prog-mode . flyspell-prog-mode)

          )
  :init
  (setq flyspell-auto-correct-binding (kbd "C-:"))

  :config
  (setq flyspell-delay 5))

(use-package ispell
  :defer t
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary personal-dictionary-file))

(use-package jinx
  :ensure t
  :delight jinx-mode
  :bind ( :map ctl-period-map
          ("f" . jinx-correct-word)

          :map global-map
          ("M-$" . jinx-correct)
          ("C-M-$" . jinx-languages)  )

  :custom
  (jinx-languages "en_US de")


  :init
  (global-jinx-mode +1)

  ;; Another interesting package that looks promising and might improve further
  ;; in the future is harper. It provides an LSP server that can be used to
  ;; correct grammar and spelling mistakes in buffers that have plain text.
  ;; ---
  ;; (if (executable-find "harper-ls")
  ;;     (hook-into-modes #'--eglot-ensure 'markdown-mode 'text-mode 'org-mode)
  ;;   (message "harper-ls isn't installed on system."))

  :config
  (eval-when-compile
    (require 'vertico-multiform))

  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20))))

;; ──────────────────────────────────────────────────────────────────

(use-package popup
  ;; Only to add an autoload for this function (which isn't a command).
  :commands (popup-tip popup-clear-all-popups)
  :defer t
  :ensure t
  :preface
  (defun popup-clear-all-popups ()
    (interactive)
    (dolist (popup popup-instances)
      (popup-delete popup))))

;;; Completion at Point
;; ――――――――――――――――――――――――――――――――――――――――

(use-package completion-preview
  :delight completion-preview-mode
  :init
  (global-completion-preview-mode +1)

  :bind ( :map completion-preview-active-mode-map
          ("C-i" . completion-preview-insert)
          ("M-i" . completion-preview-complete)
          ("M-n" . completion-preview-next-candidate)
          ("M-p" . completion-preview-prev-candidate) )

  :custom
  (completion-preview-overlay-priority 1200)
  (completion-preview-exact-match-only t))


(use-package corfu
  :ensure t
  :bind ( :map global-map
          ("TAB" . indent-for-tab-command)

          :map corfu-map
          ("TAB" . corfu-expand)
          ("RET" . corfu-complete)
          ("C-n" . corfu-next)
          ("C-p" . corfu-previous) )

  :hook
  (after-init . global-corfu-mode)
  (prog-mode . (lambda () (setq-local corfu-auto t)))

  :custom
  (corfu-preselect 'first)
  (corfu-auto-delay 1.0)
  (corfu-left-margin-width 1.0)
  (corfu-right-margin-width 1.0)

  :config
  (use-package prescient
    :ensure t
    :bind ( :map corfu-map ("M-s" . prescient-toggle-map) )
    :init
    (prescient-persist-mode +1)
    (add-to-list 'completion-styles 'prescient))

  (use-package corfu-prescient
    :ensure t
    :after corfu
    :custom (corfu-prescient-enable-filtering t)
    :init
    (corfu-prescient-mode +1)))

(use-package company
  :ensure t
  :delight company-mode
  ;; :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 1.0)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-offset-display 'lines)
  (company-format-margin-function nil)

  :bind ( :map company-active-map
          ("C-j"   . company-complete-selection)
          ("C-n"   . company-select-next-or-abort)
          ("C-p"   . company-select-previous-or-abort) )

  :config
  ;; https://www.reddit.com/r/emacs/comments/nichkl/how_to_use_different_completion_styles_in_the/
  (advice-add 'company-capf
              :around
              (lambda (capf-fn &rest args)
                (let ((completion-styles '(basic partial-completion)))
                  (apply capf-fn args)))))

(use-package company-prescient
  :ensure t
  :hook (company-mode . company-prescient-mode))

;; ──────────────────────────────────────────────────────────────────

(use-package dired
  :custom (dired-kill-when-opening-new-dired-buffer t)
  :config
  (use-package dired-hacks
    :vc  ( :url "https://github.com/Fuco1/dired-hacks.git"
           :rev :newest )
    :hook (dired-mode . dired-collapse-mode)
    :init
    (require 'dired-collapse)))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("C-c u" . dired-up-directory)
         ("M-<"   . dired-to-first-entry)
         ("M->"   . dired-to-last-entry)
         ("z"     . kill-buffer-delete-window)
         ("f"     . project-find-file)
         ("~"     . dired-go-home)
         ("P"     . dired-goto-project-root))
  :hook ( (dired-after-readin . dired-hide-details-mode)
          (dired-mode         . hl-line-mode)
          (dired-mode         . dired-omit-mode) )
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-hide-details-hide-information-lines nil)

  :config
  (define-repeat-map repeat/dired-up-directory
    ("u" . dired-up-directory))

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
    (dired-jump nil (expand-file-name "~/")))

  (defun dired-goto-project-root ()
    (interactive)
    (dired-jump nil (expand-file-name (project-root (project-current))))))

(use-package casual-suite
  :ensure t
  :defer t
  :bind ("C-h z" . pick--casual-suite)
  :preface
  (defun pick--casual-suite ()
    (interactive)
    (let* ((transient-menus)
           (selected-menu))
      (mapatoms
       (lambda (symbol)
         (when (string-match "^casual.*-tmenu$" (symbol-name symbol))
           (push (cons (symbol-name symbol) symbol)
                 transient-menus))))
      (-> (completing-read "Menu:" transient-menus)
          (assoc-default transient-menus)
          (call-interactively)))))

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
  :delight abbrev-mode
  :init
  (setq-default abbrev-mode t
                save-abbrevs t
                abbrev-file-name (expand-file-name "lib/abbrevs/abbrev-defs"
                                                   user-emacs-directory))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)
    (make-empty-file abbrev-file-name))


  :config
  (advice-add 'abbrev-insert
              :after-while
              (lambda (abbrev-symbol _ start _end)
                (let ((end (+ start (length (symbol-value abbrev-symbol)))))
                  (pulse-momentary-highlight-region start end)))))



(use-package yasnippet
  :defer 2
  :ensure t
  :delight yas-minor-mode
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
  (yas-global-mode +1)

  ;; Tree Sitter major modes and their corresponding non-tree-sitter modes
  ;; should share the same snippets.
  (add-hook 'python-ts-mode
            (lambda () (yas-activate-extra-mode 'python-mode)))
  (add-hook 'rust-ts-mode
            (lambda () (yas-activate-extra-mode 'rust-mode))))

(use-package yasnippet-snippets
  :doc
  "Extra snippets for yas-snippet!"
  :ensure t
  :after yasnippet

  :preface
  ;; Use tree-siter to precisely figure out when these snippets need to be
  ;; expanded.
  (defvar auto-expand-snippets-list (list "do")
    "List of snippets that we try to expand.")
  (defvar auto-expand-trigger-events (list ?  'return))
  (make-variable-buffer-local 'auto-expand-snippets-list)

  (defun auto-expand-snippets-expand ()
    (interactive)
    (when (and auto-expand-snippets-list
               (memql last-input-event auto-expand-trigger-events)
               (looking-back (rx-to-string `(seq (or ,@auto-expand-snippets-list)
                                                 (* space)))))
      (re-search-backward (rx-to-string '(not space)))
      (forward-char 1)
      (yas-expand)))

  (defun enable-auto-expand-snippets (snippets)
    (setq-local auto-expand-snippets-list snippets)
    (add-hook 'post-self-insert-hook #'auto-expand-snippets-expand)))

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
  :bind ( :map ledger-mode-map ("+" . hledger-increment-amount) )
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
  :bind ( ("C-c j" . hledger-run-command)

          :map hledger-mode-map
          ("C-c +" . hledger-increment-amount)

          :map hledger-view-mode-map
          ("_" . hledger-view-selective-display)
          ("+" . set-selective-display)
          ("s" . chart-numbers-on-line)
          ("z" . calc-store-numbers-on-line))

  :init
  (use-package async :ensure t :defer t)

  :config
  (setq hledger-jfile
        (expand-file-name "~/miscellany/personal/finance/accounting.journal"))

  (add-hook 'hledger-mode-hook
            (lambda ()
              (require 'company)
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company)
              (setq-local completion-styles '(basic partial-completion))
              (setq corfu-auto-delay 0.1)
              (add-to-list 'completion-at-point-functions
                           (cape-company-to-capf 'hledger-company))))

  (add-hook 'hledger-view-mode-hook #'hl-line-mode)
  (add-hook 'hledger-view-mode-hook
            (lambda ()
              (run-with-timer 0.5
                              nil
                              (lambda ()
                                (when (get-buffer-window hledger-reporting-buffer-name)
                                  (with-current-buffer hledger-reporting-buffer-name
                                    (center-text-for-reading)))))))

  :preface
  (defun hledger-view-selective-display ()
    (interactive)
    (let ((column (save-excursion
                    (beginning-of-line)
                    (re-search-forward "[[:alnum:]]")
                    (current-column))))
      (set-selective-display (1+ column)))))


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

  :preface
  (defun hledger-get-balance (account)
    (let ((balance-output (hledger-shell-command-to-string (format " balance -N %s " account))))
      (string-match "[0-9,]+\.?[0-9]*" balance-output)
      (string-to-number (string-replace "," "" (match-string 0 balance-output)))))

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
          ("C-c r f" . eglot-format)

          :map ctl-m-map
          ("g" . eglot-code-actions) )
  :init
  ;; Default: 4KB is too low for LSP servers.
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024))

  (hook-into-modes #'--eglot-ensure
      'clojure-mode 'clojure-ts-mode
      'java-mode 'java-ts-mode
      'rust-mode 'rust-ts-mode
      'python-mode 'python-ts-mode
      'go-mode 'go-ts-mode
      'c-mode 'c-ts-mode
      'c++-mode 'c++-ts-mode
      'ruby-mode 'ruby-ts-mode
      'typescript-mode 'typescript-ts-mode
      'elixir-ts-mode
      'terraform-mode
      'lua-ts-mode)

  ;; `eglot' changes the `eldoc-documentation-strategy' to a value that I do not
  ;; like. Ask `elgot' to stop messing with `eldoc' and set these parameters
  ;; separately in a hook.
  (setq eglot-stay-out-of '(eldoc-documentation-strategy))
  (setq eglot-inlay-hints-mode nil)
  (setq eglot-connect-timeout 300)
  (setq eglot-autoshutdown t)
  (setq eglot-code-action-indications '(eldoc-hint))

  :config
  (dolist (lang-server-spec
           `(((c-mode c++-mode)        . ("clangd"))
             ((ruby-mode ruby-ts-mode) . ("bundle" "exec" "solargraph" "stdio"))
             ((rust-mode rust-ts-mode) . ("rustup" "run" "stable" "rust-analyzer"))
             ((text-mode markdown-mode org-mode) . ("harper-ls" "--stdio"))
             (scala-mode               . (,(expand-file-name "bin/metals.sh" user-emacs-directory)))
             ((java-mode java-ts-mode) . ,#'java-eclipse-jdt-launcher)
             (elixir-ts-mode           . ,#'elixir-lsp-launcher)
             (jsonnet-mode             . ("jsonnet-language-server"))))
    (add-to-list 'eglot-server-programs lang-server-spec))

  :preface
  (defun --eglot-ensure ()
    (when-let ((project-root (project-current)))
      (eglot-ensure))))


(use-package dape
  :vc ( :url "https://github.com/svaante/dape.git"
        :rev :neweset)
  :commands dape
  :defer t)

(use-package treesit
  :defer t
  :when (treesit-available-p)
  :bind (("M-g u" . --treesit-backward-up) )
  :doc
  "[2023-02-08 Wed 22:48] Tree-sitter support is now built into Emacs.
   The directory where shared libraries for language grammars are
   installed is not configurable yet (fixed to tree-sitter under
   `user-emacs-directory'."

  :init
  (dolist (lang '( bash c c++ css elixir go html java js json lua markdown
                   python ruby rust scala typescript ))
    (add-to-list 'major-mode-remap-alist
                 (cons (intern (format "%s-mode" lang))
                       (intern (format "%s-ts-mode" lang)))))

  (define-repeat-map repeat/treesit-backward-up
    ("u" . --treesit-backward-up))

  :config
  ;; Official grammar implementations
  (dolist (source
           '( (bash "tree-sitter-bash")
              (c "tree-sitter-c")
              (cpp "tree-sitter-cpp")
              (css "tree-sitter-css")
              (go "tree-sitter-go")
              (html "tree-sitter-html")
              (java "tree-sitter-java")
              (javascript "tree-sitter-javascript" "master" "src")
              (python "tree-sitter-python")
              (ruby "tree-sitter-ruby")
              (rust "tree-sitter-rust")
              (scala "tree-sitter-scala")
              (tsx "tree-sitter-typescript" "master" "tsx/src")
              (typescript "tree-sitter-typescript" "master" "typescript/src")))
    (setf (cadr source)
          (format "https://github.com/tree-sitter/%s" (cadr source)))
    (add-to-list 'treesit-language-source-alist source))

  ;; Community maintained grammar implementations
  (dolist (source
           '((heex "https://github.com/phoenixframework/tree-sitter-heex" "main")
             (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main")
             (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
             (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
             (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
             ;; --
             ;; (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
             ;; (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
             ;; --
             ))
    (add-to-list 'treesit-language-source-alist source))

  (dolist (source treesit-language-source-alist)
    (install-tree-sitter-grammer-if-required (car source)))

  :preface
  (defun install-tree-sitter-grammer-if-required (language)
    "Given a language symbol install tree-sitter grammer if not
    already avaialble."
    (unless (treesit-language-available-p language)
      (message "Installing tree-sitter grammar for %s" language)
      (treesit-install-language-grammar language)))

  (defvar --treesit-highlight-overlay nil)

  ;; The following pattern where I add a function to a `{pre-post}-command-hook'
  ;; for a execution exactly once is very common. I should probably abstract
  ;; into something reusable.
  (defun --treesit-remove-overlay-hook ()
    (delete-overlay --treesit-highlight-overlay)
    (remove-hook 'pre-command-hook #'--treesit-remove-overlay-hook))

  (defun --treesit-backward-up (&optional highlight)
    "Inspired by `paredit-backward-up'."
    (interactive)
    (letrec ((original-point (point))
             (current-node (treesit-node-on (point) (point))))
      (setq --treesit-highlight-overlay
            (make-overlay (point) (point)))
      (overlay-put --treesit-highlight-overlay 'face 'highlight)
      (while (and current-node (= (point) original-point))
        (setq current-node (treesit-node-parent current-node))
        (when current-node
          (let ((start (treesit-node-start current-node))
                (end (treesit-node-end current-node)))
            (goto-char start)
            ;; Push mark only for the first time so that we can get back to
            ;; where we started the tree traversal from.
            (unless (eq this-command last-command)
              (push-mark end))
            (move-overlay --treesit-highlight-overlay start end))))
      ;; Before executing the next command remove the overlay. Adding to
      ;; `post-command-hook' => execution right after this function returns.
      (add-hook 'pre-command-hook #'--treesit-remove-overlay-hook))))

(use-package treesit-fold
  :vc ( :url "https://github.com/emacs-tree-sitter/treesit-fold"
        :rev :newest )
  :bind ( :map ctl-m-map ("TAB" . treesit-fold-toggle) )
  :delight treesit-fold-mode
  :init
  (global-treesit-fold-mode +1))

(use-package combobulate
  :vc ( :url "https://github.com/mickeynp/combobulate.git"
        :rev :newest )
  :bind ("M-g d" . combobulate))

(use-package ts-movement
  :vc ( :url "https://github.com/haritkapadia/ts-movement.git"
        :rev :newest )
  :disabled t)


;;; ----------------------------------------------------------------------------

(use-package display-line-numbers
  :bind ( :map ctl-period-map ([\C-m] . display-line-columns-dwim) )
  :preface
  (defun display-line-columns-dwim (arg)
    (interactive "p")
    (pcase arg
      (4 (display-fill-column-indicator-mode +1))
      (16 (progn
            (display-fill-column-indicator-mode -1)
            (display-line-numbers-mode -1)))


      (_ (display-line-numbers-mode +1)))))

(use-package type-break
  :disabled t
  :bind (:map ctl-quote-map ("b" . type-break) )
  :init
  (setq type-break-file-name nil)
  (setq type-break-mode-line-message-mode t)

  (type-break-mode +1))

(use-package compile
  :defer t
  :bind ( :map compilation-mode-map ("u" . rename-uniquely ))
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

  (setq compilation-scroll-output t
        compilation-always-kill t)

  (ensure-windows-on-right "\\*compilation\\*")

  ;; Switch to compilation buffer after starting compilation
  ;; (add-hook 'compilation-start-hook
  ;;           (lambda (process)
  ;;             (when-let* ((w (get-buffer-window (process-buffer process))))
  ;;               (select-window w))))
  )

(use-package eldoc
  :delight eldoc-mode
  :bind ( :map ctl-m-map
          ("d" . toggle-eldoc-doc-buffer)
          ("D" . freeze-eldoc-buffer) )
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (global-eldoc-mode +1)

  (setq-default eldoc-documentation-strategy
                #'eldoc-documentation-compose-eagerly)

  (advice-add #'eldoc-display-in-buffer
              :after
              (lambda (&rest _args)
                (when (memq major-mode '(python-mode python-ts-mode))
                  ;; basedbyright adds HTML entities, e.g. &nbsp; that are not
                  ;; rendered properly by markdown-mode in an eldoc buffer. This
                  ;; replaces those literal strings with chars that represents
                  ;; them visually.
                  (require 'mm-url)
                  (with-current-buffer (eldoc-doc-buffer)
                    (let ((inhibit-read-only t))
                      (mm-url-decode-entities))))))

  :preface
  (defun toggle-eldoc-doc-buffer ()
    "Depends upon internal details of `eldoc-mode'."
    (interactive)
    (if-let ((w (some-window (lambda (w) (eq (window-buffer w)
                                             eldoc--doc-buffer)))))
        (progn (delete-window w)
               (when-let* ((last-window-configuration (get this-command :window-configuration)))
                 (set-window-configuration last-window-configuration)))
      (put this-command :window-configuration (current-window-configuration))
      (delete-other-windows)
      (display-buffer (eldoc-doc-buffer))))


  (defun freeze-eldoc-buffer ()
    "Capture the current contents of the *eldoc* buffer. Make sure
    that the newly created buffer is visible at all times instead of
    the default *eldoc* buffer."
    (interactive)
    (with-current-buffer eldoc--doc-buffer
      (let ((contents (buffer-substring (point-min)
                                        (point-max)))
            (doc-buffer (get-buffer-create "*Doc Buffer*"))
            (eldoc-buffer-window (car (get-buffer-window-list))))
        (with-current-buffer doc-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert contents))
          (goto-char (point-min))
          (help-mode))
        (set-window-buffer eldoc-buffer-window doc-buffer))))

  (defun eldoc-documentation-using-link-at-point (cb)
    (let (url)
      (when-let* ((url-at-point (or (and (fboundp 'markdown-link-url)
                                         (markdown-link-url)))))
        (setq url url-at-point))
      (when url
        (request url
          :parser
          (lambda ()
            (shr-render-region (point-min) (point-max))
            (buffer-substring (point-min) (point-max)))

          :success
          (cl-function
           (lambda (&key data &allow-other-keys)
             (funcall cb data)))

          :error (lambda () (message "Failed to fetch url: " url)))
        ;; Expect a response asynchronously when `:success' callback is executed.
        t))))

(use-package which-func
  :disabled t
  :doc "Display the current function in the mode line."
  :config
  (setq which-func-modes '(java-mode))
  (setq which-func-unknown "<λ>")
  (which-function-mode +1))

(use-package gud
  :custom
  (gdb-show-main t)
  ;; Change this to `t' or use command `gdb-many-windows' to
  ;; have a fancier IDE like UI.
  (gdb-many-windows nil)
  (gud-gdb-command-name "rr replay -i=mi"))

(use-package realgud
  :doc "`gud' with bells and whistles."
  :disabled t
  :defer t
  :ensure t)

(progn
  ;; indent.el doesn't have a `(provide 'ident)' at the end.
  (load "indent")
  (setq-default tab-always-indent 'complete))

(use-package xref
  :doc "Find definitions like the coolest kid."
  :ensure t
  :bind ( ("M-." . xref-find-definitions)
          ("M-," . xref-go-back) )
  :config
  (define-key xref--xref-buffer-mode-map (kbd "W") #'delete-other-windows))

(use-package subword
  ;; :hook (prog-mode . subword-mode)
  :delight subword-mode
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
  (define-repeat-map repeat/help-at-pt
    ("<" . scan-buf-previous-region)
    (">" . scan-buf-next-region)
    ("." . display-help-at-pt-dwim))

  :preface
  (defun display-help-at-pt-dwim (&optional prefix)
    (interactive "P")
    (when-let* ((help (or (help-at-pt-kbd-string)
                          (progn (scan-buf-next-region (if prefix -1 +1))
                                 (help-at-pt-kbd-string)))))
      (popup-tip help
                 :point (point)
                 :around t
                 :margin 2))))

(use-package flymake
  :delight flymake-mode
  :bind ( :map flymake-mode-map
          ("M-g n" . flymake-goto-next-error)
          ("M-g p" . flymake-goto-prev-error) )
  :custom ( flymake-no-changes-timeout 1.0) )

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

     ((memq 'flycheck-mode local-minor-modes)
      (flycheck-list-errors))

     (t (message "No minor mode to run a linter.")))))

(use-package highlight-indent-guides
  :ensure t
  :delight highlight-indent-guides-mode
  :bind ( :map ctl-period-map ("C-l" . highlight-indent-guides-mode) )
  :config
  (setq highlight-indent-guides-method 'bitmap
        highlight-indent-guides-responsive nil

        highlight-indent-guides-auto-odd-face-perc 3
        highlight-indent-guides-auto-even-face-perc 3
        highlight-indent-guides-auto-character-face-perc 3))

(use-package indent-bars-ts
  :vc ( :url "https://github.com/jdtsmith/indent-bars"
        :rev :newest )
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
                                      list list_comprehension
                                      dictionary dictionary_comprehension
                                      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode)
  :config
  ;; https://github.com/jdtsmith/indent-bars/blob/main/examples.md#minimal-colorpop
  (setq indent-bars-color '(highlight :face-bg t :blend 0.15)
        indent-bars-pattern "."
        indent-bars-width-frac 0.1
        indent-bars-pad-frac 0.1
        indent-bars-zigzag nil
        indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
        indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
        indent-bars-display-on-blank-lines t))

(use-package track-changes
  :config
  (setq track-changes-record-errors nil))

(use-package aggressive-indent
  :ensure t
  :defer t
  :delight aggressive-indent-mode
  :hook ((emacs-lisp-mode racket-mode scheme-mode cc-mode rust-mode) . aggressive-indent-mode))

(use-package comint
  :defer t
  :config
  (setq comint-scroll-show-maximum-output nil))

(use-package vterm
  :ensure t
  :custom (vterm-max-scrollback 100000)
  :bind ( :map vterm-mode-map
          ("M-p" . --vterm-ctrl-p)
          ("M-n" . --vterm-ctrl-n) )
  :preface
  (defun --vterm-ctrl-p ()
    (interactive)
    (vterm-send-key "p" nil nil t))

  (defun --vterm-ctrl-n ()
    (interactive)
    (vterm-send-key "n" nil nil t)))

(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (add-to-list 'exec-path-from-shell-variables "SRC_ACCESS_TOKEN")
  (add-to-list 'exec-path-from-shell-variables "SRC_ENDPOINT")
  (exec-path-from-shell-initialize))

(use-package shell
  :custom ( explicit-shell-file-name "bash" )
  :bind ( :map ctl-quote-map ([C-m] . shell-toggle* ) )
  :init
  (add-hook 'shell-mode-hook #'--shell-mode-kill-buffer-on-exit )

  :preface
  (defun shell-toggle* (arg)
    (interactive "P")
    (let ((directory default-directory)
          (text-in-region
           (when (region-active-p)
             (buffer-substring (region-beginning) (region-end)))))
      (if (eq major-mode 'shell-mode)
          (set-window-configuration (get this-command :window-config))
        (put this-command :window-config (current-window-configuration))
        (shell)
        (when arg
          (insert (format "; cd %s ; \n" (shell-quote-argument directory))))
        (when text-in-region
          (insert text-in-region)))))

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
  :bind ( :map ctl-quote-map ("C-p" . eshell-toggle) )
  :hook ( eshell-exit-hook . delete-window-if-dedicated-to-eshell )
  :preface
  (defun delete-window-if-dedicated-to-eshell ()
    (when (< 1 (count-windows))
      (delete-window)))

  (defun eshell/restart ()
    (interactive)
    (let ((directory default-directory))
      (kill-buffer-delete-window )
      (with-current-buffer (eshell)
        (eshell/cd directory)
        (eshell-send-input "\n"))))

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

  :config
  (require 'em-hist)
  (setq eshell-modules-list
        '( eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs
           eshell-extpipe eshell-glob eshell-hist eshell-ls eshell-pred
           eshell-prompt eshell-script eshell-term eshell-tramp
           eshell-unix eshell-xtra eshell-elecslash
           ;; Earlier disabled because it's hard to get used to it.
           ;;          See: (info "(eshell) Smart scrolling")
           ;; eshell-smart
           ))

  (setq eshell-prompt-regexp "([^#$]*) [#$] "
        eshell-prompt-function
        (lambda ()
          (concat "(" (abbreviate-file-name (eshell/pwd)) ")"
                  (if (= (user-uid) 0) " # " " $ ")))

        eshell-aliases-file
        (expand-file-name "./etc/eshell-aliases" user-emacs-directory))

  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; `orderless' doesn't make much sense for shell completion.
              (setq-local completion-styles '(basic partial-completion emacs22))
              ;; Disable pixel scrolling to make sure that prompt is always
              ;; fully visible even when it is present at the bottom edge of the
              ;; screen.
              (completion-preview-mode -1)
              (pixel-scroll-mode -1)
              (pixel-scroll-precision-mode -1)))

  (add-to-list 'eshell-expand-input-functions #'eshell-expand-history-references))

(use-package eshell-atuin
  :ensure t
  :after eshell
  :hook (eshell-mode . eshell-atuin-mode)
  :config
  (advice-add 'eshell-atuin--get-input
              :filter-return
              (lambda (input)
                (unless (string-empty-p input)
                  input))))

(use-package eat
  :ensure t
  :bind (
         :map eat-mode-map
         ("<C-m> h" . --eat-hide-cursor)
         ;; ( "s-<return>" . --eat-toggle )
         ;; ( "M-S-<return>" . --eat-toggle )
         )

  :delight eat-eshell-mode
  :custom ( eat-kill-buffer-on-exit t
            eshell-visual-commands (list) )

  :preface
  (defun --eat-hide-cursor ()
    (interactive)
    (setq-local cursor-type nil))

  (defun --eat-toggle ()
    (interactive)
    (if (eq major-mode 'eat-mode)
        (set-window-configuration (get this-command :window-config))
      (put this-command :window-config (current-window-configuration))
      (eat))))


(use-package yequake
  :disabled t
  :ensure t
  :demand t
  :custom
  (yequake-frames
   '(("LLMShell"
      (buffer-fns . (yequake-gptel-session))
      (left . 1.0)
      (width . 0.30)
      (height . 1.0)
      (alpha . 1.0)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (visibility . t)
                           (sticky . t))))
     ("LLM"
      (buffer-fns . (llms-explain-image-with-context))
      (left . 1.0)
      (width . 0.30)
      (height . 1.0)
      (alpha . 1.0)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (visibility . t)
                           (sticky . t))))))
  :config
  (require 'llms)
  (require 'llms-chat)

  :preface
  (defun yequake-gptel-session ()
    (let ((gptel-backend llms-chat-gptel-deepseek-backend)
          (gptel-model 'deepseek-chat))
      (gptel " *yequake GPTEL* ")))

  (defun yequake-screenshot-and-toggle-llm ()
    (let* ((image-file (make-temp-file "image-with-context-" nil ".jpg"))
           (llms-explain-image--input-image image-file))
      (shell-command (format "%s -p -f %s"
                             llms-screenshot-command
                             (shell-quote-argument image-file)))
      (yequake-toggle "LLM"))))

;;; DevOps
;; ──────────────────────────────────────────────────────────────────

(use-package sh-script
  :init
  (add-hook 'sh-mode
            (lambda ()
              (when (not (executable-find "shellcheck"))
                (message "Not installed on system: `shellcheck'!")))))


(use-package jinja2-mode
  :defer t
  :ensure t
  :mode  ("\\.jinja\\'" "\\.j2\\'"))

;;; HASKELL-MODE
;;  ─────────────────────────────────────────────────────────────────
(use-package haskell-mode
  :ensure t
  :defer t
  :bind (:map haskell-mode-map
              ("C-c C-k" . haskell-compile) )
  :hook ((haskell-mode . haskell-doc-mode)
         (haskell-mode . haskell-indentation-mode)))


;;; Proofs
;; ──────────────────────────────────────────────────────────────────

(use-package proof-general
  :doc "https://github.com/jwiegley/dot-emacs/blob/master/init.org#proof-general"
  :ensure t
  :demand t
  :custom
  (proof-three-window-mode-policy 'hybrid)
  (proof-auto-action-when-deactivating-scripting 'retract)
  (proof-autosend-enable nil)
  (proof-electric-terminator-enable t)
  (proof-fast-process-buffer nil)
  (proof-script-fly-past-comments t)
  (proof-shell-fiddle-frames nil)
  (proof-splash-enable nil)
  (proof-sticky-errors t)
  (proof-tidy-response t)

  :custom-face
  (proof-eager-annotation-face ((t nil)))
  (proof-locked-face ((t (:background "#180526"))))
  (proof-omitted-proof-face ((t (:extend t :background "#23103c"))))
  (proof-queue-face ((t (:background "#431807"))))
  (proof-script-sticky-error-face ((t (:background "#50110e"))))
  (proof-warning-face ((t (:background "orange4"))))

  :functions
  ( proof-layout-windows proof-prf )

  :preface
  (defun my-layout-proof-windows ()
    (interactive)
    (if (< 1 (length (window-list)))
        (progn
          (switch-to-buffer proof-script-buffer)
          (delete-other-windows)
          (my-layout-proof-windows))
      (let ((window (selected-window)))
        (split-window-right)
        (switch-to-buffer proof-response-buffer)
        (split-window-below)
        (switch-to-buffer proof-goals-buffer)
        (select-window window)
        (proof-prf))))

  :config
  (use-package coq-mode
    :bind (:map coq-mode-map
                ("M-RET"       . proof-goto-point)
                ("RET"         . newline-and-indent)
                ("C-c h")
                ("C-c C-p"     . my-layout-proof-windows)
                ("C-c C-a C-s" . coq-Search)
                ("C-c C-a C-a" . coq-Search)
                ("C-c C-a C-r" . coq-SearchRewrite)
                ("C-c q"       . my-add-qed)
                ("C-c d"       . my-toggle-coq-diffs))

    :custom
    (coq-compile-auto-save 'save-coq)
    (coq-compile-before-require t)
    (coq-compile-parallel-in-background t)
    (coq-holes-minor-mode nil)
    (coq-maths-menu-enable t)
    (coq-one-command-per-line nil)
    (coq-prefer-top-of-conclusion t)
    (coq-prog-args '("-emacs"))

    :custom-face
    (coq-symbol-face ((t (:inherit default-face))))

    :preface
    (eval-when-compile (defvar proof-assistant nil))

    (defun my-add-qed ()
      (interactive)
      (unless (looking-back (rx-to-string '(seq line-start (* space)))
                            (line-beginning-position))
        (open-line 1)
        (next-line))
      (insert "Qed")
      (call-interactively (key-binding "."))
      (sleep-for 0.2)
      (call-interactively indent-line-function))

    (defun my-toggle-coq-diffs ()
      (interactive)
      (setq coq-diffs (if (eq coq-diffs 'off) 'on 'off))
      (proof-prf))

    :init
    (add-hook 'coq-mode-hook
              (lambda ()
                (holes-mode -1)
                (abbrev-mode -1)
                (sub-paredit-mode -1)))))


(use-package company-coq
  :after coq
  :ensure t
  :commands company-coq-mode
  :bind (:map company-coq-map
              ;; ("<tab>" . company-complete)
              ("M-<return>"))
  :bind (:map coq-mode-map
              ("C-M-h" . company-coq-toggle-definition-overlay))
  :hook (coq-mode . company-coq-mode)
  :custom
  (company-coq-disabled-features
   '(hello prettify-symbols smart-subscripts dynamic-symbols-backend))
  (company-coq-prettify-symbols-alist
   '(("|-"     . 8866)
     ("True"   . 8868)
     ("False"  . 8869)
     ("->"     . 8594)
     ("-->"    . 10230)
     ("<-"     . 8592)
     ("<--"    . 10229)
     ("<->"    . 8596)
     ("<-->"   . 10231)
     ("==>"    . 10233)
     ("<=="    . 10232)
     ("++>"    . 10239)
     ("<++"    . 11059)
     ("fun"    . 955)
     ("forall" . 8704)
     ("exists" . 8707)
     ("/\\"    . 8743)
     ("\\/"    . 8744)
     ("~"      . 172)
     ("+-"     . 177)
     ("<="     . 8804)
     (">="     . 8805)
     ("<>"     . 8800)
     ("*"      . 215)
     ("++"     . 10746)
     ("nat"    . 120029)
     ("Z"      . 8484)
     ("N"      . 8469)
     ("Q"      . 8474)
     ("Real"   . 8477)
     ("bool"   . 120121)
     ("Prop"   . 120031)))
  :custom-face
  (company-coq-features/code-folding-bullet-face ((t (:weight bold))))
  :functions (cape-company-to-capf)
  :config
  (add-hook 'company-coq-mode-hook
            #'(lambda ()
                ;; (company-mode -1)
                (require 'cape)
                (setq-local completion-at-point-functions
                            (mapcar #'cape-company-to-capf
                                    company-coq-enabled-backends)))))


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

(use-package mermaid-mode
  :ensure t)

(use-package org-agenda
  :defer t
  :bind ( :map org-agenda-mode-map
          ("T"       . org-agenda-toggle-toggle-tags-column)
          ("a"       . org-agenda-redo-with-days-to-deadline)
          ("g"       . org-agenda-redo)
          ("r"       . org-agenda-redo-all)
          ("M-."     . org-agenda-goto-today*)
          ("C-o"     . org-agenda-list)
          ("C-S-o"   . custom-agenda-view)
          ("x"       . org-agenda-quit)
          ("C-c C-r" . org-agenda-refile)
          ("V" . org-review-captures)
          ("C-c C-n" . take-notes)
          ("C-c C-f" . org-agenda-find-file)
          ("C-c C-s" . org-schedule-and-todo)
          ("C-c C-d" . org-deadline-and-todo) ))

(use-package org-config
  :load-path "etc/"
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-config-capture)
         ("C-c a" . org-agenda)
         ("C-c n" . jump-to-notes-this-week!)

         ;; Bindings for using the Timer
         :map ctl-m-map
         ("C-o" . org-toggle-custom-agenda)
         ("x s" . org-timer-start)
         ("x S" . org-timer-stop)
         ("x c" . org-timer-set-timer)
         ("x ." . org-timer)
         ("l"   . lower-frame)
         ("L"   . raise-frame)


         :map ctl-quote-map
         ("C-n" . open-org-file)
         ("C-d" . search-notes-files)

         :map ctl-period-map
         ("C-n" . open-org-file)
         ("C-d" . search-notes-files)


         :map org-agenda-mode-map
         ("C-c g" . google-calendar-import-to-org) )
  :config
  ;; Set `diary-file' after `org-directory' has been set properly.
  (setq diary-file (expand-file-name "diary" org-directory))

  :init
  (dolist (buffer-regex '("\\` \\*Agenda Commands\\*\\'"
                          "\\`\\*Org Capture\\*\\'"
                          "\\`\\*Org Select\\*\\'"
                          "\\`\\*Org Tags\\*\\'"
                          "\\`\\*Org Links\\*\\'"))
    (add-to-list 'display-buffer-alist
                 `(,buffer-regex display-buffer-at-bottom)))

  (add-hook 'org-mode-hook
            (lambda ()
              (visual-line-mode +1)
              (visual-wrap-prefix-mode +1)
              (let ((typo-abbrevs-file (expand-file-name "lib/abbrevs/typos-defs"
                                                         user-emacs-directory)))
                (when (file-exists-p typo-abbrevs-file)
                  (make-thread (lambda ()
                                 (quietly-read-abbrev-file typo-abbrevs-file)))))))

  (bind-key "C-c a" #'org-agenda)
  (eval-after-load "org" '(require 'org-config))

  :preface
  (defun org-toggle-custom-agenda ()
    (interactive)
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-quit)
      (org-agenda nil "i"))))


(use-package org-timeblock
  :disabled t
  :after org
  :vc ( :url "https://github.com/ichernyshovvv/org-timeblock"
        :rev :newest )
  :init
  (use-package org-ql :ensure t :defer t))

(use-package xeft
  :after org-config
  :ensure t
  :bind ("M-s x" . xeft)
  :ensure-system-package (("/usr/include/xapian.h" . xapian-core))
  :custom ( xeft-directory (expand-file-name "notes/" org-directory)
            xeft-default-extension "org"
            xeft-recursive t
            xeft-database (expand-file-name "xeft.db" emacs-assets-directory)
            xeft-file-filter (lambda (file-path)
                               (string-equal "org" (file-name-extension file-path))))
  :custom-face
  ;; Thanks to @jwiegley:
  (xeft-excerpt-body
   ((t (:inherit default :foreground "grey80"))))
  (xeft-excerpt-title
   ((t (:inherit (bold underline) :foreground "sky blue"))))
  (xeft-inline-highlight
   ((t (:inherit underline :extend t :foreground "DarkOliveGreen2")))))

(use-package pomodoro
  :load-path "packages/rest/pomodoro"
  :defer 5
  :delight pomodoro-mode
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

(use-package burly
  :disabled t
  :ensure t)

(use-package desktop
  :disabled t
  :init
  (desktop-save-mode +1)

  :config
  (desktop-auto-save-enable))

(use-package desktop+
  :disabled t
  :ensure t
  :init
  (setq desktop+-base-dir (expand-file-name "var/desktops/"
                                            user-emacs-directory)))
;;; Regions
;;; ----------------------------------------------------------------------------
(use-package delsel :init (delete-selection-mode +1))
(use-package expand-region
  :ensure t
  :doc
  "Hyperbole provides similar functionality through
  hui-select.el but I have found `expand-region' to be more
  intuitive."
  :bind (:map ctl-period-map ("@" . er/expand-region)))

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
        shr-use-fonts t
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
  :custom ( enable-recursive-minibuffers t
            history-delete-duplicates t
            history-length 1000
            read-file-name-completion-ignore-case t
            completion-cycle-threshold 3)

  :config
  (minibuffer-depth-indicate-mode +1)
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (when (< 10 (minibuffer-depth))
                (top-level))))

  :init
  ;; `partial-completion' is very useful for `completion-at-point'
  ;; (advice-add 'completion-at-point
  ;;             :around
  ;;             (lambda (compl-at-point &rest args)
  ;;               (let ((completion-styles '(basic partial-completion)))
  ;;                 (apply compl-at-point args))))
  :preface
  (defun yank-symbol-to-minibuffer-or-kill-region (&optional arg)
    (interactive "P")
    (if (region-active-p)
        (call-interactively #'kill-region)
      (insert (with-minibuffer-selected-window
                (let ((starting-point (if (eq last-command this-command)
                                          (get this-command :starting-point)
                                        (put this-command :starting-point (point))))
                      (p (point)))
                  (if arg (forward-word 1) (forward-symbol 1))
                  (pulse-momentary-highlight-region starting-point (point))
                  (buffer-substring p (point))))))))

(use-package minibuffer-command-history
  :load-path "etc/"
  :commands minibuffer-command-history
  :hook (after-init . minibuffer-command-history-enable)
  :config
  (add-to-list 'savehist-additional-variables 'minibuffer-command-history))

(use-package cape
  :ensure t
  :init
  (dolist (cape-function '( cape-dict cape-history cape-keyword cape-emoji ))
    (add-hook 'completion-at-point-functions cape-function)))

(use-package vertico
  :doc "https://github.com/minad/vertico/wiki"
  :ensure t
  :bind ( :map ctl-m-map ("r" . vertico-repeat) )
  :hook (minibuffer-setup . vertico-repeat-save)
  :init (vertico-mode +1))

(use-package vertico-multiform
  :after vertico
  :bind ( :map vertico-map
          ("M-i"   . vertico-multiform-vertical)
          ("M-G"   . vertico-multiform-grid)
          ("M-B"   . vertico-multiform-buffer)
          ("<tab>" . vertico-insert) )
  :custom
  (vertico-multiform-categories
   '((imenu buffer)))

  (vertico-multiform-commands
   '((consult-imenu buffer)
     (consult-grep buffer)
     (consult-grep-dwim buffer)
     (consult-git-grep buffer)
     (consult-ripgrep buffer)
     (xref-find-references buffer)

     ;; -- here: nil => vertical
     (consult-line)
     (consult-yank-pop reverse)

     (embark-bindings grid reverse)
     (describe-symbol grid reverse)
     (describe-variable grid reverse)
     (describe-function grid reverse)
     (execute-extended-command unobtrusive)))

  :init
  (vertico-multiform-mode +1))

(use-package vertico-quick
  :after vertico
  :bind ( :map vertico-map ("M-q" . vertico-quick-jump) ))

(use-package vertico-prescient
  :ensure t
  :init
  (vertico-prescient-mode +1))

(use-package marginalia
  :ensure t
  :bind ( :map minibuffer-local-map
          ("M-A" . marginalia-cycle) )
  :init
  (marginalia-mode +1)

  :config
  ;; Keep built-in annotators of the following categories
  (let ((categories '(file)))
    (mapc (lambda (annotator)
            (when (memq (car annotator) categories)
              (setcdr annotator
                      (cons 'builtin    ; pushing 'builtin to the front
                            (remq 'builtin (cdr annotator))))))
          marginalia-annotators)))

(use-package embark
  :ensure t
  :bind ( ("C-c x" . embark-act)

          :map minibuffer-mode-map
          ("C-c C-o" . embark-collect)
          ("C-c C-e" . embark-export)) )

(use-package consult
  :ensure t
  :custom (consult-preview-key "M-.")
  :bind
  ( :map global-map
    ("M-s f"   . consult-find)
    ("M-s M-s" . consult-grep-dwim)
    ("C-x b"   . consult-buffer)
    ("M-y"     . consult-yank-pop)

    :map ctl-x-map
    ("<C-m>" . consult-grep-dwim)

    :map isearch-mode-map
    ("C-." . consult-line)

    :map minibuffer-local-map
    ("M-r" . consult-history)

    :map ctl-m-map
    ("z" . consult-complex-command)

    :map ctl-quote-map
    ("o n" . org-record-a-note!)
    ("o N" . jump-to-notes-this-week!)
    ("o j" . org-jump-to-note!)
    ("C-'" . consult-imenu)

    :map ctl-period-map
    ("C-s" . consult-line)

    :map dired-mode-map
    ("r"     . consult-ripgrep) )

  :preface
  (defun consult-grep-dwim (&optional directory initial)
    (interactive "P")
    (require 'vc)
    (if (eq 'Git (vc-deduce-backend))
        (call-interactively #'consult-git-grep)
      (call-interactively #'consult-grep)))

  (defun jump-to-notes-this-week! ()
    (interactive)
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (org-datetree-find-iso-week-create (calendar-current-date))
      (switch-to-buffer (current-buffer))))

  (defun org-record-a-note! (prefix-arg)
    (interactive "P")
    (let ((org-use-property-inheritance nil)
          (org-use-tag-inheritance nil))
      (consult-org-heading "+note_target" 'agenda)
      (org-narrow-to-subtree)
      (org-fold-show-subtree)
      (unless prefix-arg
        (forward-line 1)
        (open-line 1)
        (insert "- Noted on ")
        (org-insert-time-stamp (current-time) t t)
        (insert "\n  "))))

  (defun org-jump-to-note! ()
    (interactive)
    (let ((org-use-property-inheritance nil)
          (org-use-tag-inheritance nil))
      (consult-org-heading "+note_target" 'agenda)
      (org-narrow-to-subtree)
      (org-fold-show-subtree))))

(use-package embark-consult :ensure t :after embark)


;;; Programming Languages
;;; ──────────────────────────────────────────────────────────────────

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package connected-repl
  :load-path "packages/rest/connected-repl/"
  :commands (connected-repl-run connected-repl-run-on-project connected-repl-connect-manually)
  :config
  (add-to-list 'connected-repl-commands
               '(elixir-ts-mode . ("iex> " "iex" "-S" "mix")))

  (add-to-list 'connected-repl-commands
               '(scala-mode . ("scala> " "scala-cli" "repl"))))

(use-package java-mode
  :defer t
  :init
  (cl-defmethod eglot-handle-notification
    (server (_method (eql language/status)) &key type message &allow-other-keys)
    (when (equal type "Started")
      (message "LSP server ready: %s" (eglot-project-nickname server))))

  ;; See:
  ;; https://github.com/eclipse-jdtls/eclipse.jdt.ls/issues/2322#issuecomment-1423092394
  (add-to-list 'file-name-handler-alist '("\\`jdt://" . java-eclipse-jdt-file-name-handler))
  (add-to-list 'recentf-exclude "^/tmp/.eglot/.*")

  :preface
  (defun java-eclipse-jdt-file-name-handler (operation &rest args)
    "Support Eclipse jdtls `jdt://' uri scheme."
    (let* ((uri (car args))
           (cache-dir "/tmp/.eglot")
           (source-file
            (expand-file-name
             (file-name-concat
              cache-dir
              (save-match-data
                (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                  (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
      (unless (file-readable-p source-file)
        (let ((content (jsonrpc-request (eglot-current-server) :java/classFileContents (list :uri uri)))
              (metadata-file (format "%s.%s.metadata"
                                     (file-name-directory source-file)
                                     (file-name-base source-file))))
          (unless (file-directory-p cache-dir) (make-directory cache-dir t))
          (with-temp-file source-file (insert content))
          (with-temp-file metadata-file (insert uri))))
      source-file))

  (defun java-eclipse-jdt-launcher (_arg)
    "Returns a command to start Eclipse JDT launcher script `jdtls'."
    (let ((launcher-script (expand-file-name "org.eclipse.jdt.ls.product/target/repository/bin/jdtls" "~/code/eclipse.jdt.ls/"))
          (root-directory (project-root (project-current))))
      (if (file-exists-p launcher-script)
          (list launcher-script
                "-data"
                (expand-file-name (format "%s-%s" (md5 root-directory)
                                          (file-name-base (directory-file-name root-directory )))
                                  "~/code/jdtls-workspace/")
                :initializationOptions '(:extendedClientCapabilities
                                         (:classFileContentsSupport t)))
        (message "Failed to find any JDT jar files.")
        nil))))

(use-package javadoc-lookup
  :ensure t
  :init
  (setq javadoc-lookup-completing-read-function completing-read-function)
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-local browse-url-browser-function
                          (if (featurep 'xwidget-internal)
                              #'xwidget-webkit-browse-url
                            #'eww-browse-url))
              (define-key java-mode-map (kbd "C-c ; d")  #'javadoc-lookup)
              (define-key java-mode-map (kbd "C-c ; i")  #'javadoc-add-import)
              (define-key java-mode-map (kbd "C-c ; s")  #'javadoc-sort-imports))))

(use-package gradle-mode
  :doc "Gradle integration in Emacs."
  :ensure t
  :hook ((java-mode . gradle-mode)
         (java-ts-mode . gradle-mode))
  :delight gradle-mode
  :config
  (setq gradle-use-gradlew t
        gradle-gradlew-executable "./gradlew"))

(use-package scala-mode
  :pin melpa
  :ensure t
  :defer t
  :mode  ("\\.sc\\'" . scala-mode)
  :hook (scala-mode . eglot-ensure)
  :config
  (use-package sbt-mode
    :ensure t
    :pin melpa))

(use-package scala-cli-repl
  :after scala-mode
  :vc ( :url "https://github.com/ag91/scala-cli-repl"
        :rev :neweset )
  :custom (scala-cli-load-repl-in-sbt-context t)
  :ensure t)

(use-package spark
  :load-path "etc/"
  :commands (spark-connect!))

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

(use-package dtrt-indent
  :doc "Automatically guess offset and tabs-indent for opened file."
  :ensure t
  :disabled t
  :delight dtrt-indent-mode
  :hook (java-mode . dtrt-indent-mode))

(use-package pos-tip
  :doc "Racer needs this package and for some reason, it is not
  installed."
  :ensure t)

(use-package rust-mode
  :defer t
  :ensure t
  :custom (rust-format-on-save nil)
  :bind ( :map rust-mode-map ("RET" . newline-and-indent) )
  :hook ((rust-mode . eldoc-mode)
         (rust-mode . cargo-minor-mode))

  :config
  (when (treesit-available-p)
    (install-tree-sitter-grammer-if-required 'rust)))

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup)
  :ensure t)

(use-package cargo
  :ensure t
  :after rust-mode
  :delight cargo-minor-mode
  :config
  (define-key cargo-mode-map (kbd "C-. C-k") #'cargo-process-check))

(use-package python-ts-mode
  :defer t
  :init
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil
        python-indent-offset 4)

  :config
  (ensure-windows-on-right "\\*Python\\*"))

(use-package flymake-ruff
  :ensure t
  :hook (python-ts-mode . flymake-ruff-load))

(use-package ruff-format
  :ensure t
  :hook (python-ts-mode . ruff-format-on-save-mode))

(use-package pyvenv
  :ensure t
  :demand t
  :init
  (when (file-exists-p "~/.venv")
    (pyvenv-activate (expand-file-name "~/.venv"))))

(use-package py-autopep8
  :ensure t
  :hook (python-ts-mode . py-autopep8-mode))

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
  :delight paredit-mode
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
  (unless (executable-find "clojure-lsp")
    (async-shell-command
     (concat "cd /tmp;"
             "curl -o install-clojure-lsp -sLO https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install;"
             "chmod +x install-clojure-lsp;"
             "./install-clojure-lsp --dir ~/.local/bin/;")))

  (setq clojure-indent-style :always-align
        clojure-align-forms-automatically nil)

  (add-hook 'project-find-functions #'project-find-clojure-root)

  (add-hook 'clojure-mode-hook
            (lambda ()
              ;; Default completion is completely broken because of
              ;; `lisp-indent-line'.
              (setq-local completion-at-point-functions
                          '(cider-complete-at-point eglot-completion-at-point t))))

  :preface
  (defun project-find-clojure-root (dir)
    (when-let* ((r (clojure-project-root-path (or dir default-directory))))
      (cons 'clojure-project r)))

  (cl-defmethod project-root ((project (head clojure-project)))
    (cdr project)))

(use-package clj-refactor
  :pin melpa
  :ensure t
  :disabled t
  :delight clj-refactor-mode
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :custom ((cljr-insert-newline-after-require . nil)
           (cljr-auto-clean-ns . nil))
  :bind (:map clj-refactor-map
              ("C-; c <SPC>" . clojure-top-level-spacing))
  :config
  (cljr-add-keybindings-with-prefix "C-;"))

(use-package cljstyle-format
  :disabled t
  :ensure t
  :hook (clojure-mode . cljstyle-format-on-save-mode)
  :delight cljstyle-format-on-save-mode)

(use-package cider
  :doc
  :ensure t
  :delight cider-mode
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

  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (define-key cider-repl-mode-map (kbd "M-s") nil)))

  :preface
  (defun cider-find-var-dwim ()
    (interactive)
    (if (cider-connected-p)
        (cider-find-var)
      (call-interactively #'xref-find-definitions)))
  (autoload 'cider--make-result-overlay "cider-overlays")

  (defun endless/eval-overlay (value point)
    (cider--make-result-overlay (format "%S" value)
                                :where point
                                :duration 'command)
    ;; Preserve the return value.
    value))


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
  :config
  (use-package geiser-racket :ensure t)
  (use-package geiser-mit    :ensure t)
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


(use-package julia-mode  :defer t :ensure t)
(use-package julia-snail :defer t :ensure t)

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
  :delight whitespace-mode
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
              (when (not (memq major-mode '(markdown-mode image-mode pdf-view-mode)))
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
(use-package typescript-mode :defer t :ensure t)
(use-package coffee-mode     :defer t :ensure t)
(use-package elm-mode        :defer t :ensure t)

(use-package apheleia
  :doc "Auto-format code after save."
  :ensure t
  :delight apheleia-mode
  :init
  (apheleia-global-mode +1))

;;; ERLANG AND ELIXIR
;; ──────────────────────────────────────────────────────────────────
(use-package erlang :ensure t :defer t)
(use-package elixir-ts-mode
  :ensure t
  :defer t
  :config
  (bind-keys :map elixir-ts-mode-map
             ("C-M-e" . elixir-end-of-defun)
             ("C-M-a" . elixir-beginning-of-defun))

  (add-hook 'elixir-ts-mode-hook
            (lambda ()
              (enable-auto-expand-snippets (list "do" "fn"))))

  :preface
  (defun elixir-lsp-launcher (_arg)
    "Returns a command to start Elixir LSP server."
    (let ((launcher-script (expand-file-name "~/code/elixir-ls/release/language_server.sh"))
          (root-directory (project-root (project-current))))
      (if (file-exists-p launcher-script)
          (list launcher-script)
        (message "Clone https://github.com/elixir-lsp/elixir-ls.git and place it under ~/code.")
        nil))))

(use-package mix :ensure t)

(use-package alchemist
  :ensure t
  :after elixir-ts-mode
  ;; :hook (elixir-ts-mode . alchemist-mode)
  :bind (:map elixir-ts-mode-map
              ("C-x C-e" . alchemist-eval-current-line))
  :init
  (setq alchemist-key-command-prefix (kbd "C-;")
        alchemist-goto-elixir-source-dir (expand-file-name "~/code/elixir/")
        alchemist-goto-erlang-source-dir (expand-file-name "~/code/otp/"))

  (ensure-windows-on-right "\\*alchemist-eval-mode\\*")

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
  :bind (:map ruby-mode-map ("C-c C-g" . search-ruby-gems))
  :config (eval-after-load 'ruby-mode
            '(progn
               (define-key ruby-mode-map (kbd "C-x t") nil)
               (define-key ruby-mode-map (kbd "C-x T") nil)))
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
                (message "Error: %s" error-thrown))))))


(use-package ruby-end
  :ensure t
  :delight ruby-end-mode
  :hook ((ruby-ts-mode . ruby-end-mode)
         (ruby-mode . ruby-end-mode)))

(use-package yari
  :doc "Suggested by https://prelude.emacsredux.com/en/stable/modules/ruby/"
  :ensure t
  :after ruby-mode
  :bind ( :map ruby-mode-map ("C-c y" . yari)
          :map ruby-ts-mode-map ("C-c y" . yari) )
  :config
  (ensure-windows-on-right "\\*yari "))

(use-package inf-ruby
  :after ruby-mode
  :bind ( :map inf-ruby-minor-mode-map
          ("C-c C-l" . ruby-load-current-file) ))

(use-package rinari
  :ensure t
  :after ruby-mode
  :bind ( :map rinari-minor-mode-map
          ("C-c =" . rinari-insert-erb-skeleton)
          ("C-c -" . rinari-insert-no-equals) )
  :config
  (global-rinari-mode)

  :preface
  (defun rinari-insert-no-equals ()
    (interactive)
    (insert "<%  %>")
    (backward-char 3)))

(use-package robe
  :ensure t
  :disabled t
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
  :defer t
  :ensure t
  :config
  (use-package go-guru :ensure t)
  (use-package go-dlv  :ensure t)

  (unless (getenv "GOPATH")
    (setenv "GOPATH" (shell-command-to-string "echo -n $GOPATH")))

  (require 'project)
  (defun project-find-go-module (dir)
    (when-let* ((root (locate-dominating-file dir "go.mod")))
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

;;; Infra
;;  ─────────────────────────────────────────────────────────────────

(use-package terraform-mode
  :ensure t
  :defer t
  :mode "\\.tf\\(vars\\)?\\'"
  :custom (terraform-format-on-save  t))

(use-package ansible :ensure t :defer t)

(use-package jsonnet-mode
  :ensure t
  :bind ( :map jsonnet-mode-map ("C-M-a" . backward-sentence) )
  :hook ( jsonnet-mode . --eglot-ensure ))


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

(use-package difftastic
  :ensure-system-package ((difft . "difftastic"))
  :ensure t
  :after magit
  :config
  (define-key magit-mode-map (kbd "C-c d") #'difftastic-magit-show))

(use-package autorevert
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode +1))

(use-package transient
  :disabled t
  :ensure t
  :defer 10
  :config
  (use-package transient-defs :load-path "etc/"))

(use-package git-link
  :ensure t
  :config
  (add-to-list 'git-link-remote-alist
               '("\\.gitlab.*\\.io" git-link-gitlab)))

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

          :map global-map
          ("C-c g" . magit-file-dispatch)

          :map ctl-period-map
          ("C-g" . magit-status)
          ("C-c" . magit-file-dispatch) )

  :init
  (setq magit-define-global-key-bindings nil)

  :config
  (magit-auto-revert-mode -1)

  (setq magit-completing-read-function completing-read-function
        ;; Showing diffs during commits is currently slow.
        magit-commit-show-diff nil
        ;; More granular diffs for all visible hunks
        magit-diff-refine-hunk 't)

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
  (setq tramp-default-method "ssh")

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
        (format "%s\|%s"
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
  :config
  (use-package messages-are-flowing
    :doc
    "Defines command
`messages-are-flowing-use-and-mark-hard-newlines' that can be
called for displaying soft and hard newlines in a message
buffer."
    :load-path "etc/")


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
  :hook ( (pdf-view-mode . pdf-tools-enable-minor-modes)
          ;; (pdf-view-mode . pdf-view-themed-minor-mode)
          )
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
  (add-hook 'pdf-view-mode-hook (lambda () (blink-cursor-mode -1)))
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
(use-package erc-config :commands erc-start! :load-path "etc/" :defer t)

;;; EMACS-SERVER
;;  ─────────────────────────────────────────────────────────────────
(use-package server
  :demand t
  :config
  (unless (server-running-p)
    (server-start))

  ;; Always change focus to a newly created frame.
  (add-hook 'server-switch-hook #'raise-frame)

  (pcase system-type
    (`windows-nt
     ;; Default directory on windows isn't ~/
     (setq default-directory (expand-file-name "~/"))
     (setq interprogram-paste-function 'x-selection-value)
     ;; for some reason, selection highlight isn't turned on by default
     (transient-mark-mode t))
    (`darwin
     (macos-fix-keyboard-modifiers))

    (`gnu/linux
     (midnight-delay-set 'midnight-delay -7200)
     (midnight-mode +1)
     ;; Start Redshift if it's not already running on the system.
     (unless (seq-find (lambda (pid)
                         (when-let* ((args (assoc-default 'args (process-attributes pid))))
                           (string-match "^redshift .*" args)))
                       (list-system-processes))
       ;; Redshift doesn't work on Wayland.
       ;; =================================
       ;; (async-shell-command (format "nohup redshift -l %s:%s > /tmp/redshift.log"
       ;;                              (number-to-string calendar-latitude)
       ;;                              (number-to-string calendar-longitude)))
       ))))

;;; ──────────────────────────────────────────────────────────────────

(use-package devdocs
  :ensure t
  :bind ( :map ctl-h-x-map ("d" . devdocs-lookup) )
  :config
  (ensure-windows-on-right "\\*devdocs\\*\\'"))

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

  :bind ( :map ctl-h-x-map ("t" . tldr) )

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
  :bind ("C-c M-o" . ace-link)
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

(use-package poke-mode
  :ensure t
  :defer t)

(use-package poke
  :ensure t
  :defer t
  :commands poke
  :config
  (poke-frame-layout-1)
  :custom ( poke-settings-pretty-print "yes"
            poke-setting-omode "tree" ))

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
  (setq network-security-level 'high
        nsm-save-host-names t))

;;; -----------------------------------------------------------------

(use-package ipcalc :ensure t :commands ipcalc)

(use-package memory-report
  :doc "Built-in package to print information about Emacs memory usage."
  :defines memory-report)

(use-package backlight
  :load-path "packages/lisp"
  :commands backlight)

(use-package proced
  :custom ( (proced-auto-update-flag t)
            (proced-filter 'emacs) )
  :bind ( :map ctl-quote-map
          ("s t" . proced)
          :map proced-mode-map
          ("w"   . delete-other-windows)
          ("q"   . kill-buffer-and-window) )

  :config
  (define-key proced-mode-map
              [remap proced-toggle-tree] #'proced-toggle-marks)
  (define-key proced-mode-map
              [remap proced-toggle-marks] #'proced-toggle-tree))

(use-package gif-screencast
  :defer t
  :doc "A better alternative to my `start-recording-window' command."
  :load-path "packages/lisp")

(use-package keycast :disabled t :ensure t :defer t)

(use-package sourcegraph
  :load-path "etc/"
  :bind-keymap ("<C-m> s" . sourcegraph-mode-map )
  :hook (sourcegraph-mode . goto-address-mode))

(use-package llms
  :load-path "etc/"
  :init
  ;; Set up autoloads to make sure `llms.el' is autoloaded after any of the the
  ;; following features are loaded.
  (dolist (feature (list 'copilot 'gptel 'esi-dictate))
    (eval-after-load feature
      '(progn
         (require 'llms)
         (require 'llms-writing)
         (require 'llms-completion)
         (require 'llms-images))))

  :bind ( :map ctl-m-map
          ("i RET" . gptel-send)
          ("i t"   . copilot-mode)
          ("i a"   . claude-code*)

          ("i b"   . llms-chat)
          ("i w"   . llms-writing-spin-up-companion)
          ("i W"   . llms-writing-shutdown)

          ("i d"   . esi-dictate-start)

          ("i q"   . gptel-quick)
          ("i c"   . gptel)
          ("i m"   . gptel-menu)
          ("i r"   . gptel-rewrite)
          ("i z"   . gptel-buffer-toggle*)
          ("C-z"   . gptel-buffer-toggle*)

          ("i I" . macher-implement)
          ("i R" . macher-revise)
          ("i A" . macher-abort) )

  :config
  (ensure-windows-on-right "\\*chatgpt .*\\*" "\\*Anthropic\\*")

  ;; --- macOS issues:
  (when (eq system-type 'darwin)
    (setq llms-chat-show-in-progress-indicator nil)))


(provide 'init)
;;; init.el ends here
