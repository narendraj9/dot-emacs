;;; org-config.el --- Org mode and related things    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: data, convenience, tools

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

;; This file is going to contain everything related to org-mode and related
;; minor modes.

;;; Code:

(require 'dash)
(require 's)
(require 'ag)
(require 'alert)
(require 'defs)
(require 'f)
(require 'org-macs)

(defun org-set-created-property ()
  (interactive)
  (org-set-property "CREATED"
                    (format-time-string (org-time-stamp-format t t)
                                        (current-time))))

(defun import-icalendar-urls (ical-urls)
  "Fetch each input url in ICAL-URLS with `url-copy-file' and place
into org files using `ical2org.awk' script.

This function is useful to fetch entries from Google calendar
into `org-agenda'.

To make these entries visible in `org-agenda', set
`org-agenda-include-diary' to `t'."
  (let ((ics-directory (expand-file-name "icalendar" org-directory))
        (ical2org-script (expand-file-name "bin/ical2org.awk" user-emacs-directory)))
    (unless (file-exists-p ics-directory)
      (make-directory ics-directory))
    (dolist (ical-url ical-urls)
      (let* ((url (url-generic-parse-url ical-url))
             (target-org-file (format "ical-%s-%s.org" (url-host url) (url-file-nondirectory ical-url)))
             (target-ical-filepath (make-temp-file "ical-import-"))
             (target-org-filepath (expand-file-name target-org-file ics-directory)))
        (unwind-protect
            (progn
              (url-copy-file ical-url target-ical-filepath t)
              (call-process-shell-command (format "%s %s > %s" (shell-quote-argument ical2org-script)
                                                  (shell-quote-argument target-ical-filepath)
                                                  (shell-quote-argument target-org-filepath))))
          (delete-file target-ical-filepath))))))

(defun google-calendar-import-to-org ()
  (interactive)
  (when (boundp 'personal-google-calendar-url)
    (import-icalendar-urls (list personal-google-calendar-url))))

(use-package org-web-tools :ensure t)

(defun org-agenda-redo-with-days-to-deadline ()
  "Change `org-agenda' buffer and display days to deadline for all tasks."
  (interactive)
  (let ((org-agenda-compact-blocks nil)
        (org-deadline-warning-days most-positive-fixnum))
    (org-agenda-redo-all)))


(defun after-org-agenda-selection-move-to-heading ()
  (org-id-goto (org-id-get-create))
  (org-narrow-to-subtree))


(defun open-org-file (arg)
  "Quick open a notes org file.
If a prefix ARG is provided, then run a `project-find-file'.
Otherwise, limit to only `org-mode' files."
  (interactive "P")
  (let ((default-directory (expand-file-name org-directory)))
    (->> (f-entries default-directory
                    (if arg (lambda (f) (f-ext-p f "org")) #'f-file?)
                    t)
         (mapcar (lambda (f) (propertize f 'display (f-relative f org-directory))))
         (completing-read "Select org file: ")
         (find-file))))


(defun search-notes-files ()
  "Search org files using `consult-grep'."
  (interactive)
  (require 'consult)
  (let ((consult-grep-args (append consult-grep-args
                                   (list " --exclude=*.htm --exclude=*.html"))))
    (consult-grep org-directory)))


(defun org-agenda-toggle-toggle-tags-column ()
  "Toggle the display of tags column in `org-agenda' view."
  (interactive)
  (setq org-agenda-remove-tags (not org-agenda-remove-tags))
  (org-agenda-redo))

(defun org-goto-heading-from-attach-dir ()
  (interactive)
  (let* ((parts (seq-filter (lambda (part)
                              (not (string-equal part "")))
                            (file-name-split default-directory)))
         (id-parts (drop (- (length parts) 2) parts)))
    (org-id-goto (format "%s%s"
                         (car id-parts)
                         (cadr id-parts)))))


(use-package org
  :demand t
  :bind ( :map org-mode-map
          ("M-q" . org-fill-paragraph)
          ("C-c t" . org-set-created-property) )
  :init
  (setq org-directory (or (getenv "ORG_DIRECTORY")
                          "~/miscellany/personal/org/")
        org-archive-location (format "%s::datetree/* Archived Tasks"
                                     (expand-file-name "_archives/archive.org"
                                                       org-directory))
        org-default-notes-file (expand-file-name "notes.org" org-directory))

  (set-register ?j (cons 'file org-default-notes-file))

  :config
  (setq org-cycle-separator-lines 0
        org-cycle-include-plain-lists 'integrate
        org-catch-invisible-edits 'error
        org-blank-before-new-entry
        '((heading . nil) (plain-list-item . nil))
        org-image-actual-width nil

        ;; Blocks and Drawers
        org-hide-block-startup nil
        org-cycle-hide-block-startup nil
        org-cycle-hide-drawer-startup t
        org-hide-drawer-startup t

        org-startup-with-latex-preview nil
        org-preview-latex-image-directory
        (expand-file-name "tmp/ltximg/" user-emacs-directory)

        ;; Speed commands let you use single keys once you are on the
        ;; first of a heading. n/p to go back and forth in the list of
        ;; headlines. For adding new commands see
        ;; `org-speed-commands-user'.
        org-use-speed-commands t
        org-use-fast-todo-selection 'expert

        ;; Keep text indented but always show leading stars
        org-startup-indented t
        org-indent-mode-turns-on-hiding-stars nil

        ;; Dependencies and sub-tasks
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t

        ;; Keep state change notes into LOGBOOK drawer
        org-log-into-drawer t
        org-log-reschedule 'time
        org-log-redeadline 'time
        org-log-done 'time)

  (push '(redeadline .  "New deadline from %S to %s on %t") org-log-note-headings)
  (push '(reschedule .  "Rescheduled from %S to %s on %t") org-log-note-headings)

  (add-hook 'org-follow-link-hook
            (lambda (&rest args) (message "Opening link..." args)))

  ;; Do not tuck notes into LOGBOOK drawer.
  (advice-add 'org-store-log-note :around #'with-no-drawer)
  (add-to-list 'org-log-note-headings (cons 'note "%t"))

  (setq org-todo-keywords
        ;; state logging for org-habit (! => with timestamp) (@ => timestamp
        ;; + note)
        '((sequence "TODO" "IN PROGRESS(i)" "NEXT" "PARKED(p!)" "|" "DONE(d!)" "NOT_DONE(x@)")
          (sequence "BLOCKED(b@)" "|" "CANCELLED(c@)")
          (sequence "TRACKING(r)" "TRACKED(g@)"))
        org-todo-keyword-faces
        '(("TODO"        (:foreground "red" :weight bold))
          ("IN PROGRESS" (:foreground "blue" :weight bold))
          ("PARKED"      (:foreground "IndianRed" :weight bold))
          ("NEXT"        (:foreground "OrangeRed" :weight bold))
          ("BLOCKED"     (:foreground "orange" :weight bold))
          ("DONE"        (:foreground "forest green" :weight bold))
          ("NOT_DONE"    (:foreground "indian red" :weight bold))
          ("CANCELLED"   (:foreground "forest green" :weight bold))
          ("TRACKING"    (:foreground "light green" :weight bold))
          ("TRACKED"     (:foreground "forest green" :weight bold))))

  (add-hook 'org-after-todo-state-change-hook
            ;; Remove scheduled date and deadline if new state is "NEXT"
            (lambda ()
              (when (equal "NEXT" org-state)
                (org-schedule '(4))
                (org-deadline '(4)))))

  (setq org-todo-state-tags-triggers
        ;; Remove :someday: tag when tasks are done.
        '((done ("someday"))))

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        ;; refiling across multiple levels
        org-refile-targets `((nil           . (:maxlevel . 2))
                             ("main.org"    . (:level . 1))
                             ("capture.org" . (:level . 1))
                             ("someday.org" . (:maxlevel . 2)))
        org-refile-allow-creating-parent-nodes t)

  (add-to-list 'org-modules 'org-habit)
  (add-hook 'org-mode-hook (lambda ()
                             (goto-address-mode +1)
                             (unbind-key "S-<return>" org-mode-map)
                             (unbind-key "C-<return>" org-mode-map)
                             (setq mode-name "Org")
                             ;; Turn on `variable-pitch-mode' if using a light
                             ;; theme
                             ;; (when (eq (frame-parameter nil 'background-mode)
                             ;;           'light)
                             ;;   (variable-pitch-mode +1))
                             (local-set-key [remap goto-address-at-point]
                                            #'org-open-at-point)))
  (plist-put org-format-latex-options
             :foreground "Black")
  (plist-put org-format-latex-options
             :background "White")
  (plist-put org-format-latex-options
             :scale 1.3)

  ;; Use UUIDs for linking headings internally.
  (setq org-id-link-to-org-use-id t)

  ;; Add a new structure template
  (add-to-list 'org-structure-template-alist '("t" . "#+TITLE: ?"))

  ;; Opening PDF files inside Emacs by default
  (setq org-file-apps
        (assoc-delete-all "\\.pdf\\'" org-file-apps)))


(use-package org-agenda
  :after org
  :init
  (setq org-agenda-cmp-user-defined #'org-backlog-compare-fn
        org-complete-tags-always-offer-all-agenda-tags t
        org-extend-today-until 1)

  (defvar org-agenda-known-files
    (list "journal.org" "main.org" "remember.org" "habits.org"
          "work.org" "calendar.org" "appointments.org")
    "Files recognized as agenda files.")

  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-files
        (seq-concatenate 'list
                         (mapcar (lambda (f) (expand-file-name f org-directory))
                                 (seq-filter (lambda (f)
                                               (member f org-agenda-known-files))
                                             (let ((default-directory org-directory))
                                               (file-expand-wildcards "*.org"))))
                         ;; Allow TODO items to be created while writing notes
                         ;; on a subject.
                         (list org-default-notes-file)
                         (directory-files-recursively (expand-file-name "notes/" org-directory)
                                                      ".*\\.org$")

                         ;; Org files imported from iCalendar using the
                         ;; `ical2org.awk' script.
                         (directory-files (expand-file-name "icalendar" org-directory)
                                          t
                                          ".*\\.org$"))

        ;; Text search all org files under `org-directory' recursively.
        org-agenda-text-search-extra-files
        (directory-files-recursively org-directory ".*\\.org$"))

  (setq org-agenda-restore-windows-after-quit t

        org-agenda-include-diary nil
        org-agenda-span 7
        org-agenda-start-on-weekday nil
        org-deadline-warning-days 14
        org-agenda-show-all-dates t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        org-agenda-skip-deadline-if-done t

        org-agenda-window-setup 'only-window

        org-agenda-tags-column 120
        org-agenda-hide-tags-regexp "\\(ATTACH\\)\\|\\(new_tags_go_here_like_this\\)"
        org-agenda-remove-tags t
        org-agenda-show-inherited-tags nil

        ;; Dependencies and blocked TODOs
        org-agenda-dim-blocked-tasks 'invisible

        org-agenda-compact-blocks nil
        org-agenda-block-separator
        (propertize (make-string 80 ?═)
                    'face '(:foreground "grey19" :height 1.2))

        org-agenda-clockreport-parameter-plist
        '(:link t :max-level 4 :fileskip0 t :compact t :narrow 80)

        ;; Keep all tasks in same state grouped together and then sort
        ;; among them according to priority.  The order of tasks in
        ;; agenda represents my ability to act on them.
        org-agenda-sorting-strategy
        '((agenda habit-down deadline-up time-up todo-state-down priority-down category-keep)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep))

        ;; org-agenda-scheduled-leaders '("Scheduled: " "Sched.%2dx: ")
        ;; org-agenda-deadline-leaders  '("Deadline:  " "In %3d d.: " "%2d d. ago: ")
        org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c"))

        org-priority-highest ?A
        org-priority-lowest ?Z
        org-priority-default ?D
        org-priority-start-cycle-with-default t

        ;; Custom agenda vews
        org-agenda-custom-commands
        '(("a" "My Agenda"
           ((agenda "TODO"
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'todo
                                                 '("ONGOING" "NEXT" "PARKED" "BLOCKED")))))
            (todo "IN PROGRESS"
                  ((org-agenda-overriding-header "  IN PROGRESS:\n  ═══════════")))
            (todo "PARKED"
                  ((org-agenda-overriding-header "  Parked Tasks:\n  ════════════")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "  Next Tasks:\n  ══════════")))))
          ("o" "All Tasks"
           ((agenda "TODO"
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'todo
                                                 '("ONGOING"
                                                   "NEXT"
                                                   "BLOCKED")))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "  Next Tasks:\n  ══════════")))
            ;; (todo "BLOCKED"
            ;;       ((org-agenda-overriding-header "  Blocked Tasks:")))
            ;; (tags "+project+LEVEL=2|+project+LEVEL=1"
            ;;       ((org-agenda-overriding-header " Themes:")))
            (todo "TODO"
                  ;; Tasks for which I haven't thought about at all.q
                  ((org-agenda-overriding-header "  Backlog:\n  ═══════")
                   (org-agenda-todo-ignore-scheduled 'all)
                   (org-agenda-todo-ignore-deadlines 'all)
                   (org-agenda-todo-ignore-timestamp 'all)
                   (org-agenda-sorting-strategy '(user-defined-down
                                                  priority-down
                                                  category-keep))))))
          ("r" "Review"
           agenda "Tasks completed/cancelled"
           ((org-agenda-start-with-log-mode t)
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if 'nottodo 'done))))))

  ;; Split org-agenda vertically | @TODO: Find a better way.
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              ;; Setting `org-agenda-window-setup' to `only-window' is not
              ;; enough.
              (delete-other-windows)
              ;; Also add appointments from org-agenda entries.
              (quietly (org-agenda-to-appt t))
              (setq mode-name "Org Agenda")
              (local-set-key [remap goto-address-at-point] #'org-agenda-open-link)

              ;; Alternative to a constantly highlighted line
              (dolist (org-move-fn '(org-agenda-next-line org-agenda-previous-line))
                (advice-add org-move-fn
                            :after
                            (lambda ()
                              (pulse-momentary-highlight-one-line (point-at-bol)))))))

  (add-hook 'org-agenda-finalize-hook #'org-agenda-delete-empty-blocks)
  (add-hook 'org-agenda-after-show-hook #'after-org-agenda-selection-move-to-heading)

  :preface
  (defun with-no-drawer (func &rest args)
    (interactive "P")
    (if (eq org-log-note-purpose 'note)
        (let ((org-log-state-notes-insert-after-drawers t)
              (org-log-into-drawer nil))
          (apply func args))
      (apply func args)))

  (defun org-entry-age ()
    "Return age of the current entry."
    (if-let ((created-at (cdr (assoc "CREATED" (org-entry-properties)))))
        (format " <%d> " (- 0 (org-time-stamp-to-now created-at)))
      ""))

  (defun org-agenda-goto-today* ()
    "Narrow down to today in `org-agenda'."
    (interactive)
    (org-agenda-goto-today)
    (org-agenda-day-view))

  (defun org-agenda-find-file ()
    "Open one of files in `org-agenda-files'."
    (interactive)
    (let ((f (org-completing-read "File: " org-agenda-files)))
      (find-file f)))

  (defun org-deadline-and-todo ()
    (interactive)
    (org-agenda-todo "TODO")
    (call-interactively #'org-agenda-deadline))

  (defun org-schedule-and-todo ()
    (interactive)
    (org-agenda-todo "TODO")
    (call-interactively #'org-agenda-schedule))

  (defun jump-to-org-agenda ()
    "Jump to the agenda buffer.
     Credits: John Wigley."
    (interactive)
    (when (not (minibufferp))
      (let ((gc-cons-threshold most-positive-fixnum)
            (buf (get-buffer "*Org Agenda*"))
            wind)
        (if buf
            (if (setq wind (get-buffer-window buf))
                (select-window wind)
              (if (called-interactively-p 'interactive)
                  (progn
                    (select-window (display-buffer buf t t))
                    (org-fit-window-to-buffer)
                    (org-agenda-redo))
                (with-selected-window (display-buffer buf)
                  (org-fit-window-to-buffer)
                  (org-agenda-redo))))
          (custom-agenda-view)))))

  (defun org-review-captures ()
    "Review all captured tasks."
    (interactive)
    (find-file (expand-file-name "capture.org"
                                 org-directory)))

  (defun custom-agenda-view ()
    "Show my custom agenda view."
    (interactive)
    (org-agenda nil "i")
    (org-fit-window-to-buffer)
    (org-agenda-redo))

  (defun org-backlog-compare-fn (a b &optional some-tag)
    "Compare to entries in Backlog initially.
     Tasks with tag :someday are low priority."
    (let* ((tag (or some-tag "someday"))
           (a-pos (get-text-property 0 'org-marker a))
           (b-pos (get-text-property 0 'org-marker b))
           (a-has-tag (-when-let ((a-tags-string (org-entry-get a-pos "TAGS")))
                        (string-match-p (regexp-quote tag) a-tags-string)))
           (b-has-tag (-when-let ((b-tags-string (org-entry-get b-pos "TAGS")))
                        (string-match-p (regexp-quote tag)
                                        (org-entry-get b-pos "TAGS")))))
      (cond
       ((and a-has-tag (not b-has-tag)) -1)
       ((and b-has-tag (not a-has-tag)) +1))))

  (defun org-agenda-delete-empty-blocks ()
    "Remove empty agenda blocks.

     A block is identified as empty if there are fewer than 3 non-empty
     lines in the block (excluding the line with
     `org-agenda-block-separator' characters).  Thanks to
     https://lists.gnu.org/archive/html/emacs-orgmode/2015-06/msg00266.html"
    (when org-agenda-compact-blocks
      (user-error "Cannot delete empty compact blocks"))
    (let ((buffer-read-only t)
          (minimum-content-line-count 3))
      (save-excursion
        (goto-char (point-min))
        (let* ((minimum-section-line-count 5)
               (block-re (format "%s" org-agenda-block-separator))
               (separator-positions (list)))
          (while (and (not (eobp)) (forward-line))
            (when (looking-at-p block-re)
              (push (line-beginning-position) separator-positions)))
          (dolist (section (-zip separator-positions (cdr separator-positions)))
            (when (< (count-lines (car section) (cdr section))
                     minimum-section-line-count)
              (put-text-property (car section) (cdr section) 'invisible t))))))))


(use-package org-cliplink
  :ensure t
  :defer t
  :preface
  (defun org-cliplink-at-point ()
    "Change URL at point by querying its title."
    (interactive)
    (when-let ((bounds (bounds-of-thing-at-point 'url))
               (url (thing-at-point 'url)))
      (delete-region (car bounds) (cdr bounds))
      (insert (org-make-link-string url (org-cliplink-retrieve-title-synchronously url)))))

  (defun org-cliplink-dwim ()
    (interactive)
    (if (thing-at-point 'url)
        (org-cliplink-at-point)
      (org-cliplink)))

  :bind (:map org-mode-map
              ("C-c M-l" . org-cliplink-dwim)))


(defun read-happiness-rating ()
  "Inspired by https://www.trackinghappiness.com/about/method/."
  (completing-read "Happiness Rating: "
                   (map 'list #'number-to-string (number-sequence 1 10))
                   nil
                   t))


(use-package org-capture
  :defer t
  :preface
  (defun org-config-capture ()
    "Renamed org-capture to `autoload' org-config.el on use."
    (interactive)
    (org-capture))

  :config
  (defconst org-config--common-metadata
    (concat ":METADATA:\n"
            " CREATED: %U\n"
            "%(unless (s-blank? \"%:link\") (concat \" URL: \" \"%:link\" \"\n\"))"
            "%(unless (s-blank? \"%:description\") (concat \" DESCRIPTION: \" \"%:description\" \"\n\"))"
            "%(unless (s-blank? \"%:annotation\") (concat \" ANNOTATION: \" \"%:annotation\" \"\n\"))"
            ":END:\n"
            "%(unless (s-blank? \"%i\") (format \"#+BEGIN_QUOTE\n %s\n#+END_QUOTE\n\" \"%i\"))"))

  (setq org-datetree-add-timestamp nil)
  (setq org-capture-templates
        `(("i" "TODO" entry (file "capture.org")
           ,(concat
             "* TODO %? \n"
             org-config--common-metadata)
           :prepend t)
          ("n" "NOTE" entry (file+olp+datetree ,org-default-notes-file)
           ,(concat "* %? \n"
                    org-config--common-metadata
                    "\n\n%i")
           :tree-type week)
          ("e" "Event" entry (file "main.org")
           ,(concat "* %? \n"
                    org-config--common-metadata
                    "\n%^T"))
          ("t" "Appointment" entry (file "appointments.org")
           ,(concat "* %? \n"
                    org-config--common-metadata
                    "\n%^T"))
          ("l" "Article" entry (file "articles.org")
           "* %(let ((url (org-cliplink-capture)))
                  (run-with-timer 1 nil (lambda () (message \"%s\" url))) url) %?"
           :empty-lines 0
           :prepend t
           :immediate-finish t)
          ("j" "Journal" entry (file+olp+datetree "journal.org")
           ,(concat "* %? %^G           \n"
                    org-config--common-metadata))
          ("c" "Question" entry (file+headline "questions.org" "Questions")
           "* %?\n ")
          ("h" "Habit" entry (file+headline "habits.org"  "Habits")
           ,(concat "* TODO %?\n"
                    "  SCHEDULED: <%(read-date \"%Y-%m-%d\") "
                    ".+%^{Repeat every (add + for non-backlog tasks)|1d|1w|1m|}> \n"
                    "  :PROPERTIES:       \n"
                    "  :STYLE:    habit   \n"
                    "  :END:              \n"))
          ("b" "Birthday" plain (file+headline "remember.org" "Birthdays")
           "\%\\%(org-anniversary %(read-date)) %?")
          ("a" "Anniversary" plain (file+headline "remember.org" "Anniversaries")
           "\%\\%(org-anniversary %(read-date)) %?")

          ;; Tracking values
          ("r" "Track Happiness"
           table-line (file+olp "habits.org"
                                "Habits"
                                "Record Happiness Rating"
                                "Happiness")
           "| %U | %(read-happiness-rating) | %^{Positive} | %^{Negative}"
           :table-line-pos "II-1")))

  ;; (add-hook 'org-capture-before-finalize-hook
  ;;           #'org-set-created-property)
  )


(use-package org-colview
  :doc "Column view shows `org-mode' entries in a table."
  :defer t
  :init
  (setq org-columns-default-format
        "%TODO  %3PRIORITY %TAGS %60ITEM(Task) %10EFFORT(Estimated Effort) {:} %10CLOCKSUM(Time Spent)"))


(use-package ob-elixir :ensure t :defer t)
(use-package ob-go     :ensure t :defer t)

(use-package ob-ditaa
  :after ob
  :defer t
  :preface
  (defun ob-ditaa-insert-color ()
    "Insert color at point interactively."
    (interactive)
    (require 'color)
    ;; Make sure that we insert new text at (point)
    (picture-update-desired-column t)
    (mapc (lambda (c)
            (picture-insert c 1))
          (apply #'format
                 "c%X%X%X"
                 (mapcar (lambda (c) (floor (* c 15)))
                         (color-name-to-rgb (read-color))))))
  :init
  (require 'picture)

  :bind (:map picture-mode-map
              ("C-c C-i" . ob-ditaa-insert-color))
  :config
  (setd org-ditaa-eps-jar-path
        (expand-file-name "~/miscellany/assets/ditaa/DitaaEps.jar")

        org-ditaa-jar-path
        (expand-file-name "~/miscellany/assets/ditaa/ditaa.jar")))


(use-package ob
  :after org
  :defer t
  :init
  (setq org-babel-load-languages
        `((emacs-lisp . t)
          (sqlite     . t)
          (sql        . t)
          (lisp       . t)
          (latex      . t)
          (python     . t)
          (R          . t)
          (ruby       . t)
          (gnuplot    . t)
          (clojure    . t)
          (C          . t)
          (shell      . t)
          (awk        . t)
          (elixir     . t)
          (go         . t)
          (haskell    . t)
          (ditaa      . t)
          (verb       . t)
          (scala-cli  . t)
          (plantuml   . t)))

  (setq org-babel-python-command "python"
        org-confirm-babel-evaluate nil)

  ;; Make tab indent according to source blocks major mode
  ;; Disabled for now: Enabling it causes the blinker to disappear after
  ;; hitting [Tab] sometimes.
  (setq org-src-tab-acts-natively nil)

  ;; Fontify quote and verse blocks
  (setq org-fontify-quote-and-verse-blocks t)

  ;; I usually do not look at the org file while editing code. So, reorganizing
  ;; the windows in the current frame isn't useful for me.
  (setq org-src-window-setup 'current-window)

  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append)

  :config
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))


(use-package org-habit
  :after org-agenda
  :init
  (setq org-habit-graph-column 80
        ;; Use `customize-variable' depending on the screen size and
        ;; resolution.
        org-habit-preceding-days 21
        org-habit-following-days 14
        org-habit-show-done-always-green t))


(use-package org-clock
  :after org-agenda
  :init
  (setq org-clock-idle-time 5
        org-clock-report-include-clocking-task t)

  :config
  (setq org-agenda-clock-report-header "\nClock Report\n──────────── "
        org-show-notification-handler
        (lambda (msg)
          (alert-libnotify-notify (list :title "Org@Emacs"
                                        :message msg))))

  ;; Let's see when they remove `org-combine-plists' if ever.
  (setq org-clocktable-defaults (org-combine-plists org-clocktable-defaults
                                                    (list :stepskip0 t
                                                          :fileskip0 t))
        org-clock-heading-function
        (lambda ()
          (s-truncate 30 (nth 4 (org-heading-components))))))


(use-package org-attach
  :after org
  :preface
  (defun org-att-complete-link (&optional arg)
    "Completion dispatcher for att: links (rewritten from org-attach-open)"
    (let* ((attach-dir (expand-file-name org-attach-directory org-directory))
           (file-paths (directory-files-recursively attach-dir ".*"))
           (completions (mapcar (lambda (f)
                                  (cons (file-name-base f) f))
                                file-paths))
           (file-name (completing-read "att: " completions nil t))
           (path (assoc-default file-name completions)))
      ;; Use existing `file:' to have relative paths work!
      (concat "file:" (file-relative-name path
                                          (file-name-directory (buffer-file-name))))))
  :config
  (org-link-set-parameters "att" :complete 'org-att-complete-link)

  (setq org-attach-directory
        (expand-file-name "data/" org-directory)))


(use-package ox-reveal
  :ensure t
  :defer t
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
        org-reveal-slide-number nil
        org-reveal-title-slide nil))


(use-package ox-gfm
  :doc
  "Github-flovered markdown export backend for org-export."
  :after org
  :defer t
  :ensure t)


(use-package org-noter
  :doc "Planning to move to `org-noter' from `interleave'."
  :ensure t
  :commands org-noter
  :preface
  (defun org-noter-add-attachment-path (&rest _args)
    "Add path to an attachment as org-noter document property."
    (unless (org-entry-get nil org-noter-property-doc-file)
      (org-entry-put nil org-noter-property-doc-file
                     (substring (org-att-complete-link) 4))))

  :config
  (setq org-noter-always-create-frame nil)
  (advice-add 'org-noter :before #'org-noter-add-attachment-path))


(use-package org-blog
  :disabled t
  :after ox-publish
  :load-path "~/blog/src/"
  :preface
  (defun write-post ()
    "Write a new post or modify an existing draft."
    (interactive)
    (require 'f)
    (let* ((default-directory "~/blog/src/drafts/")
           (all-posts (f-files "~/blog/src/posts/"
                               (lambda (path)
                                 (s-ends-with-p ".org" path))
                               t))
           (file-path (completing-read "File: " all-posts)))
      ;; If `file-path' doesn't exist yet, this would create it in drafts/
      ;; directory
      (find-file file-path))))


(use-package org-gcal
  :ensure t
  :disabled t
  :bind ( :map org-agenda-mode-map ("C-c g" . org-gcal-fetch* ) )
  :when (and (boundp 'gcal-client-id)
             (boundp 'gcal-client-secret))
  :config
  (setq org-gcal-client-id gcal-client-id
        org-gcal-client-secret gcal-client-secret
        org-gcal-file-alist
        `((,user-mail-address . ,(expand-file-name "calendar.org"
                                                   org-directory))))
  (setq org-gcal-notify-p nil
        org-gcal-auto-archive t)

  :init
  ;; (add-hook 'org-agenda-finalize-hook #'org-gcal-fetch)
  ;; (add-hook 'org-capture-before-finalize-hook #'org-gcal-sync)
  :preface
  (defun org-gcal-fetch* ()
    (interactive)
    ;; Defines `org-generic-id-*' vars as dynamic so that the next let binding
    ;; doesn't make them lexically scoped in this function.
    (require 'org-generic-id)
    (let ((org-agenda-files (list (expand-file-name "calendar.org"
                                                    org-directory)))
          org-agenda-text-search-extra-files
          org-generic-id-files org-id-extra-files org-generic-id-search-archives)
      (org-gcal-sync t t)
      ;; Give the above function some time to execute asynchronously (using
      ;; `emacs-deferred').
      (sit-for 5))))

(use-package org-edna :ensure t)

(use-package org-transclusion :ensure t)
(use-package org-remark
  :ensure t
  :commands (org-remark-global-tracking-mode org-remark-mode)
  :custom (org-remark-notes-display-buffer-action
           `((display-buffer-at-bottom)))
  ;; (org-remark-global-tracking-mode +1)
  )


(use-package org-pomodoro
  :ensure t
  :custom
  (org-pomodoro-format "")
  (org-pomodoro-finished-sound (expand-file-name "audio/quite-impressed.wav"
                                                 emacs-assets-directory)))


(use-package org-protocol    :demand t)
(use-package org-habit-stats :ensure t)


(provide 'org-config)
;;; org-config.el ends here
