;;; defs.el --- Utility var/functions for my init.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience, local

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

;; This file would contain all the var/fucntion defintions that I
;; originally used to keep in my init.el

;;; Code:

(require 'repeat)
(require 'seq)
(require 's)
(require 'cl-lib)

(defvar emacs-assets-directory
  (expand-file-name "~/miscellany/assets/")
  "Directory to keep assets that should not be shared.")

(defvar personal-dictionary-file
  (expand-file-name "~/miscellany/assets/personal-dict.en.pws")
  "File to keep words that I think should be a part of my dictionary.")

(defun seq-map-indexed (function sequence)
  "Return the result of applying FUNCTION to each element of SEQUENCE.
      Unlike `seq-map', FUNCTION takes two arguments: the element of
      the sequence, and its index within the sequence."
  (let ((index 0))
    (seq-map (lambda (elt)
               (prog1
                   (funcall function elt index)
                 (setq index (1+ index))))
             sequence)))

(defmacro setd (&rest var-dirs)
  "Set vars to dirs in VAR-DIRS relative to `user-emacs-directory'.

     The reason for writing this macro is to avoid calling
     `expand-file-name' all the time and to avoid missing calling the
     same.  This is important for keeping the directory paths portable
     as Windows and Linux have different path styles."
  `(let ((default-directory ,user-emacs-directory))
     ,(cons 'setq (seq-map-indexed (lambda (x sym-index)
                                     (if (= (mod sym-index 2) 0)
                                         x
                                       (list 'expand-file-name x)))
                                   var-dirs))))

(defsubst hook-into-modes (func &rest modes)
  "Add FUNC to mode-hooks for MODES.  Credits: John Whigley."
  (dolist (mode modes) (add-hook (intern (format "%s-hook" mode))
                                 func)))

(defsubst make-interactive (f &rest rest)
  "Define F to be an interactive function called with REST args."
  (lambda ()
    (interactive)
    (apply f rest)))

(defun other-window* (arg)
  "Act like `other-window' but change direction when ARG is given."
  (interactive "p")
  (if (= arg 4)
      (other-window -1)
    (other-window +1)))

(defun center-text-for-reading (&optional arg)
  "Setup margins for reading long texts.
If ARG is supplied, reset margins and fringes to zero."
  (interactive "P")
  ;; Set the margin width to zero first so that the whole window is
  ;; available for text area.
  (set-window-margins (selected-window) 0)
  (let* ((max-text-width (save-excursion
                           (let ((w 0))
                             (goto-char (point-min))
                             (while (not (eobp))
                               (end-of-line)
                               (setq w (max w (current-column)))
                               (forward-line))
                             w)))
         (margin-width (if arg
                           0
                         (/ (max (- (frame-width) max-text-width) 0) 2))))
    (setq left-margin-width margin-width)
    (setq right-margin-width margin-width)
    ;; `set-window-margings' does a similar thing but those changes do
    ;; not persist across buffer switches.
    (set-window-buffer nil (current-buffer))))

(defun shrink-current-window ()
  "Shrink current window by 5 lines/columns."
  (interactive
   (or (enlarge-window -5)
       (enlarge-window-horizontally -5))))

(defun fill-paragraph-and-move-forward ()
  "Combine `fill-paragraph'and `forward-paragraph'.
I tend to use then together always."
  (interactive)
  (fill-paragraph)
  (forward-paragraph))

(defun vicarie/eval-last-sexp-and-do (f)
  "Eval the last sexp and call F on its value."
  (let ((standard-output (current-buffer))
        (value (eval-last-sexp nil)))
    (funcall f value)))


(defun vicarie/eval-print-last-sexp ()
  "Evaluate and print the last sexp on the same line."
  (interactive)
  (vicarie/eval-last-sexp-and-do (lambda (value)
                                   (insert (format " (= %s ) " value)))))


(defun vicarie/eval-replace-last-sexp ()
  "Evaluate and replace last sexp with its value."
  (interactive)
  (vicarie/eval-last-sexp-and-do (lambda (value)
                                   (backward-kill-sexp)
                                   (insert (format "%s" value)))))


(defun org-late-todo (n)
  "Switch todo assuming an old date [N days ago]."
  (interactive "nDays: ")
  (require 'org-agenda)
  (let* ((delta-time (days-to-time n))
         (now (time-subtract (current-time)
                             delta-time)))
    (cl-letf (((symbol-function 'current-time) (lambda () now)))
      (org-agenda-todo))))


(defun switch-to-buffer-with-mode (arg)
  "Switch to buffer with `completion' candidates limited to just
one `major-mode'.

This command waits for the user to type just enough characters to
uniquely identify the target `major-mode'.

With a prefix ARG, the mode defaults to the `major-mode' of the
current buffer."
  (interactive "P")
  (let* ((mode-options (seq-uniq (mapcar (lambda (b)
                                           (buffer-local-value 'major-mode b))
                                         (buffer-list))))
         (mode-matches (lambda (read-chars)
                         (seq-filter (lambda (m) (string-prefix-p read-chars (symbol-name m)))
                                     mode-options)))
         selected-mode read-chars)
    (if arg
        (setq selected-mode major-mode)
      (while (not selected-mode)
        (setq read-chars
              (concat read-chars
                      (single-key-description
                       (read-char
                        (when read-chars
                          (format "%s: "
                                  (mapconcat (lambda (m)
                                               (concat (propertize read-chars
                                                                   'face 'minibuffer-prompt)
                                                       (string-remove-prefix read-chars (symbol-name m))))
                                             (funcall mode-matches read-chars)
                                             ", ")))))))
        (let ((matches (funcall mode-matches read-chars)))
          (cond
           ((zerop (length matches))
            (user-error "No matching modes found."))

           ((= 1 (length matches))
            (setq selected-mode (car matches)))))))
    (switch-to-buffer (read-buffer (format "%s: " selected-mode)
                                   nil
                                   t
                                   (lambda (b)
                                     (with-current-buffer (car b)
                                       (eq selected-mode major-mode)))))))

(defun switch-to-minibuffer ()
  "Switch to minibuffer."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))


(defun read-date (&optional format)
  "Get date from the user and return it in the format FORMAT.
If format isn't specified it defaults to `%Y %m %d`"
  (format-time-string (if format format "%Y %m %d")
                      (org-time-string-to-time (org-read-date))))


(defun make-old-content-read-only ()
  "Only allow for appending new content in the buffer."
  (interactive)
  (save-excursion
    (let ((begin (point-min))
          (end (progn
                 (goto-char (point-max))
                 (backward-word)  ; Upto the line containing a word
                 (end-of-line)
                 (point))))
      (add-text-properties begin end
                           '(read-only t rear-nonsticky t front-sticky t)))))


(defun kill-buffer-delete-window ()
  "Kill current buffer and delete its window."
  (interactive)
  (kill-buffer (current-buffer))
  (when (< 1 (count-windows))
    (delete-window)))


(defun inhibit-read-only ()
  "Avoid read-only mode.
Because eshell is silly and into read-only mode on typing over prompt."
  (interactive)
  (setq inhibit-read-only t))


(defun kill-with-linenum (beg end)
  "Kill region (BEG, END) with line numbers."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (skip-chars-backward "\n \t")
    (setq end (point))
    (let* ((chunk (buffer-substring beg end))
           (chunk (concat
                   (format "╭──────── #%-d ─ %s ──\n│ "
                           (line-number-at-pos beg)
                           (or (buffer-file-name) (buffer-name))
                           )
                   (replace-regexp-in-string "\n" "\n│ " chunk)
                   (format "\n╰──────── #%-d ─"
                           (line-number-at-pos end)))))
      (kill-new chunk)))
  (deactivate-mark))


(defun he-toggle ()
  "Toggle highlight expression inside selected parens.
Useful when showing code."
  (interactive)
  (if (equal show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-paren-style 'parenthesis)))


(defun kill-other-buffers ()
  "Kill all buffers except the current one and the erc buffers."
  (interactive)
  (let ((ignore-buffers (cons (current-buffer)
                              (if (fboundp 'erc-buffer-list)
                                  (erc-buffer-list)
                                nil))))
    (mapc (lambda (buffer)
            (when (not (memq buffer ignore-buffers))
              (kill-buffer buffer)))
          (buffer-list))))


(defun yank-to-x-clipboard (&optional region-beg region-end)
  "Yank selected text in (REGION-BEG, REGION-END) to X clipboard.
Use when on console."
  (interactive "r")
  (shell-command-on-region region-beg region-end "xclip -i -selec clip"))


(defun insert-date-time-at-point ()
  "Insert ISO 8601 timestamp at point using `org-time-stamp'."
  (interactive)
  (org-insert-time-stamp (current-time) t 'inactive))


(defun notify (msg &optional font-size duration)
  "Notify me with a MSG of size FONT-SIZE for DURATION seconds.
Requires that dzen is installed."
  (start-process-shell-command "dzen" nil
                               (format "echo %s | dzen2 -l 200 -fn 'Comic Sans MS:size=%s' -p %s"
                                       (shell-quote-argument msg)
                                       (or font-size 25)
                                       (or duration 5))))


;; Setup an emacs window into 70-30% horizontally.
(fset 'split-thirty-seventy
      "\C-x2\C-u4\C-x^\C-u4\C-x^\C-u3\C-x^")

(defun eshell-pop ()
  "Split the buffer vertically and shart shell in one of the windows."
  (interactive)
  (require 'eshell)
  (cond
   ((equal (buffer-name) eshell-buffer-name)
    (if (= (count-windows) 1)
        (bury-buffer)
      (delete-window)))

   ((get-buffer-window eshell-buffer-name)
    (select-window (get-buffer-window eshell-buffer-name)))

   (t
    (execute-kbd-macro (symbol-function 'split-thirty-seventy))
    (other-window 1)
    (eshell))))

(defun snap-it-to-file ()
  "Take a screenshot of Emacs and return the file path."
  (make-directory "/tmp/screenshots/" t)
  (let ((default-directory "/tmp/screenshots/"))
    (shell-command-to-string
     "scrot -u -e 'echo -n /tmp/screenshots/$f'")))


(defun snap-it ()
  "Take a screenshot and upload it to transfer.sh."
  (interactive)
  (upload-file (snap-it-to-file)))


(defun post-to-slack (webhook-url message)
  "Post to the slack WEBHOOK-URL contents of MESSAGE."
  (let ((url-request-method "POST")
        (url-request-data (json-encode `(:text ,message)))
        (url-callback  (lambda (_unused)
                         (search-forward "\n\n")
                         (let ((info-text (buffer-substring (point)
                                                            (point-max))))
                           (message (format "slack: %s"
                                            (s-trim info-text)))
                           (kill-buffer (current-buffer))))))
    (url-retrieve webhook-url url-callback)))


(defun screenshot-frame (window-id)
  "Take a screenshot of 400x200 pixels window with WINDOW-ID.
Taken from Chris Done's config"
  (shell-command-to-string
   (concat "import -window "
           (shell-quote-argument window-id)
           " +repage /tmp/frames/`date +%s`.png"))
  (pulse-momentary-highlight-region (window-start)
                                    (window-end)))


(defun start-recording-window ()
  "Record screenshots of the window and prepare a gif."
  (interactive)
  (message "Click on the window you want to record")
  (if (file-directory-p "/tmp/frames/")
      (delete-directory "/tmp/frames/" t))
  (make-directory "/tmp/frames/" t)
  (blink-cursor-mode -1)
  (let* ((window-id  (s-trim (shell-command-to-string
                              "xwininfo | grep 'Window id' | cut -d ' ' -f 4")))
         ;; Take a screenshot if I am idle for 1 second
         (timer (run-with-idle-timer 0.5
                                     t
                                     `(lambda ()
                                        (screenshot-frame ,window-id)))))
    (message "Started recording... [C-c x : Stop recording]")
    (global-set-key
     (kbd "C-c x")
     `(lambda (delay-between-frames)
        (interactive "nDelay between frames (e.g. 70): ")
        (cancel-timer ,timer)
        (message "Stopped recording!")
        (blink-cursor-mode 1)
        (global-unset-key (kbd "C-c x"))
        (message (shell-command-to-string
                  (format "convert -delay %s /tmp/frames/*.png /tmp/frames/out.gif && %s"
                          delay-between-frames
                          "echo Ouput saved to /tmp/frames/out.gif &")))))))


(defun fetch-parse-and-do (url parser action)
  "Helper function for all network related things.
Fetches the URL.  Changes the current buffer to the response
buffer.  Calls the PARSER on the buffer starting at the beginning
of the HTTP response in the response buffer.  Calls ACTION on the
value returned by the parser function."
  (let ((url-request-method "GET")
        (url-callback `(lambda (status)
                         (if status
                             (message "%s" status)
                           (search-forward "\n\n")
                           (,action (,parser))
                           (kill-buffer (current-buffer))))))
    (url-retrieve url url-callback)))


(defun get-location-coordinates (address)
  "Show and return the latitude and longitude of ADDRESS.
This uses the google maps api as described here:
https://developers.google.com/maps/documentation/geocoding/intro"
  (interactive "sAddress: ")
  (let ((api-url-endpoint
         (url-encode-url
          (format "https://maps.googleapis.com/maps/api/geocode/json?address=%s"
                  address)))
        (get-info-string-from-result
         (lambda (result)
           (let* ((fmt_address (assoc-default 'formatted_address result))
                  (geometry (assoc-default 'geometry result))
                  (location (assoc-default 'location geometry))
                  (latitude (assoc-default 'lat location))
                  (longitude (assoc-default 'lng location)))
             (format "Address: %s \nLatitude: %s\nLongitude: %s\n"
                     fmt_address
                     latitude
                     longitude)))))
    (fetch-parse-and-do api-url-endpoint
                        'json-read
                        (lambda (response)
                          (display-message-or-buffer
                           (mapconcat (lambda (result)
                                        (funcall get-info-string-from-result
                                                 result))
                                      (assoc-default 'results response)
                                      "\n"))))))


(defun toggle-window-split ()
  "Toggle between horizontal and vertical splits.
This has been taken from http://whattheemacsd.com/."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun swap--w1<->w2 (w1 w2)
  "Swap buffers and positions in W1 with those in W2.
Note w1 and w2 are references to window objects.  So, it is
important get the state of those buffers before we change any one
of them."
  (let ((b1 (window-buffer w1))
        (s1 (window-start w1))
        (b2 (window-buffer w2))
        (s2 (window-start w2)))
    (set-window-buffer w1 b2)
    (set-window-start w1 s2)
    (set-window-buffer w2 b1)
    (set-window-start w2 s1)))

(defun rotate-windows (&optional arg)
  "Rotate your windows.
This has been taken from http://whattheemacsd.com/.
If prefix ARG is given, the direction of rotation is reversed.
Default direction is anti-clockwise."
  (interactive "P")
  (let ((current-buffer (current-buffer)))
    (if (not (> (count-windows) 1))
        (message "You can't rotate a single window!")
      (let* ((window-refs (window-list)) ; list starts with current window
             (w1 (car window-refs))
             (w2 (if arg (car (last window-refs)) (cadr window-refs))))
        (swap--w1<->w2 w1 w2)))
    ;; Keep point in the same buffer
    (select-window (get-buffer-window current-buffer))))


(defun surround-symbol-with (c)
  "Surround the symbol at point with string C.
This works with any mode that supports thingatpt.el for a symbol."
  (interactive "cWrapper char: ")
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (s (make-string 1 c)))
    (when bounds
      ;; Insert the second delimiter first to avoid changing the
      ;; starting bound.
      (goto-char (cdr bounds))
      (insert s)
      ;; Now the first delimiter
      (goto-char (car bounds))
      (insert s))))


(defun out-or-onto-calc ()
  "Jump to the calc buffer or the editing buffer."
  (interactive)
  (require 'calc-misc)
  (if (eq major-mode 'calc-mode)
      (calc-other-window)
    (calc)))


(defun duplicate-current-line (prefix)
  "Duplicate current line.
Argument PREFIX decides whether we keep the point on current line
or the duplicated line."
  (interactive "P")
  (let* ((col (current-column))
         (beg (line-beginning-position))
         (end (line-end-position))
         (text (buffer-substring beg end)))
    (save-excursion
      (end-of-line)
      (insert (format "\n%s" text)))
    ;; When the prefix isn't supplied move the point to the next
    ;; line. It is more natural to make a copy of the first line and
    ;; edit the copy below that line.
    (when (not prefix)
      (forward-line)
      (move-to-column col))))


(defun open-woman-page (&optional cmd)
  "Open woman page for CMD  in a split right window."
  (interactive)
  (split-window-sensibly)
  (woman cmd))

(defun move-line-down ()
  "Move the current line down keeping the column."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  "Move the current line up keeping the column."
  (interactive)
  (let ((col (current-column)))
    (forward-line)
    (transpose-lines -1)
    (forward-line -1)
    (move-to-column col)))

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

(defun goto-char-in-line ()
  "Vim-like goto-char in Emacs.
Lands you before the character or after the last character.  Just
keep pressing the character until you get to the point.  I wrote
this and found out that there is `jump-char' which is better than
this."
  (interactive)
  (let* ((goto-char (if (eq last-command this-command)
                        (get this-command 'goto-char)
                      (put this-command 'goto-char (read-char "Goto: ")))))
    (and (looking-at (string goto-char)) (forward-char))
    (if (not (search-forward (string goto-char) (line-end-position) t))
        (message "No matches found.")
      (backward-char)
      (pulse-momentary-highlight-region (point)
                                        (1+ (point)))
      (set-transient-map (let ((map (make-sparse-keymap)))
                           (define-key map (vector goto-char)
                             #'goto-char-in-line)
                           map)))))

(defun underline-text (arg)
  "Insert a line under the current line based on prefix ARG.
It is filled with a default underline character `='.  If point had
been at the end of the line, moves point to the beginning of the
line directly following the underlining.  It does not underline
the line's leading whitespace, trailing whitespace, or comment
symbols.  With prefix `C-u' prompts user for a custom underline
character.  With prefix double prefix, do not underline whitespace
embedded in the line.
© Copyright 2015 Boruch Baum <boruch_baum@gmx.com>, GPL3+ license."
  (interactive "p")
  (let* ((original-point (point))
         (underline-char
          (replace-regexp-in-string "[[:cntrl:][:space:]]" "="
                                    (if (= arg 1)
                                        "="
                                      (char-to-string
                                       (read-char "What character to underline with?")))))
         (original-point-is-eol
          (when (looking-at "$") t))
         (original-point-is-eob
          (= original-point (point-max))))
    (beginning-of-line)
    (unless
        (when (looking-at "[[:space:]]*$")
          (beginning-of-line 0)
          (when (looking-at "[[:space:]]*$")
            (goto-char original-point)
            (message "nothing to do")))
      (insert
       (buffer-substring (line-beginning-position) (line-end-position))
       "\n")
      (save-restriction
        (narrow-to-region
         (progn
           (goto-char (1- (re-search-forward "[^[:space:]]" nil t)))
           (cond
            ((looking-at ";+")   (match-end 0))
            ((looking-at "#+")   (match-end 0))
            ((looking-at "//+")  (match-end 0))
            ((looking-at "/\\*+") (match-end 0))
            (t (point))))
         (1+ (progn
               (goto-char (line-end-position))
               (re-search-backward "[^[:space:]]" nil t))))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (if (= arg 16)
            (while (re-search-forward "[^[:space:]]" nil t)
              (replace-match underline-char nil))
          (re-search-forward "[^[:space:]]" nil t)
          (goto-char (1- (point)))
          (while (re-search-forward "." nil t)
            (replace-match underline-char nil)))
        (widen))
      (if original-point-is-eob
          (goto-char (point-max))
        (if original-point-is-eol
            (goto-char (re-search-forward "^"))
          (goto-char original-point))))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting.
Credits: Emacs Prelude"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun repeat-command (command &optional message-fn)
  "Make a repeatable command out of COMMAND.
Optional argument MESSAGE-FN (defaults to 'ignore) is used to
print information about what repeat is doing."
  (let ((repeat-message-function (or message-fn 'ignore)))
    (setq last-repeatable-command command)
    (repeat nil)))

(defmacro with-repeat-command (command &optional message-fn)
  (list 'defun (intern (format "with-repeat-command--%s" command)) ()
        ;; It's possible that `command' is not loaded yet as a function.
        (when-let ((fn (symbol-function command)))
          (documentation fn))
        (list 'interactive)
        (list 'repeat-command (list 'quote command) (list 'quote message-fn))))

(defvar quick-switch-themes (let ((themes-list (list 'jazz 'modus-operandi)))
                              (nconc themes-list themes-list))
  "A circular list of themes to keep switching between.
Make sure that the currently enabled theme is at the head of this
list always.

A nil value implies no custom theme should be enabled.")

(defun quick-switch-themes* ()
  "Switch between to commonly used faces in Emacs.
One for writing code and the other for reading articles."
  (interactive)
  (with-delayed-message (1 "Looks like the current theme is no in `quick-switch-themes'. C-g!")
    (while (not (memq (car quick-switch-themes) custom-enabled-themes ))
      (setq quick-switch-themes (cdr quick-switch-themes))))
  (if-let ((next-theme (cadr quick-switch-themes)))
      (progn (when-let ((current-theme (car quick-switch-themes)))
               (disable-theme (car quick-switch-themes)))
             (load-theme next-theme t)
             (message "Loaded theme: %s" next-theme))
    ;; Always have the dark mode-line theme
    (mapc #'disable-theme (delq 'smart-mode-line-dark custom-enabled-themes)))
  (setq quick-switch-themes (cdr quick-switch-themes)))

(defun quick-switch-themes ()
  "A repeatable version of `quick-switch-themes'.
The last key in the key binding can be used for repeating it."
  (interactive)
  (repeat-command 'quick-switch-themes*))

(defmacro def-lineup (command docstring &rest commands)
  "Make a COMMAND with DOCSTRING to run COMMANDS in order on repeated usage.
Currently works for commands that need no interactive input.  It
automatically makes the sequence a circular sequence
logically.  That's because `(car nil)' equals nil.

Argument DOCSTRING would serve as the docstring for COMMAND.

Example usage:
\(def-lineup fill-unfill
            \"Cycle between filling a line, filling a para and unfill.\"
            #'auto-fill-current-line #'fill-paragraph #'unfill-paragraph)

This would define a new command with the name fill-unfill that
would run commands in '(auto-fill-current-line fill-paragraph unfill-paragraph)
one after the other in a circular fashion, when called repeatedly."
  `(defun ,command () ,docstring
          (interactive)
          (let* ((command-sequence
                  (if-let* ((cseq (and (eq last-command this-command)
                                       (get this-command 'command-sequence))))
                      cseq
                    (list ,@commands)))
                 (next-command (car command-sequence)))
            (message (format "-> %s" next-command))
            (call-interactively next-command)
            (put this-command 'command-sequence (cdr command-sequence)))))

(defun enable-mode-for (mode secs)
  "Enable MODE for SECS seconds and disable it again.
Assume that (MODE +1) enables the mode and (MODE -1) disables
it."
  (funcall mode +1)
  (run-with-timer secs nil (lambda () (funcall mode -1))))

(defmacro gen-prefixed (command)
  "Create command with COMMAND that is called with a raw prefix arg.
Generated command is `command/prefixed'."
  `(defun ,(intern (concat (symbol-name command) "/prefixed")) ()
     (interactive)
     ,(concat "Calls `" (symbol-name command) "' with a prefix.")
     (funcall (function ,command) '(4))))

(defun split-money (amount)
  "Function to split expense of value AMOUNT  between two people.
Since I do not keep an account of change (< ₹ 10), I would add
enough expense for myself so as to make it a multiple of 10.

Return: (list expenses-noted-by-me
              expense-for-me
              expense-for-other-party)

Note: Don't waste your time trying to understanding all this."
  (let* ((change (mod amount 10))
         (expense-noted (+ amount (* (- 10 change) (cl-signum change))))
         (expense-for-other-party (/ amount 2.0)))
    (list expense-noted
          (- expense-noted expense-for-other-party)
          expense-for-other-party)))

(defmacro quietly (&rest forms)
  "Inhibit messages while evaluatings FORMS."
  `(let ((inhibit-message t))
     ,@forms))

(defmacro def-echoing (command)
  "Define `echoing-command' that prints returned value."
  `(defun ,(intern (format "echoing-%s" (symbol-name command))) ()
     (interactive)
     (message "%s" (call-interactively #',command))))

(defun utils-easy-move (map)
  "Set key-bindings (in MAP) for easier navigation."
  (interactive)
  (define-key map (kbd "h") 'backward-char)
  (define-key map (kbd "l") 'forward-char)
  (define-key map (kbd "j") 'next-line)
  (define-key map (kbd "k") 'previous-line)
  (define-key map (kbd "n") 'next-line)
  (define-key map (kbd "p") 'previous-line)
  map)

(defun utils-uneasy-move (map)
  "Reset key bindings (in MAP) set by easy-move."
  (interactive)
  (define-key map (kbd "h"))
  (define-key map (kbd "l"))
  (define-key map (kbd "j"))
  (define-key map (kbd "k"))
  (define-key map (kbd "n"))
  (define-key map (kbd "p"))
  map)

;;;###autoload
(define-minor-mode utils-easy-move-mode
  "A mode for bindings in a read only environment. Mimicks vim."
  :lighter "Easy"
  (utils-easy-move (make-sparse-keymap)))

(defun format-alist (x)
  "Format alist X for display."
  (let ((result ""))
    (dolist (kv x)
      (setq result (format "%s\n%s %s %s"
                           result
                           (car kv) "⟶" (cdr kv))))
    result))

(defun get-coin-price (coin-name)
  "Get rate for COIN-NAME.
Uses https://api.coinmarketcap.com/v1/ticker/$COIN-NAME/"
  (interactive "sCoin: ")
  (request (format "https://api.coinmarketcap.com/v1/ticker/%s/" coin-name)
           :parser 'json-read
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (let* ((result (aref data 0))
                              (coin-id (alist-get 'id result))
                              (usd-price (alist-get 'price_usd result)))
                         (display-message-or-buffer
                          (format "%s ($ %s)\n%s"
                                  coin-id
                                  usd-price
                                  (format-alist result))))))
           :error (cl-function
                   (lambda (&key error-thrown &allow-other-keys)
                     (message "Failed with: %s" error-thrown)))))

(defun do-when-idle (f g interval)
  "Call F when idle for INTERVAL seconds and then G when there is activity.
Thanks to Michael Heerdegen <michael_heerdegen@web.de>."
  (letrec ((run-timer-fun (lambda ()
                            (run-with-idle-timer interval
                                                 nil
                                                 (lambda ()
                                                   (funcall f)
                                                   (add-hook 'post-command-hook
                                                             activity-fun)))))
           (activity-fun (lambda ()
                           (remove-hook 'post-command-hook activity-fun)
                           (funcall g)
                           (funcall run-timer-fun))))
    (funcall run-timer-fun)))

(defvar limit-usage (make-hash-table :test 'equal))
(defun limit-usage (command max-minutes)
  "Limit usage of COMMAND to only once in MAX-MINUTES."
  (advice-add command
              :around
              `(lambda (orig-fn &rest args)
                 (let* ((hash-key (intern ,(symbol-name command)))
                        (last-ts (gethash hash-key limit-usage))
                        (gap-in-minutes (and last-ts
                                             (/ (time-to-seconds
                                                 (time-subtract (current-time)
                                                                last-ts))
                                                60))))
                   (if (and gap-in-minutes (< gap-in-minutes ,max-minutes))
                       (message "Last Accessed: %s Time to go: %.2f minutes"
                                (format-time-string "%FT%T%z" last-ts)
                                (- ,max-minutes gap-in-minutes))
                     (apply orig-fn args)
                     (puthash hash-key (current-time) limit-usage))))))

(defun overlays-to-text ()
  "Converts overlays to text.
From: https://www.emacswiki.org/emacs/OverlaysToText"
  (interactive)
  (let ((tb (get-buffer-create "*text*"))
        (s (point-min))
        (os (overlays-in (point-min) (point-max))))
    (with-current-buffer tb
      (erase-buffer))
    (setq os (sort os (lambda (o1 o2)
                        (< (overlay-start o1)
                           (overlay-start o2)))))
    (mapc (lambda (o)
            (let ((bt (buffer-substring-no-properties s (overlay-start o)))
                  (b (overlay-get o 'before-string))
                  (text (or (overlay-get o 'display)
                            (buffer-substring-no-properties (overlay-start o) (overlay-end o))))
                  (a (overlay-get o 'after-string))
                  (inv (overlay-get o 'invisible)))
              (with-current-buffer tb
                (insert bt)
                (unless inv
                  (when b (insert b))
                  (insert text)
                  (when a (insert a))))
              (setq s (overlay-end o))))
          os)
    (let ((x (buffer-substring-no-properties s (point-max))))
      (with-current-buffer tb
        (insert x)))
    (pop-to-buffer tb)))

(defun repeated-delete-indentation ()
  (interactive)
  (repeat-command #'delete-indentation))

(defun selective-display-beyond-col (arg)
  "Hide all text beyond current column."
  (interactive "P")
  (if arg
      (set-selective-display nil)
    (set-selective-display (1+ (current-column)))))

(defun auto-fill-current-line ()
  "Break current line with `do-auto-fill'."
  (interactive)
  (do-auto-fill))

(defun unfill-paragraph ()
  "Unfill paragraph removing hard newlines.
From Emacs Wiki."
  (interactive)
  (let* ((inhibit-read-only t)
         (fill-column (point-max)))
    (fill-paragraph nil)))

(def-lineup fill-or-unfill
  "Sequence filling/unfilling with the same biding."
  #'auto-fill-current-line
  #'fill-paragraph
  #'unfill-paragraph)

(defun comment-auto-fill ()
  "Auto-fill comments."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode +1))

(defun just-one-below ()
  "Join following line with this line."
  (interactive)
  (delete-indentation +1))

(defmacro with-timing (trace-id &rest forms)
  "Run FORMS as they are showing time taken to execute them.
TRACE-ID is printed along with the timing to recognize what was
being timed.

Shows a message only when the forms take more than a second."
  (let ((nowvar (make-symbol "nowvar"))
        (elapsed (make-symbol "elapsed")))
    `(let ((,nowvar (current-time)))
       ,@forms
       (let ((,elapsed (float-time (time-subtract (current-time) ,nowvar))))
         (when (< 1 ,elapsed)
           (message "[%s] took [%s] seconds" ,trace-id ,elapsed))))))

(defun indian-math-group-float (f)
  "Group floating point numbers in calc-mode.
This doesn't affect integers probably because calc uses a
different function for formatting those."
  (shell-command-to-string (concat "printf \"%'f\" " f)))

(defun restclient-do-it-with-curl ()
  "Execute current request with curl, with key and certs.
Define variables :key, :cert and :cacert as absolute paths to the
respective files."
  (interactive)
  (require 'restclient)
  (restclient-http-parse-current-and-do
   (lambda (method url headers entity)
     (let* ((vars (restclient-find-vars-before-point))
            (restclient-display-buffer-name (if restclient-same-buffer-response
                                                restclient-same-buffer-response-name
                                              (format "*HTTP %s %s*" method url)))
            ;; Extra arguments to curl for handling TLS stuff: certificates and keys
            (keyfile (assoc-default ":key" vars))
            (ca-cert (assoc-default ":cacert" vars))
            (client-cert (assoc-default ":cert" vars))
            (header-args (apply 'append (mapcar (lambda (header)
                                                  (list "-H" (format "%s: %s"
                                                                     (car header)
                                                                     (cdr header))))
                                                headers)))
            (curl-command (concat "curl "
                                  (mapconcat 'shell-quote-argument
                                             (append '("-sD" "-")
                                                     (when keyfile `("--key" ,keyfile))
                                                     (when ca-cert `("--cacert" ,ca-cert))
                                                     (when client-cert `("--cert" ,client-cert))
                                                     header-args
                                                     (list (concat "-X" method))
                                                     (list url)
                                                     (when (> (string-width entity) 0)
                                                       (list "-d" entity)))
                                             " "))))
       (with-temp-buffer
         (insert (shell-command-to-string curl-command))
         ;; Remove  characters
         (goto-char (point-min))
         (replace-regexp "" "")
         (restclient-http-handle-response nil
                                          method
                                          url
                                          restclient-display-buffer-name
                                          nil
                                          t
                                          ))))))


(defun backward-delete-word ()
  "Delete word before point without putting it into the `kill-ring'."
  (interactive)
  (delete-region (point) (progn (forward-word -1) (point))))


(defun websearch-it (&optional base-url)
  "Search the region between mark and point using base-url.

If BASE-URL is not nil, use it as the URL template to insert the
search keyword."
  (interactive)
  (letrec ((query (if (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (read-string "Query: " (word-at-point))))
           (url (format (or base-url "https://duckduckgo.com/?q=%s") query)))
    (eww-browse-url url)))

(defun search-linguee ()
  "Search for word at Linguee.com."
  (interactive)
  (websearch-it "https://www.linguee.com/english-german/search?source=auto&query=%s"))

(defun translate-with-linguee ()
  "Use https://www.deepl.com/translator#de/en/ for translating German -> English."
  (interactive)
  (websearch-it "https://www.deepl.com/translator#de/en/%s"))

(defun upcase-last-symbol-and-space ()
  "Up-case the last keyword and end it with a space."
  (interactive)
  (upcase-region (save-excursion
                   (re-search-backward (rx (or (syntax whitespace)
                                               line-start))
                                       (point-min) t))
                 (point))
  (insert " "))

(defcustom mode-line-current-weather nil
  "Display current weather information in the mode-line.")

(defvar current-weather nil "Information about current weather")
(defun current-weather ()
  (when-let ((_ mode-line-current-weather)
             (api-key (-> (auth-source-search :host "openweathermap.org")
                          car
                          (plist-get :appid))))
    (request "https://api.openweathermap.org/data/2.5/weather"
             :params `((lat   . ,calendar-latitude)
                       (lon   . ,calendar-longitude)
                       (appid . ,api-key)
                       (units . "metric"))

             :parser #'json-read

             :error
             (lambda (&rest _args)
               (message "Failed to fetch weather info from openweathermap.org"))

             :success
             (lambda (&rest args)
               (let ((data (plist-get args :data)))
                 (setq current-weather
                       (propertize (format "%s°C"
                                           (->> data
                                                (alist-get 'main)
                                                (alist-get 'temp)))
                                   'help-echo
                                   (format "Updated at: %s\n%s"
                                           (format-time-string "%FT%T%z"(current-time))
                                           (pp-to-string data))
                                   'timestamp
                                   (current-time))))))))


(defun start-emacspeak ()
  (interactive)
  (load-file (expand-file-name "~/code/emacspeak/lisp/emacspeak-setup.el")))

(defun preserve-kill-ring (original-fn &rest args)
  "Advice function for preserving `kill-ring' and
`kill-ring-yank-pointer'."
  (let* ((kill-ring* (copy-sequence kill-ring))
         (kill-ring-yank-pointer* kill-ring*))
    (apply original-fn args)
    (setq kill-ring kill-ring*
          kill-ring-yank-pointer kill-ring-yank-pointer*)))

(defun show-emacs-configuration ()
  "Display version and configuration of the running Emacs."
  (interactive)
  (display-message-or-buffer
   (format "Version: %s\n Configuration: \n\tSystem: %s\n\tConfigure flags: %s\n\tEnabled Opts: %s"
           (emacs-version)
           system-configuration
           system-configuration-options
           system-configuration-features)))

(defun switch-to-scratch-new-tab ()
  (interactive)
  (tab-bar-new-tab)
  (switch-to-buffer "*scratch*"))


(provide 'defs)
;;; defs.el ends here
