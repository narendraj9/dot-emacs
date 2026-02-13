;;; llms-writing.el --- LLMs for writing better.     -*- lexical-binding: t; -*-

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

(require 'cl-lib)

;; Strip RET from inline-diff's overlay keymap so it doesn't hijack
;; normal editing.  The M-prefixed bindings (M-RET, M-DEL, M-n, M-p)
;; remain available.
(with-eval-after-load 'inline-diff
  (keymap-unset inline-diff-overlay-map "RET" t)
  (keymap-unset inline-diff-overlay-map "<ret>" t))

(defvar-local llms-writing--last-instruction nil
  "Last rewrite instruction used in this buffer.")

(defvar-local llms-writing--dismissing nil
  "Non-nil while `llms-writing-dismiss-inline-diff' is running.
Used to prevent recursion between dismiss and mode deactivation.")

(defvar llms-writing--instruction-history nil
  "History of rewrite instructions for `read-string'.")

(defun llms-writing--get-surrounding-paragraph ()
  "Return the bounds of the paragraph around point as (BEG . END).
A paragraph is delimited by blank lines or buffer boundaries."
  (cons (save-excursion
          (if (re-search-backward "\n\n" (point-min) t)
              (match-end 0)
            (point-min)))
        (save-excursion
          (if (re-search-forward "\n\n" (point-max) t)
              (match-beginning 0)
            (point-max)))))

(defun llms-writing--diff-count ()
  "Return the number of inline-diff overlays in the buffer."
  (cl-count-if (lambda (ov)
                 (eq (overlay-get ov 'category) 'inline-diff-overlay))
               (overlays-in (point-min) (point-max))))

;;; Review minor mode

(defun llms-writing--review-accept ()
  "Accept the inline-diff change at point and advance to next."
  (interactive)
  (when-let* ((ov (inline-diff--overlay-at (point))))
    (inline-diff-apply ov)
    (let ((remaining (llms-writing--diff-count)))
      (if (zerop remaining)
          (message "All changes accepted")
        (inline-diff-next 1)
        (message "%d change%s remaining" remaining
                 (if (= remaining 1) "" "s"))))))

(defun llms-writing--review-reject ()
  "Reject the inline-diff change at point and advance to next."
  (interactive)
  (when-let* ((ov (inline-diff--overlay-at (point))))
    (inline-diff-reject ov)
    (let ((remaining (llms-writing--diff-count)))
      (if (zerop remaining)
          (message "All changes dismissed")
        (inline-diff-next 1)
        (message "%d change%s remaining" remaining
                 (if (= remaining 1) "" "s"))))))

(defun llms-writing--review-accept-all ()
  "Accept all remaining inline-diff changes."
  (interactive)
  (let ((inline-diff-move-after-acting nil)
        (count 0))
    (save-excursion
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (and (overlay-buffer ov)
                   (eq (overlay-get ov 'category) 'inline-diff-overlay))
          (goto-char (overlay-start ov))
          (inline-diff-apply ov)
          (cl-incf count))))
    (message "Accepted all %d change%s" count (if (= count 1) "" "s"))))

(defun llms-writing--redo ()
  "Dismiss current diffs and redo the rewrite with the last instruction."
  (interactive)
  (llms-writing-dismiss-inline-diff)
  (llms-writing-rewrite-inline (or llms-writing--last-instruction "")))

(defun llms-writing--redo-with-new ()
  "Dismiss current diffs and redo with a new instruction."
  (interactive)
  (llms-writing-dismiss-inline-diff)
  (call-interactively #'llms-writing-rewrite-inline))

(defvar-keymap llms-writing-review-map
  :doc "Keymap for `llms-writing-review-mode'."
  "n"   #'inline-diff-next
  "p"   #'inline-diff-previous
  "a"   #'llms-writing--review-accept
  "d"   #'llms-writing--review-reject
  "A"   #'llms-writing--review-accept-all
  "r"   #'llms-writing--redo
  "R"   #'llms-writing--redo-with-new
  "q"   #'llms-writing-dismiss-inline-diff)

(defun llms-writing--maybe-exit-review ()
  "Disable review mode when no inline-diff overlays remain."
  (when (and llms-writing-review-mode
             (zerop (llms-writing--diff-count)))
    (llms-writing-review-mode -1)))

(define-minor-mode llms-writing-review-mode
  "Minor mode for reviewing inline diffs."
  :lighter " Review"
  :keymap llms-writing-review-map
  (if llms-writing-review-mode
      (progn
        (add-hook 'post-command-hook #'llms-writing--maybe-exit-review nil t)
        (message "Review: [n/p] navigate [a/d] accept/reject [A] accept all [r] redo [R] new [q] dismiss"))
    (remove-hook 'post-command-hook #'llms-writing--maybe-exit-review t)
    (unless llms-writing--dismissing
      (llms-writing-dismiss-inline-diff))))

(defun llms-writing--enter-review ()
  "Enable review mode and jump to the first change."
  (inline-diff-next 1)
  (llms-writing-review-mode 1))

;;;###autoload
(defun llms-writing-rewrite-inline (instruction)
  "Rewrite the current paragraph or region in-place using inline diff.
INSTRUCTION is the prompt for the LLM rewrite.  If empty, reuses
the last instruction or the `writing' directive from `gptel-directives'.

After the rewrite arrives, `llms-writing-review-mode' activates:
  n/p  navigate between changes
  a    accept change at point (auto-advances)
  d    reject change at point (auto-advances)
  A    accept all remaining changes
  r    dismiss and redo with same instruction
  R    dismiss and redo with new instruction
  q    dismiss all changes
The mode auto-disables when all changes are handled.
\\[llms-writing-dismiss-inline-diff] dismisses from anywhere."
  (interactive
   (list (read-string "Instruction (empty to reuse last): "
                      nil 'llms-writing--instruction-history)))
  (require 'inline-diff)
  ;; Clear any existing inline diffs first.
  (llms-writing-dismiss-inline-diff)
  (unless (string-empty-p instruction)
    (setq llms-writing--last-instruction instruction))
  (let* ((beg (if (region-active-p) (region-beginning)
               (car (llms-writing--get-surrounding-paragraph))))
         (end (if (region-active-p) (region-end)
               (cdr (llms-writing--get-surrounding-paragraph))))
         (original-text (buffer-substring-no-properties beg end))
         (source-buffer (current-buffer))
         (beg-marker (copy-marker beg))
         (end-marker (copy-marker end t))
         (gptel-backend llms-chat-gptel-openai-backend)
         (gptel-model 'gpt-4o)
         (system-prompt (or llms-writing--last-instruction
                            (alist-get 'writing gptel-directives))))
    (when (region-active-p) (deactivate-mark))
    (pulse-momentary-highlight-region beg end)
    (message "Rewriting...")
    (gptel-request original-text
      :system system-prompt
      :callback
      (lambda (response info)
        (if (not response)
            (message "LLM rewrite failed: %s" (plist-get info :status))
          (when (buffer-live-p source-buffer)
            (with-current-buffer source-buffer
              (let ((inline-diff-do-refinement nil))
                (inline-diff-words (marker-position beg-marker)
                                   (marker-position end-marker)
                                   response))
              (set-marker beg-marker nil)
              (set-marker end-marker nil)
              (llms-writing--enter-review))))))))

;;;###autoload
(defun llms-writing-dismiss-inline-diff ()
  "Remove all inline-diff overlays, keeping original text.
Works from anywhere in the buffer."
  (interactive)
  (when (bound-and-true-p llms-writing-review-mode)
    (let ((llms-writing--dismissing t))
      (llms-writing-review-mode -1)))
  (let ((inline-diff-move-after-acting nil)
        (found nil))
    (save-excursion
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (and (overlay-buffer ov)
                   (eq (overlay-get ov 'category) 'inline-diff-overlay))
          (goto-char (overlay-start ov))
          (inline-diff-reject ov)
          (setq found t))))
    (when found
      (message "Dismissed all inline diffs"))))

(provide 'llms-writing)
;;; llms-writing.el ends here
