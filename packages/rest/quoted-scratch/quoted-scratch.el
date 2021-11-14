;;; quoted-scratch.el --- Quotes for your scratch buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: quotes, lore, wisdom, data
;; Version: 0.1
;; Package-Requires: ((emacs "24"))

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

;; For displaying a random quote on Emacs startup.  I use it for
;; maintianing a list of quotes as well.
;;
;;   (require 'quoted-scratch)
;;   (setq initial-scratch-message nil)
;;   (add-hook 'emacs-startup-hook
;;             (lambda ()
;;               (run-with-timer 1 nil 'quoted-scratch-refresh-scratch-buffer)
;;               (quoted-scratch-refresh-quote-when-idle)))
;;
;; To add new quotes to `quoted-scratch-personal-quotes-file', use M-x
;; `quoted-scratch-add-new-quote'.  If you want the quote to be fetched from
;; http://quotes.rest/qod.json, use `quoted-scratch-fetch-qod'.
;;

;;; Code:

(require 'json)
(require 'url)
(require 'pulse)

(defgroup quoted-scratch nil
  "Customization group for `quoted-scratch'."
  :group 'environment)

(defface quoted-scratch-quote-face
  '((((class color) (background dark))
     (:foreground "LemonChiffon" :height 1.2))
    (t (:foreground "Dark Blue" :height 1.2)))
  "Face for a quote."
  :group 'quoted-scratch)

(defface quoted-scratch-auroville-quality-face
  '((((class color) (background dark))
     (:foreground "Sienna" :height 3.0))
    (t (:foreground "Dark Slate Blue" :height 3.0)))
  "Face for showing an Auroville quality."
  :group 'quoted-scratch)

(defcustom quoted-scratch-show-auroville-quality t
  "Show an Auroville quality along with the quote."
  :group 'quoted-scratch
  :type 'face)

(defcustom quoted-scratch-quotes-source :quotes-file
  "Source for quotes: 1) Quotes file or http://quotes.rest/."
  :group 'quoted-scratch
  :type '(choice (const :quotes-file) (const :qod)))

(defcustom quoted-scratch-separator "\n"
  "String used to separate individual scratch strings."
  :group 'quoted-scratch
  :type 'string)

(defcustom quoted-scratch-scratchers '(quoted-scratch-generate-scratch-message)
  "List of functions which generate content for the scratch buffer.

The final string shown in the scratch buffer is the concatenation
of all strings generated by `quoted-scratch-scratchers' separated by `quoted-scratch-separator'."
  :group 'quoted-scratch
  :type '(repeat function))

(defconst quoted-scratch-script-directory
  (expand-file-name (file-name-directory (or load-file-name
                                             default-directory)))
  "The directory that this script is kept in.")

(defcustom quoted-scratch-personal-quotes-file
  (expand-file-name "_assets/quotes.txt" quoted-scratch-script-directory)
  "Path to the custom quotes file.  Must have quotes separated by a newline."
  :group 'quoted-scratch
  :type 'file)

(defcustom quoted-scratch-quote-idle-refresh-interval 60
  "Refresh scratch buffer quote after these many seconds of inactivity."
  :group 'quoted-scratch
  :type 'number)

(defcustom quoted-scratch-quotes
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents quoted-scratch-personal-quotes-file)
      (goto-char (point-min))
      (split-string
       (buffer-substring-no-properties (point-min) (point-max))
       "\n\n")))
  "Collection of quotes."
  :group 'quoted-scratch
  :type '(repeat string))


(defcustom quoted-scratch-properties `( read-only t
                                        syntax-table ,(standard-syntax-table) )
  "Symbol whose properties serve as the default properties of the
text text of the quotes in *scratch* buffer."
  :group 'quoted-scratch
  :type '(plist))

(defun quoted-scratch-go-to-starting-line ()
  "Function to go the first line that stars a new entry for anything.
Cleans up whitespace."
  (goto-char (point-max))
  (beginning-of-line)
  (while (looking-at "^\\s-*$")
    (forward-line -1))
  (end-of-line)
  (let ((times-yet-to-move (forward-line 2)))
    (dotimes (_ times-yet-to-move)
      (insert "\n"))))

(defun quoted-scratch-random-quote-string ()
  "Return a random quote."
  (if quoted-scratch-quotes
      (propertize (format "%s\n\n"
                          (nth (random (length quoted-scratch-quotes)) quoted-scratch-quotes))
                  'font-lock-face 'quoted-scratch-quote-face
                  'rear-nonsticky t)
    (message "No quotes defined. Maybe the quotes file wasn't parsed properly")))

(defun quoted-scratch-prepare-quote (quote &optional author)
  "Prepare a nicely formatted QUOTE from the arguments.
Optional argument AUTHOR is the name of the author."
  (let* ((author-line-space-count  (- fill-column (length author) 2))
         (author-line-string (format "%s - %s"
                                     (if (< author-line-space-count 0)
                                         ""
                                       (make-string author-line-space-count ? ))
                                     (or author "Unknown"))))
    (format "“%s”\n%s" quote author-line-string)))

(defun quoted-scratch-add-new-quote (quote &optional author)
  "Add a new QUOTE to the list of quotes.
Turn ‘quoted-scratch-quotes’ into a variable maintained with `customize-save-variable`.
Optional argument AUTHOR is what the word suggests but checkdoc was complaining so this sentence."
  (interactive "sQuote: \nsAuthor: ")
  (with-current-buffer (find-file-noselect quoted-scratch-personal-quotes-file)
    (quoted-scratch-go-to-starting-line)
    (insert (quoted-scratch-prepare-quote quote author))
    (save-buffer)
    (kill-buffer)))

(defun quoted-scratch-get-auroville-quality ()
  "Return one of the Auroville qualities."
  (let ((index (string-to-number (format-time-string "%d")))
        (qualities
         '(

           "
                  誠実
                Seijitsu
                Sincerity"

           "
                  謙虚
                 Kenkyo
                Humility "

           "
                  感謝
                 Kansha
                Gratitude "

           "
                 忍耐力
              Nintai-ryoku
              Perseverance "

           "
                  吸引
                 Kyūin
               Aspiration "

           "
                 感受性
                Kanjusei
               Receptivity "

           "
                  進捗
               Shinchoku
               Progress "

           "
                  勇気
                  Yūki
                 Courage "
           "
                   善
                  Zen
                Goodness "

           "
                 寛大さ
               Kandai-sa
               Generosity "

           "
                  平等
                 Byōdō
                Equality "

           "
                  平和
                 Heiwa
                 Peace ")))

    (propertize (nth  (mod index (length qualities)) qualities)
                'font-lock-face 'quoted-scratch-auroville-quality-face
                'rear-nonsticky t)))

(defun quoted-scratch-generate-scratch-message (&optional quote-string)
  "Generate message content for scratch buffer.
Make sure you set the :text-type text property to :quote-string.

If argument QUOTE-STRING is provided, use that as the quote."
  (propertize (format "%s%s"
                      (or quote-string (quoted-scratch-random-quote-string))
                      (if quoted-scratch-show-auroville-quality
                          (quoted-scratch-get-auroville-quality)
                        ""))
              ;; Distinguishing quote text from other text with a text
              ;; property.
              :text-type :quote-string
              'rear-nonsticky t))

(defun quoted-scratch-remove-text-with-property (start p v)
  "From point START, remove first chunk with prop P set to V.
This function returns the point value for the second of the
deleted text so that it can be called again with that value to
delete all text in a buffer."
  (let* ((beg (text-property-any start (point-max) p v))
         (end (and beg
                   (text-property-not-all beg (point-max) p v))))
    (and beg ; there is some text
         (delete-region beg (or end ; it's all of the text
                                (point-max))))
    beg))

(defun quoted-scratch-update-quote-text-in-scratch (quote-text)
  "Update quote text in *scratch* with QUOTE-TEXT."
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((quote-visible-p (pos-visible-in-window-p (point-min)))
          (here-marker (point-marker))
          (inhibit-read-only t))

      ;; Advance marker when we insert text at its position
      (set-marker-insertion-type here-marker t)

      ;; I might have fragmented the quote text in which case, I would
      ;; like to work only on the quote text and not change the other
      ;; unrelated text in the scratch buffer.
      (while (quoted-scratch-remove-text-with-property (point-min)
                                                       :text-type
                                                       :quote-string))
      ;; Now insert new quote at the top of the buffer
      (goto-char (point-min))
      (insert (propertize quote-text 'category 'quoted-scratch-properties))
      (let ((pulse-delay 0.10))
        (pulse-momentary-highlight-region (point-min)
                                          (point)
                                          'next-error))
      (font-lock-mode 1)
      (goto-char (marker-position here-marker))
      (when quote-visible-p
        (set-window-start (selected-window) (point-min))))))

(defun quoted-scratch-qod-callback (status)
  "Callback for ‘quoted-scratch-fetch-qod’ command.

This currently replaces the contents
of the *scratch* buffer with the quote string.

Argument STATUS is the http status of the request."
  (search-forward "\n\n")
  (if (not status)
      (let* ((quote-json (json-read))
             (quotes (assoc-default
                      'quotes (assoc-default
                               'contents quote-json)))
             (quote (aref quotes 0))
             (quote-string (assoc-default 'quote quote))
             (quote-author (assoc-default 'author quote))
             (quote* (propertize (quoted-scratch-prepare-quote quote-string
                                                   quote-author)
                                 'font-lock-face 'quoted-scratch-quote-face
                                 'rear-nonsticky t)))
        (quoted-scratch-update-quote-text-in-scratch (quoted-scratch-generate-scratch-message quote*)))
    (message "Error fetching quote: %s"
             (assoc-default 'message
                            (assoc-default 'error (json-read))))))

;;;###autoload
(defun quoted-scratch-refresh-scratch-buffer (&optional pop-to-bufferp)
  "Recreate and refresh the scracth buffer.
Optional argument POP-TO-BUFFERP makes the window pop to the buffer if non-nil."
  (interactive)
  (let ((window-start (window-start)))
    (quoted-scratch-update-quote-text-in-scratch (mapconcat 'funcall
                                                            quoted-scratch-scratchers
                                                            quoted-scratch-separator))
    (and pop-to-bufferp (pop-to-buffer "*scratch*"))
    (when (equal (selected-window)
                 (get-buffer-window "*scratch*"))
      (set-window-start (selected-window) window-start))))

;;;###autoload
(defun quoted-scratch-jump-to-scratch-buffer ()
  "Refresh and jump to scratch buffer."
  (interactive)
  (if-let ((b (get-buffer "*scratch*")))
      (progn (switch-to-buffer b)
             (delete-other-windows))
    (quoted-scratch-refresh-scratch-buffer t)))

;;;###autoload
(defun quoted-scratch-fetch-qod ()
  "Fetches quote of the day from theysaidso.com."
  (interactive)
  (with-current-buffer
      (let ((url-request-method "GET")
            (qod-service-url "http://quotes.rest/qod.json"))
        (url-retrieve (url-generic-parse-url qod-service-url)
                      'quoted-scratch-qod-callback))))

;;;###autoload
(defun quoted-scratch-refresh-quote-when-idle ()
  "Refresh quote in *scratch* when idle for `quoted-scratch-quote-idle-refresh-interval' seconds."
  (interactive)
  (setplist 'quoted-scratch-properties quoted-scratch-properties)
  (run-with-idle-timer quoted-scratch-quote-idle-refresh-interval
                       t
                       'quoted-scratch-refresh-scratch-buffer))

(provide 'quoted-scratch)

;;; quoted-scratch.el ends here
