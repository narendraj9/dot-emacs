;;; wordly.el --- Word lookup using Pearson/Webster API  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience, tools
;; Version: 0.1
;; Package-Requires: ((pcache "0.4.2") (popup "0.5.3") (emacs "24.3"))

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

;;
;; A library for searching Pearson and Webster's online dictionaries for
;; word definitions and word synonyms.
;;
;; Simple setup:
;; (setq wordly-pearson-consumer-key "<key from http://api.pearson.com")
;; (setq wordly-webster-thesaurus-key "<key from http://www.dictionaryapi.com/api")
;;
;;  Then, you can lookup the defintion of word at point with:
;;       M-x wordly-define-word-at-point
;;  and the synonyms for the word at point with
;;       M-x wordly-show-synonyms-for-word-at-point
;;
;; If you use `use-package`, you can do something like this:
;;
;; (use-package wordly
;;   :load-path "<path/to/wordly/>"
;;   :pin manual
;;   :bind (("C-c d" . wordly-define-word-at-point)
;;          ("C-c s" . wordly-show-synonyms-for-word-at-point))

;;; Code:

(require 'pcache)
(require 'popup)
(require 'json)
(require 'url-util)
(require 'lv)

(defgroup wordly nil
  "Customization group for wordly."
  :group 'environment)

(defcustom wordly-pearson-consumer-key nil
  "Key for accessing the Pearson's dictionaries."
  :group 'wordly
  :type 'string)

(defcustom wordly-webster-thesaurus-key nil
  "Key for the Merriam-Webster's Thesaurus."
  :group 'wordly
  :type 'string)

(defcustom wordly-pearson-results-limit 6
  "The number of results to fetch."
  :group 'wordly
  :type 'number)

(defvar wordly-pearson-base-url
  "https://api.pearson.com"
  "Base url for the Pearson API.")

(defvar wordly-webster-base-url
  "http://www.dictionaryapi.com/api"
  "Base url for the Merriam-Webster's API.")

(defvar wordly-pearson-pcache-repo
  (when (version< "25.0" emacs-version)
    (pcache-repository :file "wordly-pearson"))
  "Pcache repository for the Pearson API.")

(defvar wordly-webster-pcache-repo
  (when (version< "25.0" emacs-version)
    (pcache-repository :file "wordly-webster"))
  "Pcache repository for the Pearson API.")

(defun wordly-pearson-build-url (word &optional synonymp)
  "Builds the url end point for finding WORD in REFERENCE.
Optional argument SYNONYMP is true if we are searching for synonyms."
  (url-encode-url (format "%s/v2/dictionaries%s/entries?apikey=%s&limit=%s&%s=%s"
                          wordly-pearson-base-url
                          (if synonymp
                              ""
                            "/ldoce5")
                          wordly-pearson-consumer-key
                          wordly-pearson-results-limit
                          (if synonymp
                              "synonyms"
                            "headword")
                          word)))

(defun wordly-pearson-fetch-parse-do (url parser action)
  "Fetch `URL` and parse it with PARSER and do ACTION on parsed value.
`PARSER` parses the contents of the buffer from point"
  (let* ((url-request-method "GET")
         (query-buffer (current-buffer))
         (query-point (point))
         (url-callback `(lambda (status)
                          (search-forward "\n\n")
                          (,action (,parser) ,query-buffer ,query-point)
                          (kill-buffer))))
    (url-retrieve url url-callback)))

(defun wordly-pearson-parser ()
  "Parsers the current buffer from `point` to return a string."
  (let* ((text (buffer-substring-no-properties (point) (point-max)))
         (decoded-text (decode-coding-string text 'utf-8))
         (json-response (json-read-from-string decoded-text))
         (results (assoc-default 'results json-response)))
    (if (equal results [])
        "Couldn't find anything :("
      (mapconcat 'wordly-pearson-parser-result results "\n"))))

(defun wordly-pearson-parser-result (r)
  "Parse a result json object.
Argument R is the parsed json object."
  (let* ((headword (assoc-default 'headword r))
         (ipa (assoc-default 'ipa (elt (assoc-default 'pronunciations r) 0)))
         (part-of-speech (assoc-default 'part_of_speech r))
         (senses (mapconcat 'wordly-pearson-parser-sense
                              (assoc-default 'senses r)
                              "\n")))
    (format "%s: %s %s\n%s"
            part-of-speech
            headword
            (wordly-pearson-parser-string-or-empty "(%s)" ipa)
            senses)))

(defun wordly-pearson-parser-string-or-empty (f s)
  "Format string with F as argument to `format' if S is non-empty.

Otherwise return empty string."
  (if (or (not s) (equal s ""))
      ""
    (format f s)))

(defun wordly-pearson-parser-sense (s)
  "Return the string representation for a sense object from Pearson.
Argument S is the parsed object."
  (let* ((definitions (assoc-default 'definition s))
         (examples (assoc-default 'examples s))
         (ces (assoc-default 'collocation_examples s)))
    (format ": %s\n%s%s%s%s"
            (mapconcat 'identity definitions "\n")
            (wordly-pearson-parser-string-or-empty
             "⟶ %s\n"
             (mapconcat
              (lambda (e) (assoc-default 'collocation e))
              ces
              " | "))
            (wordly-pearson-parser-string-or-empty
             " • %s\n"
             (mapconcat
              (lambda (e)
                (assoc-default 'text (assoc-default 'example e)))
              ces
              "\n"))
            (wordly-pearson-parser-string-or-empty
             " • %s\n"
             (mapconcat
              (lambda (e)
                (assoc-default 'text e))
              examples
              "\n"))
            (wordly-pearson-parser-string-or-empty
             "synonym: %s\n"
             (assoc-default 'synonym s)))))

(defun wordly-pearson-show-popup (s query-buffer query-point)
  "Show string S in a popup in QUERY-BUFFER at the position QUERY-POINT."
  (with-current-buffer query-buffer
    (popup-tip s
               :point query-point
               :margin t
               :around t
               :truncate nil
               :scroll-bar t)))

(defun wordly-define-word (word)
  "Provide definition for `WORD`."
  (let* ((pcache-word-key (intern (format "def:%s"
                                  (substring-no-properties word))))
         (entry-in-cache (pcache-get wordly-pearson-pcache-repo
                                     pcache-word-key)))
    (if entry-in-cache
        (wordly-pearson-show-popup entry-in-cache
                                   (current-buffer)
                                   (point))
      (wordly-pearson-fetch-parse-do
       (wordly-pearson-build-url word)
       'wordly-pearson-parser
       (lambda (s qbuffer qpoint)
         (wordly-pearson-show-popup s qbuffer qpoint)
         (pcache-put wordly-pearson-pcache-repo
                     pcache-word-key
                     s))))))

(defun wordly-find-synonyms (word)
  "Find out synonyms for WORD.
Since the pearson api isn't very good at finding synonyms, I am
using the Merriam Webster's API for the task."
  (let* ((pcache-word-key (intern (format "syn:%s"
                                          (substring-no-properties word))))
         (entry-in-cache (pcache-get wordly-webster-pcache-repo
                                     pcache-word-key)))
    (if entry-in-cache
        (wordly-pearson-show-popup entry-in-cache
                                   (current-buffer)
                                   (point))
      (wordly-pearson-fetch-parse-do
       (url-encode-url (format "%s/v1/references/thesaurus/xml/%s?key=%s"
                               wordly-webster-base-url
                               word
                               wordly-webster-thesaurus-key))
       `(lambda ()
          "The Great Anynymous Parser for the Merriam-Webster's Thesaurus."
          (while (re-search-forward "<it>\\|</it>" nil t)
            (replace-match "" nil nil))
          (goto-char (point-min))
          (search-forward "\n\n")
          (let* ((root (car (xml-parse-region (point) (point-max))))
                 (entries (xml-get-children root 'entry))
                 (sense (car (xml-get-children (car entries) 'sens)))
                 (synonyms (xml-node-children (car (xml-get-children sense
                                                                     'syn))))
                 (antonyms (xml-node-children (car (xml-get-children sense
                                                                     'ant)))))
            (format  "%s\n• Synonyms: %s\n• Antonyms: %s"
                     ,word
                     (and synonyms
                          (replace-regexp-in-string "\\(\\|\\)" ""  (car synonyms)))
                     (and antonyms
                          (replace-regexp-in-string "\\(\\|\\)" "" (car antonyms))))))
       (lambda (s qbuffer qpoint)
         (wordly-pearson-show-popup s qbuffer qpoint)
         (pcache-put wordly-webster-pcache-repo
                     pcache-word-key
                     s))))))


(defun wordly-lookup-word (word dict fallback-function)
  "Lookup a given WORD in the dictionary DICT or fallback to FALLBACK-FUNCTION.
Currently I think the online dictionary is more useful so I have
an invalid command name 'sdcv-invalid'."
  (if  (executable-find "sdcv-invalid")
      (popup-tip (shell-command-to-string
                  (format "sdcv -nu \"%s\" %s %s"
                          (shell-quote-argument dict)
                          (shell-quote-argument word)
                          " | tail -n +5 ")))
    (funcall fallback-function word)))


(defun wordly-lookup-word-at-point (dict fallback-function)
  "Generic helper function for `define-word-at-point'.

See also `wordl-show-synonyms-for-word-at-point'.
Argument DICT is the name of dictionary.
Argument FALLBACK-FUNCTION is used when DICT fails."
  (let* ((word (word-at-point)))
    (cond (mark-active
           (wordly-lookup-word (buffer-substring (mark) (point))
                        dict
                        fallback-function))
          (word
           (wordly-lookup-word word dict fallback-function))
          (t
           (wordly-lookup-word
              (read-from-minibuffer "No word at point. Enter word: ")
              dict
              fallback-function)))))

;;;###autoload
(defun wordly-define-word-at-point ()
    "Show the definition of the word at point.

Assumes that popup.el is already loaded, wordnet dictionary is available
and sdcv is installed or the credentials for Pearson API are set properly."
    (interactive)
    (wordly-lookup-word-at-point "WordNet"
                                 'wordly-define-word))

;;;###autoload
(defun wordly-show-synonyms-for-word-at-point ()
  "Show synonyms similar to `wordly-define-word-at-point'."
  (interactive)
  (wordly-lookup-word-at-point "Moby Thesaurus II"
                               'wordly-find-synonyms))

(provide 'wordly)

;;; wordly.el ends here
