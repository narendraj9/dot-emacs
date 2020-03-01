# wordly


Lookup word defintions and synonyms with dict, Pearson or Webster
API's.

![Wordly](_assets/demo.gif?raw=true "Wordly")

## Installation

Clone this repository or download it as zip. Add it to your `load-path`:

      (add-to-list 'load-path "/path/to/wordly/dir/")


## Setup


```elisp

;; Simple setup:
(setq wordly-pearson-consumer-key "<key from http://api.pearson.com")
(setq wordly-webster-thesaurus-key "<key from http://www.dictionaryapi.com/api")

;;
;;  Then, you can lookup the defintion of word at point with:
;;       M-x wordly-define-word-at-point
;;  and the synonyms for the word at point with
;;       M-x wordly-show-synonyms-for-word-at-point
;;
;; If you use `use-package`, you can do something like this:
;;
   (use-package wordly
     :load-path "<path/to/wordly/>"
     :pin manual
     :bind (("C-c d" . wordly-define-word-at-point)
            ("C-c s" . wordly-show-synonyms-for-word-at-point)))
```
