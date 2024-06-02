;;; llms-usage.el --- Track token usage and estimate costs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: data

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

(require 'gptel-openai)
(require 'savehist)

(add-to-list 'savehist-additional-variables
             'llms-chat--accumulated-costs)
(defvar llms-chat--accumulated-costs 0)

(defun llms-chat--openrouter-model-name (model-name)
  "Given a general model-name, returnt the closest openrouter model name (if possible).
We use this information to estimate costs."
  (pcase llms-chat--last-used-model
    (`"llama3-70b-8192" "meta-llama/llama-3-70b")
    (`"sonar-medium-online" "perplexity/llama-3-sonar-large-32k-online")
    (`"gemini-1.5-pro" "google/gemini-pro-1.5")
    (`"gemini-1.5-flash" "google/gemini-flash-1.5")
    (t llms-chat--last-used-model)))

(defun llms-chat-openrouter-estimte-cost (model-name prompt-tokens completion-tokens)
  (when-let* ((model-name (llms-chat--openrouter-model-name model-name))
              (model-info (llms-chat-openrouter-model-search model-name))
              (pricing (plist-get model-info :pricing))
              (prompt-pricing (plist-get pricing :prompt))
              (completion-pricing (plist-get pricing :completion)))
    (+ (* prompt-tokens (string-to-number prompt-pricing))
       (* completion-tokens (string-to-number completion-pricing)))))

(cl-defgeneric llms-chat--model-usage (backend response)
  (:documentation "Extract usage metadata from model response for a gptel backend."))

(cl-defmethod llms-chat--model-usage ((backend gptel-openai) response)
  (let* ((usage-info (plist-get response :usage))
         (total-tokens (plist-get usage-info :total_tokens))
         (prompt-tokens (plist-get usage-info :prompt_tokens))
         (completion-tokens (plist-get usage-info :completion_tokens))
         (cost (llms-chat-openrouter-estimte-cost llms-chat--last-used-model
                                                  prompt-tokens
                                                  completion-tokens)))
    (list :tokens total-tokens :cost cost)))

(cl-defmethod llms-chat--model-usage ((backend gptel-gemini) response)
  (let* ((usage-info (plist-get response :usageMetadata))
         (total-tokens (plist-get usage-info :totalTokenCount))
         (prompt-tokens (plist-get usage-info :promptTokenCount))
         (completion-tokens (plist-get usage-info :candidatesTokenCount))
         (cost (llms-chat-openrouter-estimte-cost llms-chat--last-used-model
                                                  prompt-tokens
                                                  completion-tokens)))
    (list :tokens total-tokens :cost cost)))

(cl-defmethod gptel--parse-response :before (backend response _info)
  (let ((model-usage (llms-chat--model-usage backend response))
        (mode-line-string))

    (when-let ((tokens (plist-get model-usage :tokens)))
      (setq mode-line-string
            (number-to-string (/ tokens 1000.0))))

    (when-let ((cost (plist-get model-usage :cost)))
      (setq llms-chat--accumulated-costs
            (+ llms-chat--accumulated-costs cost))
      (setq mode-line-string
            (propertize (format "%sk @ ¢%.4f/$%.4f"
                                mode-line-string
                                (* cost 100)
                                llms-chat--accumulated-costs)
                        'face 'mode-line-highlight)))
    (setq llms-chat--mode-line-string mode-line-string)
    (run-with-timer 4 nil (lambda ()
                            (setq llms-chat--mode-line-string nil)))))


(provide 'llms-usage)
;;; llms-usage.el ends here
