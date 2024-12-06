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

(defun llms-chat--openrouter-model-name (model-name)
  "Given a general model-name, returnt the closest openrouter model name (if possible).
We use this information to estimate costs."
  (pcase model-name
    (`"llama3-70b-8192" "meta-llama/llama-3-70b")
    (`"sonar-medium-online" "perplexity/llama-3-sonar-large-32k-online")
    (`"gemini-1.5-pro" "google/gemini-pro-1.5")
    (`"gemini-1.5-flash" "google/gemini-flash-1.5")
    (_ model-name)))

(defun llms-chat-openrouter-estimte-cost (model-name prompt-tokens completion-tokens)
  (when-let* ((model-name (llms-chat--openrouter-model-name model-name))
              (model-info (llms-chat-openrouter-model-search model-name))
              (pricing (plist-get model-info :pricing))
              (prompt-pricing (plist-get pricing :prompt))
              (completion-pricing (plist-get pricing :completion)))
    (+ (* prompt-tokens (string-to-number prompt-pricing))
       (* completion-tokens (string-to-number completion-pricing)))))

(cl-defgeneric llms-chat--tokens-consumed (backend model-name response)
  (:documentation "Extract usage metadata from model response for a gptel backend.")
  nil)

(cl-defmethod llms-chat--tokens-consumed ((backend gptel-openai) model-name response)
  (let* ((usage-info (plist-get response :usage))
         (total-tokens (plist-get usage-info :total_tokens))
         (prompt-tokens (plist-get usage-info :prompt_tokens))
         (completion-tokens (plist-get usage-info :completion_tokens)))
    (list :prompt-tokens total-tokens
          :completion-tokens completion-tokens)))

(cl-defmethod llms-chat--tokens-consumed ((backend gptel-gemini) model-name response)
  (let* ((usage-info (plist-get response :usageMetadata))
         (total-tokens (plist-get usage-info :totalTokenCount))
         (prompt-tokens (plist-get usage-info :promptTokenCount))
         (completion-tokens (plist-get usage-info :candidatesTokenCount)))
    (list :prompt-tokens total-tokens
          :completion-tokens completion-tokens)))

(cl-defmethod gptel--parse-response :around (backend response info)
  (let* ((response-text (cl-call-next-method backend response info)))

    ;; Hack: I need to do this to make sure I can pass this information from
    ;; where it is available, i.e. here in this function, to the callback that
    ;; `gtpel-request' is going to call with the response string. I am attaching
    ;; the raw response from the LLM API as a text property of the string that
    ;; `gptel-request' extracts and passes onto the user supplied callback. I am
    ;; not replacing the definition of `gptel--parse-response' because I want to
    ;; make sure that the existing `gptel' features continue to work.
    (put-text-property 0 (length response-text) 'response response response-text)

    ;; Make sure to return the actual response text.
    response-text))

(defun llms-chat-model-usage (backend model-name response)
  ;; See: gptel--parse-response :around (backend response info) ^ above.
  (let* ((response (get-text-property 0 'response response))
         (token-usage (llms-chat--tokens-consumed backend model-name response))
         (prompt-tokens (plist-get token-usage :prompt-tokens))
         (completion-tokens (plist-get token-usage :completion-tokens)))
    `( :cost ,(llms-chat-openrouter-estimte-cost model-name
                                                 prompt-tokens
                                                 completion-tokens)
       :tokens ,(+ prompt-tokens completion-tokens)
       ,@token-usage)))

(defun llms-chat-model-usage->mode-line-string (model-usage)
  (let ((mode-line-string))
    (when-let* ((prompt-tokens (plist-get model-usage :prompt-tokens))
                (completion-tokens (plist-get model-usage :completion-tokens))
                (tokens (+ prompt-tokens completion-tokens)))
      (setq mode-line-string
            (number-to-string (/ tokens 1000.0))))

    (when-let* ((cost (plist-get model-usage :cost)))
      (setq mode-line-string
            (propertize (format "%sk @ ¢%.4f" mode-line-string (* cost 100))
                        'face 'mode-line-highlight)))
    mode-line-string))


(provide 'llms-usage)
;;; llms-usage.el ends here
