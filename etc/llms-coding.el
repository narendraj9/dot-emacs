;;; llms-coding.el --- Bind coding-agent keys to a launcher  -*- lexical-binding: t; -*-

;;; Commentary:

;; One indirection between my key bindings and whatever starts my coding
;; agents.  The bindings in init.el name `llms-coding-<agent>'; the
;; aliases below decide what that actually runs.  Moving off omnigent
;; means editing this file, not the bindings.
;;
;; Today every agent is an omnigent session: `omnigent-start' creates it
;; over the API, filed under the current project, then boots the harness
;; onto it.  `omnigent-run' is the exception, since `omni run' picks its
;; own agent.

;;; Code:

(require 'omnigent)

;;;###autoload (defalias 'llms-coding-claude #'omnigent-claude)
(defalias 'llms-coding-claude #'omnigent-claude)
;;;###autoload (defalias 'llms-coding-codex #'omnigent-codex)
(defalias 'llms-coding-codex #'omnigent-codex)
;;;###autoload (defalias 'llms-coding-pi #'omnigent-pi)
(defalias 'llms-coding-pi #'omnigent-pi)
;;;###autoload (defalias 'llms-coding-omni #'omnigent-run)
(defalias 'llms-coding-omni #'omnigent-run)

(provide 'llms-coding)
;;; llms-coding.el ends here
