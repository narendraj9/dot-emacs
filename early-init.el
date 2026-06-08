;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup t
      ;; This needs to be set before Emacs starts loading `init.el' because
      ;; Emacs will `activate' packages installed in `package-user-dir' before
      ;; `init.el' is loaded.
      package-user-dir (expand-file-name "packages/elpa/"
                                         user-emacs-directory)
      ;; This combines all package `autoload' definitions and keeps them in a
      ;; single file.

      ;; Disabling quick start because upgrading packages takes a lot
      ;; longer. `package-quickstart-file' is compiled after each package is
      ;; upgraded.
      package-quickstart nil
      package-quickstart-file (expand-file-name "var/package-quickstart.el"
                                                user-emacs-directory)

      ;; Enable verbose logging to debug issues
      use-package-verbose nil)

;; --- Native compilation linker fix (macOS / Homebrew gcc) ---
;; libgccjit links each .eln by invoking whichever `gcc' is first on PATH.
;; Apple's /usr/bin/gcc (clang) links fine, but if a *real* GCC driver is
;; picked up (e.g. from a nix/dev shell on PATH), it emits `-lemutls_w'
;; without telling Apple's `ld' where gcc's target lib dir is, and the link
;; fails: "ld: library 'emutls_w' not found / error invoking gcc driver".
;; Add gcc's lib dirs as -L flags so the link succeeds whichever gcc is used.
;; Scoped to native-comp (unlike LIBRARY_PATH, it doesn't leak to subprocesses).
;; Paths are globbed so they survive gcc/macOS version bumps.
(when (eq system-type 'darwin)
  (let ((dirs (seq-filter
               #'file-directory-p
               (append (file-expand-wildcards
                        "/opt/homebrew/lib/gcc/current/gcc/*/*")
                       (list "/opt/homebrew/lib/gcc/current")))))
    (setq native-comp-driver-options
          (append (mapcar (lambda (d) (concat "-L" d)) dirs)
                  (and (boundp 'native-comp-driver-options)
                       native-comp-driver-options)))))
