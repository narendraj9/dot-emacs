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
