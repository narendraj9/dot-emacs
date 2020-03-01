(deftheme vicarie-and-blackboard
  "Created 2016-07-04.")

(custom-theme-set-faces
 'vicarie-and-blackboard
 `(default ((t (:background "#0C1021" :foreground "#F8F8F8"))))
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "LemonChiffon"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Carlito" :width ultraexpanded :height 130 :weight normal))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 '(region ((t (:background "dark slate blue"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(font-lock-builtin-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "dark slate blue")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Orchid")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant italic :foreground "pale violet red"))))
 '(font-lock-constant-face ((((class grayscale) (background light)) (:underline (:color foreground-color :style line) :weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:underline (:color foreground-color :style line) :weight bold :foreground "Gray50")) (((class color) (min-colors 88) (background light)) (:foreground "dark cyan")) (((class color) (min-colors 88) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 16) (background light)) (:foreground "CadetBlue")) (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine")) (((class color) (min-colors 8)) (:foreground "magenta")) (t (:underline (:color foreground-color :style line) :weight bold))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background light)) (:foreground "Blue1")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Blue")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 8)) (:weight bold :foreground "blue")) (t (:weight bold :inverse-video t))))
 '(font-lock-keyword-face ((((class grayscale) (background light)) (:weight bold :foreground "LightGray")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "Purple")) (((class color) (min-colors 88) (background dark)) (:foreground "Cyan1")) (((class color) (min-colors 16) (background light)) (:foreground "Purple")) (((class color) (min-colors 16) (background dark)) (:foreground "Cyan")) (((class color) (min-colors 8)) (:weight bold :foreground "cyan")) (t (:weight bold))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "Aquamarine" :inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red" :inherit (bold)))))
 '(font-lock-string-face ((((class grayscale) (background light)) (:slant italic :foreground "DimGray")) (((class grayscale) (background dark)) (:slant italic :foreground "LightGray")) (((class color) (min-colors 88) (background light)) (:foreground "VioletRed4")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 16) (background light)) (:foreground "RosyBrown")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 8)) (:foreground "green")) (t (:slant italic))))
 '(font-lock-type-face ((((class grayscale) (background light)) (:weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:underline (:color foreground-color :style line) :weight bold))))
 '(font-lock-variable-name-face ((((class grayscale) (background light)) (:slant italic :weight bold :foreground "Gray90")) (((class grayscale) (background dark)) (:slant italic :weight bold :foreground "DimGray")) (((class color) (min-colors 88) (background light)) (:foreground "sienna")) (((class color) (min-colors 88) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod")) (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 8)) (:weight light :foreground "yellow")) (t (:slant italic :weight bold))))
 '(font-lock-warning-face ((t (:weight bold :foreground "pink" :inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "cyan1"))))
 '(link-visited ((t (:foreground "violet" :inherit (link)))))
 '(fringe ((t (:foreground "#2E2920" :background "#2E2920"))))
 '(header-line ((t (:box nil :foreground "grey90" :background "grey20" :inherit (mode-line)))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(mode-line ((t (:weight bold :box (:line-width -1 :color nil :style released-button) :foreground "Black" :background "bisque1"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:weight light :box (:line-width -1 :color "grey40" :style nil) :foreground "grey80" :background "grey30" :inherit (mode-line)))))
 '(isearch ((t (:foreground "white" :background "blue"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 `(next-error ((t (:inherit (region)))))
 `(query-replace ((t (:inherit (isearch)))))

 ;; org-mode
 `(org-hide ((t (:foreground "#2e3436"))))
 `(org-level-1 ((t (:bold nil :foreground "dodger blue" :height 1.3))))
 `(org-level-2 ((t (:bold nil :foreground "#edd400" :height 1.2))))
 `(org-level-3 ((t (:bold nil :foreground "#6ac214" :height 1.1))))
 `(org-level-4 ((t (:bold nil :foreground "tomato" :height 1.0))))

 `(org-date ((t (:underline t :foreground "magenta3"))))
 `(org-footnote  ((t (:underline t :foreground "magenta3"))))
 `(org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
 `(org-special-keyword ((t (:foreground "brown"))))
 `(org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
 `(org-block ((t (:foreground "#bbbbbc"))))
 `(org-quote ((t (:inherit org-block :slant italic))))
 `(org-verse ((t (:inherit org-block :slant italic))))
 `(org-todo ((t (:bold t :foreground "Red"))))
 `(org-done ((t (:bold t :foreground "ForestGreen"))))
 `(org-tag ((t (:weight bold :foreground "dark salmon" :slant italic))))

 `(org-agenda-structure ((t (:weight bold :foreground "LemonChiffon" :background "SteelBlue" :box (:color "black")))))
 `(org-agenda-date ((t (:foreground "#6ac214"))))
 `(org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))
 `(org-warning ((t (:weight bold :foreground "tomato"))))

 `(org-upcoming-deadline ((t (:weight normal :foreground "goldenrod"))))
 `(org-scheduled-previously ((t (:foreground "LightCoral"))))
 `(org-habit-alert-face ((t (:foreground "black" :background "gold"))))

 ;; company
 `(company-scrollbar-bg ((t (:background "#2a2a2a"))))
 `(company-scrollbar-fg ((t (:background "#1d1d1d"))))
 `(company-tooltip ((t (:background "#cccccc" :foreground "black"))))
 `(company-tooltip-selection ((t (:background "sandy brown"))))

 ;; helm
 `(helm-candidate-number ((t (:background "goldenrod" :foreground "black"))))
 `(helm-header-line-left-margin ((t (:background "chocolate" :foreground "black"))))
 `(helm-selection ((t (:background "steel blue" :distant-foreground "white"))))
 `(helm-source-header ((t (:background "saddle brown" :foreground "white" :weight bold :height 1.3 :family "Sans Serif"))))

 ;; Highlighted line in `hl-line-mode'
 `(hl-line ((t (:box (:line-width -1 :color "grey28" :style nil)))))
 `(highlight ((t (:background "grey28" :foreground "cornsilk"))))
 `(highline-face ((t (:background "SeaGreen"))))
 `(highline-symbol-face ((t (:background "SeaGreen"))))


 ;; Leading key-char for `ace-window'
 `(aw-leading-char-face ((t (:foreground "goldenrod" :height 2.0 :weight bold))))

 ;; Gnus
 `(gnus-cite-face-1 ((t (:foreground "#ad7fa8"))))
 `(gnus-cite-face-2 ((t (:foreground "sienna4"))))
 `(gnus-cite-face-3 ((t (:foreground "khaki4"))))
 `(gnus-cite-face-4 ((t (:foreground "PaleTurquoise4"))))
 `(gnus-group-mail-1-empty((t (:foreground "light cyan"))))
 `(gnus-group-mail-1((t (:bold t :foreground "light cyan"))))
 `(gnus-group-mail-2-empty((t (:foreground "turquoise"))))
 `(gnus-group-mail-2((t (:bold t :foreground "turquoise"))))
 `(gnus-group-mail-3-empty((t (:foreground "#729fcf"))))
 `(gnus-group-mail-3((t (:bold t :foreground "cornsilk" :height 1.1))))
 `(gnus-group-mail-low-empty((t (:foreground "dodger blue"))))
 `(gnus-group-mail-low((t (:bold t :foreground "dodger blue"))))
 `(gnus-group-news-1-empty((t (:foreground "light cyan"))))
 `(gnus-group-news-1((t (:bold t :foreground "light cyan"))))
 `(gnus-group-news-2-empty((t (:foreground "turquoise"))))
 `(gnus-group-news-2((t (:bold t :foreground "turquoise"))))
 `(gnus-group-news-3-empty((t (:foreground "#729fcf"))))
 `(gnus-group-news-3((t (:bold t :foreground "#edd400"))))
 `(gnus-group-news-low-empty((t (:foreground "dodger blue"))))
 `(gnus-group-news-low((t (:bold t :foreground "dodger blue"))))
 `(gnus-header-name ((t (:bold t :foreground "#729fcf"))))
 `(gnus-header-from ((t (:bold t :foreground "#edd400"))))
 `(gnus-header-subject ((t (:foreground "#edd400"))))
 `(gnus-header-content ((t (:italic t :foreground "#8ae234"))))
 `(gnus-header-newsgroups((t (:italic t :bold t :foreground "LightSkyBlue3"))))
 `(gnus-signature((t (:italic t :foreground "dark grey"))))
 `(gnus-summary-cancelled((t (:background "black" :foreground "yellow"))))
 `(gnus-summary-high-ancient((t (:bold t :foreground "royal blue"))))
 `(gnus-summary-high-read((t (:bold t :foreground "lime green"))))
 `(gnus-summary-high-ticked((t (:bold t :foreground "tomato"))))
 `(gnus-summary-high-unread((t (:bold t :foreground "white"))))
 `(gnus-summary-low-ancient((t (:italic t :foreground "lime green"))))
 `(gnus-summary-low-read((t (:italic t :foreground "royal blue"))))
 `(gnus-summary-low-ticked((t (:italic t :foreground "dark red"))))
 `(gnus-summary-low-unread((t (:italic t :foreground "white"))))
 `(gnus-summary-normal-ancient((t (:foreground "royal blue"))))
 `(gnus-summary-normal-read((t (:foreground "lime green"))))
 `(gnus-summary-normal-ticked((t (:foreground "indian red"))))
 `(gnus-summary-normal-unread((t (:foreground "white"))))
 `(gnus-summary-selected ((t (:background "brown4" :foreground "white"))))
 `(message-header-name((t (:foreground "tomato"))))
 `(message-header-newsgroups((t (:italic t :bold t :foreground "LightSkyBlue3"))))
 `(message-header-other((t (:foreground "LightSkyBlue3"))))
 `(message-header-xheader((t (:foreground "DodgerBlue3"))))
 `(message-header-subject ((t (:foreground "white"))))
 `(message-header-to ((t (:foreground "white"))))
 `(message-header-cc ((t (:foreground "white"))))

 ;; Face for `ido'
 `(ido-first-match ((t (:foreground "goldenrod" :weight bold))))
 `(ido-only-match ((t (:foreground "#ff982d" :weight bold))))
 `(ido-subdir ((t (:foreground "#8AE234"))))
 `(ido-virtual ((t (:foreground "#7c7c7c"))))

 ;; ERC
 `(erc-default-face ((t (:foreground "papaya whip"))))
 `(erc-input-face ((t (:foreground "burlywood"))))

 ;; Info quoted
 `(Info-quoted ((t (:family "Comic Sans MS" :foreground "papaya whip"))))

 ;; Faces for `'web-mode'
 `(web-mode-html-tag-face ((t (:foreground "tomato"))))
 `(web-mode-html-attr-face ((t (:foreground "yellow"))))

 ;; Vertical borders and window dividers
 `(vertical-border ((t (:foreground "peru" :background "peru"))))
 `(window-divider ((t (:foreground "peru" :background "peru"))))

 ;; linum-mode
 `(linum ((t (:foreground "steel blue" :background "black"))))

 ;; For reading news and (maybe) info
 `(variable-pitch ((t (:family "Carlito"))))

 ;; Elfeed
 `(elfeed-search-title-face ((((class color) (background light))
                              (:foreground "#000" :strike-through t))
                             (((class color) (background dark))
                              (:foreground "#fff" :strike-through t))))
 `(elfeed-search-unread-title-face ((t :weight bold
                                       :strike-through nil)))

 ;; Mode-line faces for `eyebrowse'.
 `(eyebrowse-mode-line-inactive ((t (:foreground "white"))))
 `(eyebrowse-mode-line-active ((t (:background "goldenrod" :foreground "black")))))


(provide-theme 'vicarie-and-blackboard)
