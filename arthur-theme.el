;;  -*- mode: emacs-lisp; -*-

(defun color-theme-arthur-dark ()
"Color theme by Arthur Danskin."
  (interactive)
  (color-theme-install
   '(color-theme-arthur-dark
     ((background-color . "black")
      (foreground-color . "#a0a0a0")
      (background-mode . dark)
      (mouse-color . "white")
      (cursor-color . "grey80"))

     ((ansi-color-names-vector . ["grey30" "firebrick3" "green3" "goldenrod2"
                                  "RoyalBlue2" "VioletRed4" "cyan4" "white"])
      (ansi-term-color-vector . [unspecified "grey30" "firebrick3" "green3" "goldenrod2"
                                  "RoyalBlue2" "VioletRed4" "cyan4" "white"])
      (term-default-fg-color . "grey75")
      (term-default-bg-color . "black"))
     

     ;; general
     (default ((t (nil))))
     (fringe ((t (:background "#0d0715"))))
     (mode-line ((t (:background "#aa0825" :foreground "black"))))
     (mode-line-inactive ((t (:background "#0d0715" :foreground "#555e6e"))))
     (mode-line-highlight ((t (:background "grey60" :foreground "black"))))
     (mode-line-buffer-id ((t (:foreground unspecified :background unspecified))))
     (which-func ((t (:bold t :foreground unspecified))))
     (minibuffer-prompt ((t (:foreground "red2"))))

     (font-lock-comment-face ((t (:slant unspecified :foreground "#3b3959" :background unspecified))))
     (font-lock-comment-delimiter-face ((t (:foreground "#726f99" :background unspecified))))
     (font-lock-doc-face ((t (:foreground "DeepPink4" :background unspecified))))
     (font-lock-string-face ((t (:foreground "#b7ffff" :background unspecified))))
     (font-lock-regexp-grouping-construct ((t (:foreground "PaleGreen3"))))
     (font-lock-regexp-grouping-backslash ((t (:foreground "OliveDrab3"))))
     (font-lock-keyword-face ((t (:bold t :foreground "#e90e3a"))))
     (font-lock-constant-face ((t (:foreground "#a090c0"))))
;     (font-lock-type-face ((t (:foreground "#0F7889"))))
     (font-lock-type-face ((t (:foreground "#3f1935"))))
;     (font-lock-variable-name-face ((t (:slant unspecified :foreground "aquamarine4"))))
     (font-lock-variable-name-face ((t (:italic t :foreground "#482b49"))))
     (font-lock-function-name-face ((t (:bold t :foreground "#aa0825"))))
     (font-lock-builtin-face ((t (:foreground "#740624"))))
     (font-lock-preprocessor-face ((t (:foreground "#d73a6c"))))
     (font-lock-negation-char-face ((t (:foreground "#aa0825"))))
     (font-lock-warning-face ((t (:bold t :foreground "OrangeRed3"))))
     (tuareg-font-lock-governing-face ((t (:bold t :foreground "OrangeRed" :weight bold))))
     (tuareg-font-lock-operator-face ((t (:foreground "SeaGreen"))))

     (highline-face ((t (:background "grey12"))))
     (setnu-line-number-face ((t (:background "grey15" :foreground "white" :bold t))))
     (show-paren-match ((t (:foreground "black" :background "#04bbff" :bold t))))
     (show-paren-mismatch-face ((t (:bold t :foreground "red"))))
;     (paren-face ((t (:foreground "DarkGoldenrod4"))))
     (paren-face ((t (:foreground "grey40"))))
     (region ((t (:foreground "black" :background "#bbff04"))))
     (highlight ((t (:background "midnight blue"))))
     (isearch ((t (:foreground "black" :background "#04bbff"))))
     (lazy-highlight ((t (:foreground "black" :background "#726f99"))))
     (secondary-selection ((t (:background "navy"))))
     
     (comint-highlight-input ((t (:bold t :weight bold))))
     (comint-highlight-prompt ((t (:foreground "green3"))))

     (flymake-errline ((t (:underline "OrangeRed"))))
     (flymake-warnline ((t (:underline "dark goldenrod"))))
     
     (button ((t (:underline t :foreground "LightSteelBlue"))))
     (link ((t (:underline t :foreground "LightSteelBlue"))))
     (info-xref ((t (:underline t :foreground "#726f99"))))
     (info-xref-visited ((t (:underline t :foreground "DeepPink4"))))
     (w3m-anchor ((t (:underline t :foreground "LightSteelBlue"))))
     (link-visited ((t (:underline t :foreground "DeepPink4"))))
     (w3m-arrived-anchor ((t (:underline t :foreground "DeepPink4"))))
     (help-argument-name ((t (:foreground "coral3"))))

     (w3m-header-line-location-title ((t (:foreground "SpringGreen3" :background "gray20"))))
     (w3m-tab-background ((t (:background "grey20"))))
     (w3m-tab-selected-background ((t (:background "grey30"))))
     (w3m-tab-selected ((t (:background "grey90" :foreground "black" :box "grey50"))))
     (w3m-tab-selected-retrieving ((t (:background "grey90" :foreground "red3" :box "grey50"))))
     (w3m-tab-unselected ((t (:background "grey20" :foreground "grey70" :box "grey50"))))
     (w3m-tab-unselected-retrieving ((t (:background "grey20" :foreground "red3" :box "grey50"))))
     (w3m-tab-unselected-unseen ((t (:background "grey20" :foreground "blue" :box "grey50"))))
     (w3m-tab-mouse ((t (:background "grey60" :foreground "black" :box "grey50"))))
     (w3m-form ((t (:box "grey50"))))
     (w3m-form-button ((t (:background "grey20" :foreground "grey70" :box "grey50"))))
     (w3m-form-button-pressed ((t (:background "grey30" :foreground "grey70" :box "grey50"))))
     (w3m-form-button-mouse ((t (:background "grey95" :foreground "black" :box "grey50"))))

     (widget-field-face ((t (:background "navy"))))

     (widget-single-line-field-face ((t (:background "RoyalBlue"))))
     (custom-button ((t (:background "grey20" :foreground "grey70" :box "grey50"))))
     (custom-button-pressed ((t (:background "grey30" :foreground "grey70" :box "grey50"))))
     (custom-button-mouse ((t (:background "grey95" :foreground "black" :box "grey50"))))
                                     
     )))

(defun color-theme-arthur-mild ()
  "grayscale"
  (interactive)
  (let ((color-theme-is-cumulative t))
    (color-theme-arthur-dark)
    (color-theme-install
     '(color-theme-arthur-mild
       () ()
       (minibuffer-prompt ((t (:foreground "grey90"))))
       (mode-line ((t (:background "#ffffff" :foreground "black"))))
       (fringe ((t (:background "#101010"))))
       (mode-line-inactive ((t (:background "#101010" :foreground "#808080"))))

       (font-lock-doc-face ((t (:foreground "grey50"))))
       (font-lock-string-face ((t (:foreground "grey60"))))
       (font-lock-regexp-grouping-construct ((t (:foreground "grey80" :bold t))))
       (font-lock-regexp-grouping-backslash ((t (:foreground "grey40"))))
       (font-lock-keyword-face ((t (:bold t :foreground "grey60"))))
       (font-lock-constant-face ((t (:foreground "grey60"))))
       (font-lock-type-face ((t (:foreground "grey90"))))
       (font-lock-variable-name-face ((t (:italic t :foreground "grey75"))))
       (font-lock-function-name-face ((t (:weight unspecified :underline t :foreground "grey75"))))
       (font-lock-builtin-face ((t (:foreground "grey60"))))
       (font-lock-preprocessor-face ((t (:foreground "DeepPink3"))))
       (font-lock-negation-char-face ((t (:foreground "#ffffff"))))
       (font-lock-warning-face ((t (:bold t :foreground "OrangeRed3"))))
       (font-lock-comment-face ((t (:foreground "#404040" :background unspecified))))
       (font-lock-comment-delimiter-face ((t (:foreground "#606060" :background unspecified))))
       
       (font-latex-bold-face ((t (:foreground "grey75" :bold t))))
       (font-latex-warning-face ((t (:foreground "coral4"))))

       (show-paren-match-face ((t (:bold t :foreground "gray75"))))
       (help-argument-name ((t (:foreground "grey70" :bold t))))

     ))))


(defun color-theme-arthur-light ()
"Color theme by Arthur Danskin, created 2007-11-16."
  (interactive)
  (let ((color-theme-is-cumulative t))
    ;(color-theme-arthur-dark)
    (color-theme-install
     '(color-theme-arthur-light
       ((background-color . "white")
        (foreground-color . "black")
        (background-mode . light)
        (mouse-color . "black")
        (cursor-color . "black"))

       ((term-default-fg-color . "black")
        (term-default-bg-color . "white"))

       (fringe ((t (:background "#eeeeee" :foreground "#005080"))))
       (mode-line ((t (:background "#4F4390" :foreground "white"))))
       (mode-line-inactive ((t (:background "#ccccccc":foreground "black"))))
       (mode-line-highlight ((t (:background "grey20" :foreground "grey70"))))
       (minibuffer-prompt ((t (:foreground "#0060a0"))))
       (region ((t (:background "#aaaaaa"))))
       (paren-face ((t (:foreground "#508090"))))
       (font-lock-comment-face ((t (:foreground "#644454" :background unspecified))))
       (font-lock-comment-delimiter-face ((t (:foreground "#877797" :background unspecified))))
       (font-lock-doc-face ((t (:foreground "DeepPink4" :background unspecified))))
       (font-lock-string-face ((t (:foreground "#843243" :background unspecified))))
       (font-lock-keyword-face ((t (:bold nil :foreground "#203079" :background unspecified))))
;;;        (font-lock-warning-face ((t (:bold t :foreground "red"))))
       (font-lock-constant-face ((t (:foreground "#707080"))))
       (font-lock-type-face ((t (:foreground "#535385" :background unspecified))))
       (font-lock-variable-name-face ((t (:italic unspecified :foreground "#404F34"))))
       (font-lock-function-name-face ((t (:bold t :foreground "#002000" :background unspecified))))
       (font-lock-builtin-face ((t (:foreground "#6f102f"))))
       (font-lock-preprocessor-face ((t (:foreground "#111111"))))

       (tuareg-font-lock-governing-face ((t (:bold t :foreground "firebrick" :weight bold))))
       (tuareg-font-lock-operator-face ((t (:foreground "SpringGreen4"))))

       (show-paren-match ((t (:background "#00bbff" :foreground "black" :bold t))))
       (highlight ((t (:background "#FFA300"))))
       (isearch ((t (:background "#00bbff"))))
       (lazy-highlight ((t (:background "#00aaee"))))

       (button ((t (:underline t :foreground "LightSteelBlue4"))))
       (link ((t (:underline t :foreground "LightSteelBlue4"))))
       (info-xref ((t (:underline t :foreground "LightSteelBlue4"))))
       (w3m-anchor ((t (:underline t :foreground "LightSteelBlue4"))))
;;;        (secondary-selection ((t (:background "navy"))))
       (widget-field-face ((t (:background "ghost white"))))
;;;        (widget-single-line-field-face ((t (:background "RoyalBlue"))))

;;;        (comint-highlight-input ((t (:bold t :weight bold))))
;;;        (comint-highlight-prompt ((t (:foreground "chartreuse4"))))

       ))))

(provide 'arthur-theme)