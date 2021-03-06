;;  -*- lexical-binding: t; -*-
(defvar arthur-current-theme 'nil)

(defvar arthur-dark
  '(color-theme-arthur-dark
    ((background-color . "#1a1818")
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
    (line-number ((t (:slant unspecified :foreground "#606060" :background unspecified))))
    (mode-line ((t (:background "#aa0825" :foreground "black"))))
    (mode-line-inactive ((t (:background "#0d0715" :foreground "#555e6e"))))
    (mode-line-highlight ((t (:background "grey60" :foreground "black"))))
    (mode-line-buffer-id ((t (:foreground unspecified :background unspecified))))
    (which-func ((t (:weight bold :foreground unspecified))))
    (minibuffer-prompt ((t (:foreground "red2"))))

    (font-lock-comment-face ((t (:slant unspecified :foreground "#5b5979" :background unspecified))))
    (font-lock-comment-delimiter-face ((t (:foreground "#726f99" :background unspecified))))
    (font-lock-doc-face ((t (:foreground "DeepPink4" :background unspecified))))
    (font-lock-string-face ((t (:foreground "#80cccc" :background unspecified))))
    (font-lock-regexp-grouping-construct ((t (:foreground "PaleGreen3"))))
    (font-lock-regexp-grouping-backslash ((t (:foreground "OliveDrab3"))))
    (font-lock-keyword-face ((t (:weight bold :foreground "#e90e3a"))))
    (font-lock-constant-face ((t (:foreground "#9070c0"))))
                                        ;     (font-lock-type-face ((t (:foreground "#0F7889"))))
    (font-lock-type-face ((t (:foreground "#7f3975"))))
                                        ;     (font-lock-variable-name-face ((t (:slant unspecified :foreground "aquamarine4"))))
    (font-lock-variable-name-face ((t (:italic t :foreground "#782b49"))))
    (font-lock-function-name-face ((t (:weight bold :foreground "#aa0825"))))
    (font-lock-builtin-face ((t (:foreground "#940624"))))
    (font-lock-preprocessor-face ((t (:foreground "#d73a6c"))))
    (font-lock-negation-char-face ((t (:foreground "#aa0825"))))
    (font-lock-warning-face ((t (:weight bold :foreground "OrangeRed3"))))
    (tuareg-font-lock-governing-face ((t (:weight bold :foreground "OrangeRed" :weight bold))))
    (tuareg-font-lock-operator-face ((t (:foreground "SeaGreen"))))

    (highline-face ((t (:background "grey12"))))
    (show-paren-match ((t (:foreground "black" :background "#04bbff" :weight unspecified))))
    (show-paren-mismatch-face ((t (:weight unspecified :foreground "red"))))
                                        ;     (paren-face ((t (:foreground "DarkGoldenrod4"))))
    (paren-face ((t (:foreground "grey40"))))
    (region ((t (:foreground "black" :background "#bbff04"))))
    (highlight ((t (:background "#665"))))
    (isearch ((t (:foreground "black" :background "#04bbff"))))
    (lazy-highlight ((t (:foreground "black" :background "#726f99"))))
    (secondary-selection ((t (:background "navy"))))
    
    (comint-highlight-input ((t (:weight bold :weight bold))))
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
    ))

(defvar arthur-dark2
  '(color-theme-arthur-dark2
    ((background-color . "#202024")
     (foreground-color . "#c0c0c0")
     (background-mode . dark)
     (mouse-color . "#fefefe")
     (cursor-color . "#c0c0c0"))
    
    (fringe ((t (:background "#282828"))))
    (mode-line ((t (:background "#90cc30" :foreground "black"))))
    (mode-line-inactive ((t (:background "#404040" :foreground "#a0a0a0"))))
    (mode-line-highlight ((t (:background "grey60" :foreground "black"))))
    (mode-line-buffer-id ((t (:foreground unspecified :background unspecified))))
    (which-func ((t (:weight bold :foreground unspecified))))
    (minibuffer-prompt ((t (:weight bold :foreground "#b0e000"))))
    
    (font-lock-keyword-face ((t (:weight bold :foreground "#b0e000"))))
    (font-lock-variable-name-face ((t (:italic t :foreground "#b0b080"))))
    (font-lock-constant-face ((t (:foreground "#70bb60"))))
    (font-lock-type-face ((t (:foreground "#60bb00"))))
    (font-lock-builtin-face ((t (:foreground "#80cc70"))))
    (font-lock-preprocessor-face ((t (:foreground "#00bbff"))))
    (font-lock-negation-char-face ((t (:foreground "#f0e000"))))
    (font-lock-string-face ((t (:foreground "#80cc30" :background unspecified))))
    (font-lock-function-name-face ((t (:weight bold :foreground "#a0bb44"))))

    (font-lock-comment-face ((t (:slant unspecified :foreground "#606060" :background unspecified))))
    (font-lock-comment-delimiter-face ((t (:foreground "#808080" :background unspecified))))
    (font-lock-doc-face ((t (:foreground "#80cc00" :background unspecified))))
    ))

(defun clamp (val &optional mn mx)
  (setq mn (or mn 0))
  (setq mx (or mx 1))
  (min (max val mn) mx))

(defvar arthur-theme-hue 0.0 "color theme hue offset")
(defvar arthur-theme-saturation 1.0 "color theme saturation multiplier")
(defvar arthur-theme-value 1.0 "color theme value multiplier")
(defvar arthur-theme-contrast 1.1 "color theme contrast multiplier")
(defvar arthur-theme-invert nil "invert theme if non-nil")
(defvar arthur-theme-invert-hue nil "invert theme hue if non-nil")

(defun arthur-color-invert (val) (if arthur-theme-invert (- 1.0 val) val))
(defun arthur-color-invert-hue (val) (if arthur-theme-invert-hue (- 0.5 val) val))

(defun color-theme-adjust-1 (dat)
  (let* ((rgb         (x-color-values dat))
         (red         (/ (float (nth 0 rgb)) 65535.0)) ; Convert from 0-65535 to 0.0-1.0
         (green       (/ (float (nth 1 rgb)) 65535.0))
         (blue        (/ (float (nth 2 rgb)) 65535.0))
         ;; (hsv         (hexrgb-rgb-to-hsv red green blue))
         (hsv         (hexrgb-rgb-to-hsv (arthur-color-invert red)
                                         (arthur-color-invert green)
                                         (arthur-color-invert blue)))
         ;; (arthur-theme-hue (+ (if arthur-theme-invert 0.5 0.0) arthur-theme-hue))
         (arthur-theme-saturation (- arthur-theme-saturation (if arthur-theme-invert 0.25 0.0)))
         (arthur-theme-contrast (+ arthur-theme-contrast (if arthur-theme-invert 0.25 0.0)))
         
         (hue         (mod (+ (arthur-color-invert-hue (nth 0 hsv)) arthur-theme-hue) 1.0))
         (saturation  (clamp (* (nth 1 hsv) arthur-theme-saturation)))
         (value       (* (nth 2 hsv) arthur-theme-value))
         (value       (clamp (+ 0.5 (* (- value 0.5) arthur-theme-contrast))))
         )
    ;; (value       (arthur-color-invert (clamp (+ (nth 2 hsv) arthur-theme-value)))))
    (hexrgb-hsv-to-hex hue saturation value 2)))

(defun color-theme-adjust (dat)
  "Return color theme DAT hue-shifted by AMOUNT"
  (cond
   ((stringp dat) (color-theme-adjust-1 dat))
   ((and (memq dat '(light dark)) arthur-theme-invert) (if (eq dat 'light) 'dark 'light))
   ((consp dat) (cons (color-theme-adjust (car dat)) (color-theme-adjust (cdr dat))))
   (t dat)))

(defun color-theme-arthur-dark ()
  "Color theme by Arthur Danskin."
  (interactive)
  (setq arthur-current-theme 'dark)
  (color-theme-install (color-theme-adjust arthur-dark)))


(defun color-theme-arthur-dark2 ()
  "yellow-green / grey"
  (interactive)
  (require 'hexcolor)
  (let ((color-theme-is-cumulative t))
    (color-theme-arthur-dark)
    (setq arthur-current-theme 'dark2)
    (color-theme-install (color-theme-adjust arthur-dark2))))

(defun color-theme-arthur-mild ()
  "grayscale"
  (interactive)
  (let ((color-theme-is-cumulative t))
    (color-theme-arthur-dark)
    (setq arthur-current-theme 'mild)
    (color-theme-install
     '(color-theme-arthur-mild
       () ()
       (minibuffer-prompt ((t (:foreground "grey90"))))
       (mode-line ((t (:background "grey70" :foreground "black"))))
       (fringe ((t (:background "#101010"))))
       (mode-line-inactive ((t (:background "#101010" :foreground "#808080"))))

       (font-lock-doc-face ((t (:foreground "grey50"))))
       (font-lock-string-face ((t (:foreground "grey75"))))
       (font-lock-regexp-grouping-construct ((t (:foreground "grey80" :weight bold))))
       (font-lock-regexp-grouping-backslash ((t (:foreground "grey40"))))
       (font-lock-keyword-face ((t (:weight bold :foreground "grey85"))))
       (font-lock-constant-face ((t (:foreground "grey60"))))
       (font-lock-type-face ((t (:foreground "grey90"))))
       (font-lock-variable-name-face ((t (:italic t :foreground "grey75"))))
       (font-lock-function-name-face ((t (:weight unspecified :underline t :foreground "grey75"))))
       (font-lock-builtin-face ((t (:foreground "grey60"))))
       (font-lock-preprocessor-face ((t (:foreground "DeepPink3"))))
       (font-lock-negation-char-face ((t (:foreground "#ffffff"))))
       (font-lock-warning-face ((t (:weight bold :foreground "OrangeRed3"))))
       (font-lock-comment-face ((t (:foreground "#404040" :background unspecified))))
       (font-lock-comment-delimiter-face ((t (:foreground "#606060" :background unspecified))))
       
       (font-latex-bold-face ((t (:foreground "grey75" :weight bold))))
       (font-latex-warning-face ((t (:foreground "coral4"))))

       (help-argument-name ((t (:foreground "grey70" :weight bold))))

       ))))


(defun color-theme-arthur-light ()
  "Color theme by Arthur Danskin, created 2007-11-16."
  (interactive)
  (let ((color-theme-is-cumulative t))
                                        ;(color-theme-arthur-dark)
    (setq arthur-current-theme 'light)
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
       (mode-line-inactive ((t (:background "#bbbbbb":foreground "black"))))
       (mode-line-highlight ((t (:background "grey20" :foreground "grey70"))))
       (minibuffer-prompt ((t (:foreground "#0060a0"))))
       (region ((t (:background "#aaaaaa"))))
       (paren-face ((t (:foreground "#508090"))))
       (font-lock-comment-face ((t (:foreground "#644454" :background unspecified))))
       (font-lock-comment-delimiter-face ((t (:foreground "#877797" :background unspecified))))
       (font-lock-doc-face ((t (:foreground "DeepPink4" :background unspecified))))
       (font-lock-string-face ((t (:foreground "#843243" :background unspecified))))
       (font-lock-keyword-face ((t (:weight unspecified :foreground "#203079" :background unspecified))))
;;;        (font-lock-warning-face ((t (:weight bold :foreground "red"))))
       (font-lock-constant-face ((t (:foreground "#707080"))))
       (font-lock-type-face ((t (:foreground "#535385" :background unspecified))))
       (font-lock-variable-name-face ((t (:italic unspecified :foreground "#404F34"))))
       (font-lock-function-name-face ((t (:weight bold :foreground "#002000" :background unspecified))))
       (font-lock-builtin-face ((t (:foreground "#6f102f"))))
       (font-lock-preprocessor-face ((t (:foreground "#111111"))))
       (font-lock-negation-char-face ((t (:foreground "#ff0000"))))

       (tuareg-font-lock-governing-face ((t (:weight bold :foreground "firebrick" :weight bold))))
       (tuareg-font-lock-operator-face ((t (:foreground "SpringGreen4"))))
       
       (show-paren-match ((t (:background "#00bbff" :foreground "black" :weight unspecified))))
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

;;;        (comint-highlight-input ((t (:weight bold :weight bold))))
;;;        (comint-highlight-prompt ((t (:foreground "chartreuse4"))))

       ))))


(defun color-theme-arthur-light2 ()
  "grayscale"
  (interactive)
  (let ((color-theme-is-cumulative t))
    (color-theme-arthur-light)
    (setq arthur-current-theme 'light2)
    (color-theme-install
     '(color-theme-arthur-light2
       ((background-color . "#e0e0e0")
        (foreground-color . "#000000")
        (background-mode . light)
        (mouse-color . "black")
        (cursor-color . "black"))
       
       (fringe ((t (:background "#d0d0d0"))))
       (mode-line ((t (:background "#90cc30" :foreground "black"))))
       (mode-line-inactive ((t (:background "#404040" :foreground "#a0a0a0"))))
       (mode-line-highlight ((t (:background "grey60" :foreground "black"))))
       (mode-line-buffer-id ((t (:foreground unspecified :background unspecified))))
       (which-func ((t (:bold t :foreground unspecified))))
       (minibuffer-prompt ((t (:bold t :foreground "#708000"))))
       
       (font-lock-keyword-face ((t (:bold t :foreground "#406000"))))
       (font-lock-variable-name-face ((t (:italic t :foreground "#606030"))))
       (font-lock-constant-face ((t (:foreground "#207b10"))))
       (font-lock-type-face ((t (:foreground "#208b00"))))
       (font-lock-builtin-face ((t (:foreground "#307c20"))))
       (font-lock-preprocessor-face ((t (:foreground "#00bbff"))))
       (font-lock-negation-char-face ((t (:foreground "#cf0000"))))
       (font-lock-string-face ((t (:foreground "#408c05" :background unspecified))))
       (font-lock-function-name-face ((t (:bold t :foreground "#707b04"))))

       (font-lock-comment-face ((t (:slant unspecified :foreground "#606060" :background unspecified))))
       (font-lock-comment-delimiter-face ((t (:foreground "#808080" :background unspecified))))
       (font-lock-doc-face ((t (:foreground "#80cc00" :background unspecified))))
       ))))

(defun color-theme-arthur-light3 ()
  "grayscale"
  (interactive)
  (let ((color-theme-is-cumulative t))
    (color-theme-arthur-light)
    (setq arthur-current-theme 'light3)
    (color-theme-install
     '(color-theme-arthur-light3
       ((background-color . "#f8f8e8")
        (foreground-color . "#000000")
        (background-mode . light)
        (mouse-color . "black")
        (cursor-color . "black"))
       
       (fringe ((t (:background "#fffff8"))))
       (mode-line ((t (:background "#404040" :foreground "#a0a0a0"))))
       (mode-line-inactive ((t (:background "#fffff8" :foreground "black"))))
       (mode-line-highlight ((t (:background "grey60" :foreground "black"))))
       (mode-line-buffer-id ((t (:foreground unspecified :background unspecified))))
       (which-func ((t (:bold t :foreground unspecified))))
       (minibuffer-prompt ((t (:bold t :foreground "#708000"))))
       
       (font-lock-keyword-face ((t (:bold t :foreground "#b06000"))))
       (font-lock-variable-name-face ((t (:pitalic t :foreground "#606060"))))
       (font-lock-constant-face ((t (:foreground "#603030"))))
       (font-lock-type-face ((t (:foreground "#805000"))))
       (font-lock-builtin-face ((t (:foreground "#808030"))))
       (font-lock-preprocessor-face ((t (:foreground "#808080"))))
       (font-lock-negation-char-face ((t (:foreground "#cf0000"))))
       (font-lock-string-face ((t (:foreground "#902000" :background unspecified))))
       (font-lock-function-name-face ((t (:bold t :foreground "#404040"))))

       (font-lock-comment-face ((t (:slant unspecified :foreground "#606060" :background unspecified))))
       (font-lock-comment-delimiter-face ((t (:foreground "#808080" :background unspecified))))
       (font-lock-doc-face ((t (:foreground "#902000" :background unspecified))))
       ))))


(defun arthur-theme ()
  (interactive)
  (cond
   ((eq arthur-current-theme 'dark) (color-theme-arthur-dark))
   ((eq arthur-current-theme 'dark2) (color-theme-arthur-dark2))
   ((eq arthur-current-theme 'mild) (color-theme-arthur-mild))
   ((eq arthur-current-theme 'light) (color-theme-arthur-light))
   ((eq arthur-current-theme 'light2) (color-theme-arthur-light2))
   ((eq arthur-current-theme 'light3) (color-theme-arthur-light3))))

(defun arthur-theme-incr (sym inc)
  (interactive)
  (set sym (+ (symbol-value sym) (* inc hexcolor-increment)))
  (message "Hue: %.3f Sat: %.3f Val: %.3f Invert?:%s InvertHue?:%s"
           arthur-theme-hue arthur-theme-saturation arthur-theme-value arthur-theme-invert arthur-theme-invert-hue)
  (arthur-theme))

(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'arthur-current-theme)
  (add-to-list 'desktop-globals-to-save 'arthur-theme-hue)
  (add-to-list 'desktop-globals-to-save 'arthur-theme-saturation)
  (add-to-list 'desktop-globals-to-save 'arthur-theme-value)
  (add-to-list 'desktop-globals-to-save 'arthur-theme-invert-hue)
  (add-hook 'desktop-after-read-hook 'arthur-theme))
  
(global-set-key (kbd "<f7>") (lambda () (interactive) (setq arthur-theme-invert-hue (not arthur-theme-invert-hue)) (arthur-theme-incr 'arthur-theme-hue 0.0)))
(global-set-key (kbd "<f3>") (lambda () (interactive) (setq arthur-theme-invert (not arthur-theme-invert)) (arthur-theme-incr 'arthur-theme-hue 0.0)))
(global-set-key (kbd "<f5>") (lambda () (interactive) (arthur-theme-incr 'arthur-theme-hue -1.0)))
(global-set-key (kbd "<f6>") (lambda () (interactive) (arthur-theme-incr 'arthur-theme-hue 1.0)))
(global-set-key (kbd "<f8>") (lambda () (interactive) (arthur-theme-incr 'arthur-theme-hue (/ 0.1 hexcolor-increment))))
(global-set-key (kbd "<f9>") (lambda () (interactive) (arthur-theme-incr 'arthur-theme-saturation -1.0)))
(global-set-key (kbd "<f10>") (lambda () (interactive) (arthur-theme-incr 'arthur-theme-saturation 1.0)))
;; (global-set-key (kbd "<f9>") (lambda () (interactive) (arthur-theme-incr 'arthur-theme-value -1.0)))
;; (global-set-key (kbd "<f10>") (lambda () (interactive) (arthur-theme-incr 'arthur-theme-value 1.0)))

;; (eval-when-compile
  ;; (defvar arthur-current-theme nil)
  ;; (arthur-theme))

(provide 'arthur-theme)
