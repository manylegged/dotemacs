
(require 'hexrgb)

(defvar hexcolor-increment (/ 2.0 255.0))

(defvar hexcolor-mode-map
  (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<S-up>") 'hexcolor-incr-value-at-point)
        (define-key map (kbd "<S-down>") 'hexcolor-decr-value-at-point)
        (define-key map (kbd "<S-left>") 'hexcolor-decr-saturation-at-point)
        (define-key map (kbd "<S-right>") 'hexcolor-incr-saturation-at-point)
        (define-key map (kbd "<C-S-left>") 'hexcolor-decr-hue-at-point)
        (define-key map (kbd "<C-S-right>") 'hexcolor-incr-hue-at-point)
        map))

(defvar hexcolor-keywords
  '(("\\(0x\\|[#]\\)[0-9a-fA-F]\\{2\\}?\\([0-9a-fA-F]\\{6\\}\\)[uL]?\\>" 
     (0 (hexcolor-fontify) prepend)
     ))
  "keywords passed to `font-lock-add-keywords' for `hexcolor-mode'")

(defun forward-hexcolor (&optional arg)
  "Called from (`thing-at-point' 'hexcolor)"
  (interactive)
  (setq arg (or arg 1))
  (when (> arg 0)
    (skip-syntax-forward " "))
  (while (string-match-p "[#xX0-9A-Fa-f]" 
                         (char-to-string (if (< arg 0) (char-before) (char-after))))
    (forward-char arg))
  (point))

(defun hexcolor-incr-at-point (incr-func &optional arg)
  "Increment color at point using INCR-FUNC to handle actual color modification"
  (setq arg (or arg 1))
  (let* ((color (thing-at-point 'hexcolor))
         (header (and (string-match "^0[xX]" color) (match-string 0 color))))
    (when color
      (when header
        (setq color (replace-regexp-in-string header "#" color)))
      (setq color (funcall incr-func color (* (float arg) hexcolor-increment) 2))
      (when header
        (setq color (concat header (substring color 1))))
      (let ((pos (point))
            (beg (forward-hexcolor -1))
            (end (forward-hexcolor 1)))
        (goto-char beg)
        (kill-region beg end)
        (insert color)
        (font-lock-fontify-region beg end)
        (goto-char pos)))))

(defun hexcolor-incr-saturation-at-point (&optional arg) (interactive) (hexcolor-incr-at-point 'hexrgb-increment-saturation arg))
(defun hexcolor-incr-hue-at-point (&optional arg) (interactive) (hexcolor-incr-at-point 'hexrgb-increment-hue arg))
(defun hexcolor-incr-value-at-point (&optional arg) (interactive) (hexcolor-incr-at-point 'hexrgb-increment-value arg))
(defun hexcolor-decr-saturation-at-point () (interactive) (hexcolor-incr-at-point 'hexrgb-increment-saturation -1))
(defun hexcolor-decr-hue-at-point () (interactive) (hexcolor-incr-at-point 'hexrgb-increment-hue -1))
(defun hexcolor-decr-value-at-point () (interactive) (hexcolor-incr-at-point 'hexrgb-increment-value -1))
  
(defun hexcolor-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (color-values color))
         (r (car values))
         (g (cadr values))
         (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

(defun hexcolor-fgcolor (color)
  (if (< (hexcolor-luminance color) 128)
      "white" "black"))

(defun hexcolor-fontify ()
  (let ((color (concat "#" (match-string-no-properties 2))))
    `(face (:foreground ,(hexcolor-fgcolor color) :background ,color)))
  )

(define-minor-mode hexcolor-mode
  "Toggle Hexcolor mode.
With arg, turn hexcolor-mode on if arg is positive, off otherwise.

Hexcolor mode highlights hexidecimal and X color names in the
color they specify."
  ;;:lighter " 0xRGB"
  :lighter ""
  :group 'hexcolor
  :keymap hexcolor-mode-map
  (let ((keywords hexcolor-keywords))
    (if hexcolor-mode
        (font-lock-add-keywords nil keywords)
      (font-lock-remove-keywords nil keywords))
    (when (called-interactively-p 'any)
      (font-lock-flush))))


(defvar facecolor-keywords
  `((,(regexp-opt (mapcar 'symbol-name (face-list)) 'words)
     (0 (intern (match-string 0))))))

(define-minor-mode facecolor-mode
  "Toggle Facecolor mode.
With arg, turn facecolor-mode on if arg is positive, off otherwise.

Facecolor mode highlights face names with the faces they specify"
  :group 'hexcolor
  (if facecolor-mode
      (font-lock-add-keywords nil facecolor-keywords)
    (font-lock-remove-keywords nil facecolor-keywords))
  (when (called-interactively-p 'any)
    (font-lock-flush)))


(provide 'hexcolor)
