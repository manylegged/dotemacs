
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

(defvar hexcolor-keywords
  '(("\\(0x\\|[#]\\)[0-9a-fA-F]\\{2\\}?\\([0-9a-fA-F]\\{6\\}\\)\\>" 
     (0 (hexcolor-fontify) prepend)
     ))
  "keywords passed to `font-lock-add-keywords' for `hexcolor-mode'")


(define-minor-mode hexcolor-mode
  "Toggle Hexcolor mode.
With arg, turn hexcolor-mode on if arg is positive, off otherwise.

Hexcolor mode highlights hexidecimal and X color names in the
color they specify."
  ;:lighter " 0xRGB"
  :lighter ""
  :group 'hexcolor
  (let ((keywords hexcolor-keywords))
    (if hexcolor-mode
      (font-lock-add-keywords nil keywords)
      (font-lock-remove-keywords nil keywords))
    (ignore-errors (font-lock-fontify-buffer))))


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
  (font-lock-fontify-buffer))


(provide 'hexcolor)
