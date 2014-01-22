;; This file is for autoloads and other related stuff that would
;; otherwise go in my .emacs but isn't really configuration and just
;; clogs things up. 

(eval-when-compile
  (require 'cl))

(autoload 'hexcolor-mode "hexcolor" "fontify color names in their color" t)
(autoload 'facecolor-mode "hexcolor" "fontify face names in their color" t)

(autoload 'htmlize-buffer "htmlize" nil t)
(autoload 'htmlize-region "htmlize" nil t)

(autoload 'gnugo "gnugo" "Play Go" t)
(autoload 'analog-clock "analog-clock" "display an analog clock" t)

;; (let-alias ((message format))
;;   (when (load "~/.emacs.d/ess-5.3.6/lisp/ess-site.el" t t) ; r (ESS)
;;     (ess-restore-asm-extns)
;;     (kill-buffer "*ESS*")))


(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;(load "auctex" t t)
;(load "preview-latex" t t)

;(load "dictionary-init" t t)

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'markdown-latex-mode "markdown-mode" "Major mode for editing Markdown latex files" t)

(autoload 'flymake-extra-enable "flymake-extra" "Turn on flymake" t)

(autoload 'visible-lines-mode "visible-lines" "line oriented movements" t)

(autoload 'rfc2047-decode-string "rfc2047")
(autoload 'rfc2047-decode-region "rfc2047")

(autoload 'glsl-mode "glsl-mode" "Major mode for GLSL programs" t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(autoload 'opengl-mode "opengl" "Minor mode for OpenGL programs" t)

(autoload 'hippie-help "hippie-help" "Intelligently display help for the thing at point." t)
(autoload 'hippie-goto "hippie-help" "Intelligently goto the thing at point." t)
(autoload 'hippie-eldoc "hippie-help" "Eldoc using hippie help keybindings" t)
(autoload 'hippie-help-mode "hippie-help" "keybindings for hippie help" t)

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(ignore-errors
  (require 'eye nil t)
  (add-to-list 'auto-mode-alist (cons eye-image-type-regexp 'eye-mode))
  (mapc (lambda (x) (add-to-list 'auto-mode-alist (cons (car x) 'eye-mode)))
        eye-backend-alist)
  (add-to-list 'magic-fallback-mode-alist (cons 'image-type-auto-detected-p 'eye-mode)))

(autoload 'u-mandelbrot "u-mandelbrot" "A simple fractal browser" t)
(autoload 'mandelbrot "u-mandelbrot" "A simple fractal browser" t)

(provide 'arthur-autoload)