;;; -*- lexical-binding: t; -*-
;;; u-mandelbrot.el --- A simple fractal browser.
;;
;;  Copyright (C) 2001 by Ulf Jasper
;;
;;  Emacs Lisp Archive Entry
;;  Author:     Ulf Jasper <ulf.jasper@web.de>
;;  Filename:   u-mandelbrot.el
;;  Time-stamp: "10. April 2002, 21:34:18 (ulf)"
;;  Created:    January 26 2001
;;  Keywords:   Games, 
;;  Version:    $Id: u-mandelbrot.el,v 1.5 2002/08/04 11:49:12 ulf Exp $
;;
;;
;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.
;;
;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;;; Philosophy:
;;
;;  The following ideas have been focussed during the development of this
;;  package (`u-mandelbrot.el').
;;  
;;  * It is intended as a small contribution to support the
;;    Neo-Post-Retro-Programming(TM) Movement.
;;
;;  * It shall give another demonstration of the fact that Emacs not only
;;    is The One True Editor, but also The Universal User Interface.
;;
;;  * It shall demonstrate that there are more ways to waste your time than
;;    you might have thought.
;;
;;  * It shall remind you of the good old days when your screen had a
;;    resolution of 160 x 120 pixels and everything was much better. 
;;
;;; Installation Instructions:
;; 
;;  In order to use this package, load this file and say `M-x u-mandelbrot
;;  RET'. For performance reasons you might want to consider byte-compiling
;;  this file. 
;;  If you have tried that, and you should feel like using this package
;;  ever again, you might want to place this file somewhere in your
;;  load-path and put the following in your Emacs startup file (~/.emacs)
;;
;;  (autoload 'u-mandelbrot "u-mandelbrot" "A simple fractal browser" t)
;;
;;; Usage:
;;
;;  Just call `u-mandelbrot' and follow the instructions. Please be patient
;;  -- it might take a while...
;;
;;; Commentary / Disclaimer:
;; 
;;  This is the result of weekend work. Don't expect too much. It does not
;;  make use of any fancy tricks. It just applies the $z \rightarrow z^2 +
;;  z_0$ algorithm in a brute force way.
;;
;;  The "ascii color-table" is quite poor. It should be enlarged.
;;
;;  This package has been tested on Emacs 20.7.1 and XEmacs 21.1.10.
;;  Apparently it runs faster with XEmacs... (!?)
;;
;;  (X)Emacs versions prior to these will never be supported.
;;
;;; History:
;; 
;;  1.0: Initial release
;; 
;;  Edited by Arthur Danskin, April 2008
;;   * use text properties instead of overlays - less slowdown
;;   * prompt less
;;   * display ascii and color at the same time!
;;   * u-mandelbrot major mode
;;   * more keybindings
;;
;;; Code:


(defvar u-mandelbrot-center-x 0
  "Real part of current center position.")
(defvar u-mandelbrot-center-y 0
  "Imaginary part of current center position.")
(defvar u-mandelbrot-re-offset 0
  "Real part of current topleft corner position.")
(defvar u-mandelbrot-im-offset 0
  "Imaginary part of current topleft corner position.")
(defvar u-mandelbrot-zoomfactor 15.0
  "Current zoom factor")
(defvar u-mandelbrot-numsteps 100 
  "Maximal number of iterations per pixel.")
(defvar u-mandelbrot-numcols 100
  "Number of colors for color mode.")
(defvar u-mandelbrot-line-width 0
  "Width of paint area, including linebreak")
(defvar u-mandelbrot-font-ratio 2
  "Height over width ratio for current font.")

(defvar u-mandelbrot-grayscale nil
  "Use grayscale colortable if t.")
(defvar u-mandelbrot-face-set nil
  "Face for painting pixels within the mandelbrot set.")
(defvar u-mandelbrot-face-vec nil
  "Vector containing all faces for display.")
(defvar u-mandelbrot-display-while-working nil
  "Display each line immediately it t.")
(defvar u-mandelbrot-ascii-map
  [?, ?` ?~ ?^ ?* ?x ?< ?0 ?$ ?# ?@ ?B ?%
      ?& ?X ?E ?O ?o ?> ?= ?+ ?- ?\" ?' ?.]
;;;   [?@ ?0 ?O ?o ?x ?* ?= ?+ ?~ ?- ?, ?. ]
  "Color table for ascii mode.")


;; Does somebody remember `u-color-cycle.el'? Try these defuns instead of
;; u-color-cycle-*-val.
(defun u-mandelbrot-g-val (i)
  (cond ((< i (/ 2 3.0)) (* 255 (sin (* 1.5 pi i))))
	(t           0 )))

(defun u-mandelbrot-b-val (i)
  (cond ((< i (/ 1 3.0)) 0)
	(t               (* 255 (sin (* 1.5 pi (- i 0.333)))))))

(defun u-mandelbrot-r-val (i)
  (cond ((< i (/ 1 3.0)) (* 255 (sin (* 1.5 pi (+ i 0.333)))))
	((< i (/ 2 3.0)) 0)
	(t 	         (* 255 (sin (* 1.5 pi (- i 0.666)))))))

(defun u-mandelbrot-create-faces (num)
  "Create NUM faces for u-mandelbrot."
  (let (
        (u-color "")
        (c-face (make-face 'u-mandelbrot-face-set))
        (r-val 0)
        (g-val 0)
        (b-val 0)
        (j 0))
    (set-face-background c-face "black")
    (set-face-foreground c-face "white")
    (setq u-mandelbrot-face-set c-face)
    (setq u-mandelbrot-face-vec (make-vector num nil))
    (dotimes (i num)
      (setq j (/ (* i 1.0) (1- num)))
      (if u-mandelbrot-grayscale
          (progn
            (setq j (* 255 (- 1 j)))
            (setq u-color (format "#%02x%02x%02x" j j j)))
        (setq r-val (u-mandelbrot-r-val j))
        (setq g-val (u-mandelbrot-g-val j))
        (setq b-val (u-mandelbrot-b-val j))
	(setq u-color (format "#%02x%02x%02x" r-val g-val b-val)))
      (setq c-face (intern (format "u-mandelbrot-face-%03d" i)))
      (aset u-mandelbrot-face-vec i c-face)
      (make-face c-face)
      (set-face-background c-face u-color)
      (set-face-foreground c-face "black"))))


(defun u-mandelbrot-calculate (z)
  "...
Argument Z complex number."
  (let* ((c (car z))
	 (d (car (cdr z)))
	 (max u-mandelbrot-numsteps) (i 0)
	 (abs-value 0)
	 (x 0)
	 (y 0)
	 (x2 0)
	 (y2 0)
	 (tx 0)
	 )
    (while (and (< i max) (< abs-value 1234567))
      (setq i (+ 1 i))
      (setq x2 (* x x))
      (setq y2 (* y y))
      (setq tx (- (+ x2 c) y2))
      (setq y (+ (* 2 x y) d))
      (setq x tx)
      (setq abs-value (+ (* x x) (* y y)))
      )
    i))

(defun u-mandelbrot-paint-cell (x y)
  (let* ((value (u-mandelbrot-calculate
                 (u-mandelbrot-cursor-to-complex x y t)))
	 (start (+ x (* y u-mandelbrot-line-width) 1))
	 ;;(end (+ 1 start))
	 (c-face nil)
         (c (aref u-mandelbrot-ascii-map
                  (% (/ value 3) (length u-mandelbrot-ascii-map)))
            ))
    (if (>= value u-mandelbrot-numsteps)
        (setq c-face u-mandelbrot-face-set
              c ? )
      (setq c-face (aref u-mandelbrot-face-vec
                         (% value (length u-mandelbrot-face-vec)))))
    (insert-char c 1)
    (set-text-properties start (1+ start) (list 'face c-face))
    (face-background c-face)))

(defun u-mandelbrot-cursor-to-complex (&optional x y ratio)
  (unless (and x y)
    (when (eq ?\n (char-after))
      (backward-char))
    (setq x (% (point) u-mandelbrot-line-width)
          y (/ (point) u-mandelbrot-line-width)))
  (let ((r (+ (/ x u-mandelbrot-zoomfactor) u-mandelbrot-re-offset))
        (i (+ u-mandelbrot-im-offset (/ y u-mandelbrot-zoomfactor))))
    (when ratio
      (setq i (* i u-mandelbrot-font-ratio)))
    (list r i)))

(defun u-mandelbrot-zoom (step &rest _args)
  (interactive "p")
  (setq step (* step 1.5))
  (if (< step 0)
      (setq step (/ 1 (abs step))))
  (u-mandelbrot-set-zoom (* u-mandelbrot-zoomfactor step)))

(defun u-mandelbrot-set-zoom (zoom)
  (interactive (list (read-number "Set zoom to "
                                  u-mandelbrot-zoomfactor)))
  (let* ((c (u-mandelbrot-cursor-to-complex))
	 (x (car c))
	 (y (cadr c)))
    (setq u-mandelbrot-zoomfactor (float zoom))
    (setq u-mandelbrot-center-x x)
    (setq u-mandelbrot-center-y y)
    (u-mandelbrot-paint)
    ;; (message "Zooming from %f %+fi (%g%%)" x y u-mandelbrot-zoomfactor)
    ))

(defun u-mandelbrot-zoom-out (step)
  (interactive "p")
  (u-mandelbrot-zoom (- step)))


;; (defun u-mandelbrot-show-progress (current length total)
;;   (if u-mandelbrot-display-while-working (sit-for 0))
;;   (let* ((string (make-string length ?.))
;; 	 (value (/ (* current length) total))
;; 	 (i 0))
;;     (while (<= i value)
;;       (aset string i ?#)
;;       (setq i (+ 1 i)))
;;     (message string)))

(defun u-mandelbrot-paint ()
  (interactive)
  (set-buffer "*mandelbrot*")
  (let ((height (* 2 (/ (1- (window-height)) 2)))
        (width (* 2 (/ (1- (window-width)) 2))))
    (setq u-mandelbrot-line-width (1+ width))
    (setq u-mandelbrot-re-offset
          (- u-mandelbrot-center-x
             (/ (* 0.5 width) u-mandelbrot-zoomfactor)))
    (setq u-mandelbrot-im-offset
          (- u-mandelbrot-center-y
             (/ (* 0.5 height) u-mandelbrot-zoomfactor)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dotimes-with-progress-reporter (j height) "Drawing Fractal... "
;;;         (u-mandelbrot-show-progress j width height)
        (dotimes (i width)
          (u-mandelbrot-paint-cell i j))
        (insert "\n")))
    (goto-char (+ (* (/ height 2) u-mandelbrot-line-width)
                  (/ width 2)))))


(defun u-mandelbrot ()
  "..."
  (interactive)
  (let ((m-buffer (get-buffer-create "*mandelbrot*")))
    (switch-to-buffer m-buffer))
  (u-mandelbrot-mode))

(defalias 'mandelbrot 'u-mandelbrot)

(define-derived-mode u-mandelbrot-mode fundamental-mode "Mandelbrot"
  "u-mandelbrot --- Copyright (C) 2001 by Ulf Jasper

\\{u-mandelbrot-mode-map}"
  (make-local-variable 'u-mandelbrot-line-width)
  (make-local-variable 'u-mandelbrot-zoomfactor)
  ;;(setq u-mandelbrot-zoomfactor 3.0)
  (make-local-variable 'u-mandelbrot-re-offset)
  (make-local-variable 'u-mandelbrot-im-offset)
  (make-local-variable 'u-mandelbrot-center-x)
  (setq u-mandelbrot-center-x 0.0)
  (make-local-variable 'u-mandelbrot-center-y)
  (setq u-mandelbrot-center-y 0.0)

;;;     (setq u-mandelbrot-numcols (length u-mandelbrot-ascii-map))
  (u-mandelbrot-create-faces (+ 1 u-mandelbrot-numcols))
;;;     (u-mandelbrot-fill)
  (setq buffer-undo-list t)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (u-mandelbrot-paint))


(defun u-mandelbrot-display-pos ()
  (interactive)
  (let ((z (u-mandelbrot-cursor-to-complex nil nil t)))
    (message "%f %+fi (%f%%)" (car z) (cadr z)
             u-mandelbrot-zoomfactor)))

(defun u-mandelbrot-recenter ()
  (interactive) (u-mandelbrot-set-zoom u-mandelbrot-zoomfactor))

(define-key u-mandelbrot-mode-map " " 'u-mandelbrot-display-pos)
(define-key u-mandelbrot-mode-map "q" 'kill-buffer)
(define-key u-mandelbrot-mode-map "+" 'u-mandelbrot-zoom)
(define-key u-mandelbrot-mode-map (kbd "RET") 'u-mandelbrot-zoom)
(define-key u-mandelbrot-mode-map "-" 'negative-argument)
(define-key u-mandelbrot-mode-map (kbd "DEL") 'u-mandelbrot-zoom-out)
(define-key u-mandelbrot-mode-map (kbd ".") 'u-mandelbrot-set-zoom)
(define-key u-mandelbrot-mode-map "u" 'u-mandelbrot-paint)
(define-key u-mandelbrot-mode-map [remap recenter-top-bottom]
  'u-mandelbrot-recenter)

(dotimes (i 10)
  (define-key u-mandelbrot-mode-map (number-to-string i) 'digit-argument)
  (setq i (1+ i)))

(provide 'u-mandelbrot)

;;; u-mandelbrot.el ends here
