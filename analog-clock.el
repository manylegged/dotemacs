;;; analog-clock.el --- Analog clock for GNU/Emacs

;; Copyright (C) 2008 Yoni Rabkin
;;
;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;     
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;; 
;; This package renders an analog clock inside the Emacs window.
;;
;; To use it, do M-x analog-clock.

;;; Installation:
;;
;; Place it in your load path with:
;;
;; (add-to-list 'load-path "/PATH/TO/analog-clock/")
;;
;; Then add to your .emacs:
;;
;; (require 'analog-clock)

;;; History:
;; 
;; In the beggining there was TECO...

;;; Code:

(require 'artist)

(defgroup analog-clock nil
  "Display an analog clock."
  :group 'games
  :prefix "analog-clock-")

(defvar analog-clock-buffer-name "*Analog Clock*")

(defvar analog-clock-update-timer nil
  "Interval timer object.")

(defvar analog-clock-hours-factor 0.4)
(defvar analog-clock-minutes-factor 0.7)

(defvar analog-clock-phase (/ (* pi 3) 2))
(defvar analog-clock-horizontal-factor 2)

(defvar analog-clock-numerals-factor 0.8)
(defvar analog-clock-numerals-type 'western)
(defvar analog-clock-numerals-positions
  (list 0
	(/ pi 6)
	(/ pi 3)
	(/ pi 2)
	(/ (* pi 2) 3)
	(/ (* pi 5) 6)
	pi
	(/ (* pi 7) 6)
	(/ (* pi 4) 3)
	(/ (* pi 3) 2)
	(/ (* pi 5) 3)
	(/ (* pi 11) 6)
	))
(defvar analog-clock-numerals-western
  (list "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "1" "2"))
(defvar analog-clock-numerals-roman
  (list "III" "IV" "V" "VI" "VII" "VIII" "IX" "X" "XI" "XII" "I" "II"))

(defun analog-clock-draw-numerals (center-x center-y size)
  "Draw the numerals on the clock face."
  (let ((r (round (* size
		     analog-clock-numerals-factor)))
	(j center-x)
	(k center-y)
	(numerals-list (cond ((eq analog-clock-numerals-type
				  'western)
			      analog-clock-numerals-western)
			     ((eq analog-clock-numerals-type
				  'roman)
			      analog-clock-numerals-roman)))
	(numeral-index 0))
    (dolist (t0 analog-clock-numerals-positions)
      (let ((x (round (+ (* (* r analog-clock-horizontal-factor) (cos t0))
			 j)))
	    (y (round (+ (* r (sin t0))
			 k))))
	(artist-text-insert-overwrite x y (nth numeral-index numerals-list))
	(setq numeral-index
	      (1+ numeral-index))))))

(defun analog-clock-draw-minutes-hand (center-x center-y size)
  "Draw the minutes hand on the clock face."
  (let* ((minutes (+ (nth 1 (decode-time))
                     (/ (nth 0 (decode-time)) 60.0)))
	 (t0 (+ (* (* pi 2) (/ minutes 60.0))
		analog-clock-phase))
	 (r (round (* size
		      analog-clock-minutes-factor)))
	 (j center-x)
	 (k center-y))
    (artist-draw-line
     center-x
     center-y
     (round (+ (* (* r analog-clock-horizontal-factor) (cos t0))
	       j))
     (round (+ (* r (sin t0))
	       k)))))

(defun analog-clock-draw-hours-hand (center-x center-y size)
  "Draw the hours hand on the clock face."
  (let* ((minutes (nth 1 (decode-time)))
	 (hours (nth 2 (decode-time)))
	 (t0 (+ (+ (nth (mod hours 12) analog-clock-numerals-positions)
		   (* (/ pi 6) (/ minutes 60.0)))
		analog-clock-phase))
	 (r (round (* size
		      analog-clock-hours-factor)))
	 (j center-x)
	 (k center-y))
    (artist-draw-line
     center-x center-y
     (round (+ (* (* r analog-clock-horizontal-factor) (cos t0))
	       j))
     (round (+ (* r (sin t0))
	       k)))))

(defun analog-clock-draw-body (center-x center-y size)
  "Draw the body of the clock."
  (with-current-buffer analog-clock-buffer-name
    (artist-mode)
    (artist-draw-ellipse-general
     center-x center-y
     (round (* size analog-clock-horizontal-factor))
     size)))

(defun analog-clock-kill-buffer ()
  "Kill the analog clock window."
  (interactive)
  (and analog-clock-update-timer 
       (cancel-timer analog-clock-update-timer))
  (kill-this-buffer))

(define-derived-mode analog-clock-mode fundamental-mode "Analog Clock"
  "Major mode for \\<artist-mode-map>\\[Analog Clock].

\\{analog-clock-mode-map}"
  :group 'analog-clock
  (make-local-variable 'analog-clock-update-timer)
  (setq buffer-read-only t)
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  (when analog-clock-update-timer
    (cancel-timer analog-clock-update-timer))
  (setq analog-clock-update-timer
        (run-at-time nil 60 'analog-clock-update-handler))
;;;   (add-hook 'window-configuration-change-hook
;;;             'analog-clock-update-handler nil t)
  (analog-clock-update-handler))

(define-key analog-clock-mode-map "q" 'analog-clock-kill-buffer)
(define-key analog-clock-mode-map "u" 'analog-clock-update-handler)

(defun analog-clock-draw ()
  "Draw the clock."
  (let ((center-x (floor (/ (window-width) 2)))
	(center-y (floor (/ (window-height) 2)))
	(size (- (floor (/ (min (/ (window-width)
                                   analog-clock-horizontal-factor)
                                (window-height))
                           2))
                 2)))
    (analog-clock-draw-body center-x center-y size)
    (analog-clock-draw-numerals center-x center-y size)
    (analog-clock-draw-minutes-hand center-x center-y size)
    (analog-clock-draw-hours-hand center-x center-y size)))

(defun analog-clock-update-handler ()
  "Update the display."
  (interactive)
  (with-current-buffer analog-clock-buffer-name
    (with-selected-window (get-buffer-window (current-buffer))
      (let ((inhibit-read-only t))
        (let ((artist-line-char-set t)
              (artist-fill-char-set t)
              (artist-line-char artist-erase-char)
              (artist-fill-char artist-erase-char)
              (x1 0)
              (y1 0)
              (x2 (1- (window-width)))
              (y2 (1- (window-height))))
          (erase-buffer)
          (artist-fill-rect (artist-draw-rect x1 y1 x2 y2) x1 y1 x2 y2))
        (analog-clock-draw)
        ;; For some reason, artist mode needs to be turned off manually
        ;; after use.
;;;         (artist-mode -1)
        ))))

(defun analog-clock ()
  "Display an analog clock."
  (interactive)
  (get-buffer-create analog-clock-buffer-name)
  (switch-to-buffer analog-clock-buffer-name)
  (analog-clock-mode))

(provide 'analog-clock)

;;; analog-clock.el ends here
