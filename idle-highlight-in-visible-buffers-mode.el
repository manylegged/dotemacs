;;; -*- lexical-binding: t; -*-
;;; idle-highlight-in-visible-buffers-mode.el --- highlight the word the point is on

;; Copyright (C) 2018  Ignacy Moryc

;; Author: Ignacy Moryc
;; URL: https://github.com/ignacy/idle-highlight-in-visible-buffers
;; Package-Version: 20181027.1531
;; Version: 0.0.1
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Based on idle-highlight-mode but works on all visible buffers

;;; Usage:

;; (add-hook 'prog-mode-hook 'idle-highlight-in-visible-buffers)

;;; Code:

(require 'thingatpt)

(defgroup idle-highlight-in-visible-buffers nil
  "Highlight other occurrences in all visible buffers of the word at point."
  :group 'faces)

(defface idle-highlight-in-visible-buffers
  '((t (:inherit highlight)))
  "Face used to highlight other occurrences of the word at point."
  :group 'idle-highlight-in-visible-buffers)

(defcustom idle-highlight-in-visible-buffers-exceptions '("def" "end")
  "List of words to be excepted from highlighting."
  :group 'idle-highlight-in-visible-buffers
  :type '(repeat string))

(defcustom idle-highlight-in-visible-buffers-idle-time 0.5
  "Time after which to highlight the word at point."
  :group 'idle-highlight-in-visible-buffers
  :type 'float)

(defvar idle-highlight-in-visible-buffers-regexp nil
  "Buffer-local regexp to be idle-highlighted.")
(make-variable-buffer-local 'idle-highlight-in-visible-buffers-regexp)

(defvar idle-highlight-in-visible-buffers-global-timer nil
  "Timer to trigger highlighting.")

(defun idle-highlight-in-visible-buffers-buffers-list ()
  "Given a list of buffers, return buffers which are currently visible."
  (let ((buffers '()))
    (walk-windows (lambda (w) (push (window-buffer w) buffers))) buffers))

(defun idle-highlight-in-visible-buffers-unhighlight-word ()
  "Remove highlighting from all visible buffers."
  (save-window-excursion
    (dolist (buffer (idle-highlight-in-visible-buffers-buffers-list))
      (switch-to-buffer buffer)
      (when idle-highlight-in-visible-buffers-regexp
        (unhighlight-regexp idle-highlight-in-visible-buffers-regexp)
        (setq idle-highlight-in-visible-buffers-regexp nil)))))

(defun idle-highlight-in-visible-buffers-highlight-word-at-point ()
  "Highlight the word under the point in all visible buffers."
  (let ((target (thing-at-point 'symbol)))
    (if (and target
             (not (member target idle-highlight-in-visible-buffers-exceptions))
             (not (string-match-p "[0-9].*" target)))
        (let ((regexp (concat "\\_<" (regexp-quote target) "\\_>")))
          (save-window-excursion
            (dolist (buffer (idle-highlight-in-visible-buffers-buffers-list))
              (switch-to-buffer buffer)
              (unless (string= regexp idle-highlight-in-visible-buffers-regexp)
                (when idle-highlight-in-visible-buffers-regexp
                  (unhighlight-regexp idle-highlight-in-visible-buffers-regexp))
                (setq idle-highlight-in-visible-buffers-regexp regexp)
                (highlight-regexp regexp 'idle-highlight-in-visible-buffers)))))
      (idle-highlight-in-visible-buffers-unhighlight-word))))

;;;###autoload
(define-minor-mode idle-highlight-in-visible-buffers-mode
  "Idle-Highlight-In-Visible-Buffers Minor Mode"
  :group 'idle-highlight-in-visible-buffers
  :global t
  (if idle-highlight-in-visible-buffers-mode
      (progn (unless idle-highlight-in-visible-buffers-global-timer
               (setq idle-highlight-in-visible-buffers-global-timer
                     (run-with-idle-timer idle-highlight-in-visible-buffers-idle-time
                                          :repeat 'idle-highlight-in-visible-buffers-highlight-word-at-point)))
             (setq idle-highlight-in-visible-buffers-regexp nil))
    (cancel-timer idle-highlight-in-visible-buffers-global-timer)
    (setq idle-highlight-in-visible-buffers-global-timer nil)
    (idle-highlight-in-visible-buffers-unhighlight-word)))

(provide 'idle-highlight-in-visible-buffers-mode)
;;; idle-highlight-in-visible-buffers-mode.el ends here
