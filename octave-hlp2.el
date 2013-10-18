;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; Arthur Danskin <arthurdanskin@gmail.com>, April 2008
;;
;; Improved help at point for octave.
;; works by querying an inferior octave program.
;; You can even get help for your own functions and things like "+" and "("
;; 
;; To use, add something like this to your init file:
;; (add-hook 'octave-mode-hook
;;            (lambda () (local-set-key (kbd "C-c h") 'octave-help2)))


;; TODO this should probably be generated automatically
(defvar octave-help2-oplist
  '("!" "#" "&&" ")" "+" "-" ".*" ".^" ";" "<>" ">" "\\" "|" "~="
    "!=" "%" "'" "*" "++" "--" ".**" "/" "<" "=" ">=" "]" "||"
    "&" "(" "**" "," ".'" "./" ":" "<=" "==" "[" "^" "~"))

(defvar octave-help2-table nil
  "hash table for completion of all the stuff octave can help us with")

(defun octave-help2-init-comp (&optional force)
  "Build a list of octave help topics by reading what octave says
when you thpe 'help'"
  (if (and (null force) octave-help2-table)
      octave-help2-table
    (run-octave t)
    (let* ((groups (comint-redirect-results-list-from-process 
                    inferior-octave-process "help" ":
\\([^*]+\\)\*\*\*" 1))
           (lst (apply 'nconc (mapcar 'split-string groups)))
           (lst-nom (mapcar (lambda (f) (substring
                                f 0 (string-match-p "\\.m$" f)))
                            lst)))
      (nconc lst-nom octave-help2-oplist)
      (setq octave-help2-table (make-hash-table :test 'equal))
      (mapc (lambda (e) (puthash e t octave-help2-table)) lst-nom))))

(defun octave-help2-at-point ()
  "Return the octave symbol that we have help for at point, or
nil if there isn't one."
  (octave-help2-init-comp)
  (let* ((sym (thing-at-point 'symbol))
         (op (buffer-substring (point) (1+ (point))))
         (op1 (buffer-substring (1- (point)) (1+ (point))))
         (op2 (buffer-substring (point) (+ 2 (point))))
         (tries (list sym op op1 op2))
         try return)
    (while tries
      (setq try (car tries)
            tries (cdr tries))
      (if (gethash try octave-help2-table)
          (setq tries nil
                return try)))
    return))

(defun octave-help2 (key)
  "Get help on Octave symbols from the inferior octave process."
  (interactive
   (list
    (let* ((sym (octave-help2-at-point))
           (prompt (if sym
                       (format "Describe Octave symbol (default %s): " sym)
                     "Describe Octave symbol: ")))
      (completing-read prompt octave-help2-table nil t nil nil sym))))
  (let ((temp-buffer-show-hook		; avoid xref stuff
	 (lambda ()
	   (toggle-read-only 1)
	   (setq view-return-to-alist
		 (list (cons (selected-window) help-return-method))))))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (set (make-local-variable 'comint-redirect-subvert-readonly) t)
        (run-octave t)
        (if (equal key "\\")
            (setq key "\\\\"))
        (comint-redirect-send-command-to-process
         (concat "help \"" key "\"") (help-buffer)
         inferior-octave-buffer nil nil)
      ;; (run-at-time 1 nil (lambda () (with-help-window (help-buffer)
;;                                (resize-temp-buffer-window)
;;                                (message "resize!"))))
        )))
;;;       (resize-temp-buffer-window)))
;;;   (with-output-to-temp-buffer (help-buffer)
  
    ;; (print-help-return-message)
    )

(provide 'octave-hlp2)
