;;; hippie-help.el - Intelligent help and navigation
;;
;;; History:
;; 
;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; Copyright 2008-2013 Arthur Danskin <arthurdanskin@gmail.com>
;; April 2008 - initial version 
;; May   2008 - ebrowse support, fix man support, cleanups
;; July  2013 - hippie-eldoc mode
;; 
;;; Commentary:
;; 
;; This package provides a unified interface for context sensitive help and navigation, analogous
;; to `hippie-expand'. `hippie-goto' is a generalized `find-tag' / `imenu' / `find-function' /
;; etc. and `hippie-help' is a generalized `describe-function' / `describe-variable' /
;; `python-describe-symbol' / etc. `hippie-eldoc' uses the machinery of `hippie-goto' to show the
;; definition of the function at point in the minibuffer.
;;
;; The implementation of this is really messy because we need to be able to tell if we should ask
;; a function for help before we actually call it. This generally requires dipping into internal
;; functions and/or overriding `completing-read' to prevent the user from being annoyed. The
;; macro `with-no-interactivity' encapsulates most of this.
;;
;;; Installation:
;; (autoload 'hippie-help "hippie-help" "Display help for the thing at point." t)
;; (autoload 'hippie-goto "hippie-help" "Goto the thing at point." t)
;; put (hippie-eldoc 1) in your favorite programming language hook. It works best with C and C++.

;; suggested key bindings:
;; (define-key global-map (kbd "M-?") 'hippie-help)
;; (define-key global-map [remap find-tag] 'hippie-goto)
;; 

(eval-when-compile
  (require 'doxymacs nil t)
  (require 'opengl nil t)
  (require 'octave-hlp2 nil t)
  (require 'woman)
  (require 'ebrowse)
  (require 'eldoc))

(require 'etags)
(require 'ffap)
(require 'imenu)

(defvar hap-debug-enabled nil "Enable internal debugging?")
(defvar hap-current-element nil)

;; There must be a better way to do this
(defvar hap-elisp-help-modes
  '(emacs-lisp-mode help-mode lisp-interaction-mode)
  "List of modes where `hippie-help' should try to use
  `describe-function' or `describe-variable'")

(defvar hap-ignore-symbol-regexp 
  (regexp-opt (list "const" "float" "double" "long" "int" "else" "return") 'symbols))

(defvar hap-max-comment-lines 4)

(defmacro with-no-interactivity (&rest body)
  "Run BODY with interactive functions overridden to not prompt user, not change windows, etc.
If BODY calls prompting functions, pick the default automatically"
  `(flet ((read-from-minibuffer (prompt init &rest args) init)
          (completing-read (&rest args) (apply 'hap-silent-completing-read args))
          (y-or-n-p (prompt) nil)
          (yes-or-no-p (prompt) nil)
          (switch-to-buffer (buf-or-name &rest args) (set-buffer buf-or-name))
          (pop-to-buffer (buffer &rest args) (set-buffer buffer))
          (select-window (win &optional norec) (set-buffer (window-buffer win)) win)
          (push-mark (&rest args) nil)
          (message (fmt &rest args) nil)
          (find-file-noselect (name &rest args) (set-buffer (find-buffer-visiting name))))
     ,@body))

(defmacro save-etags-state (&rest body)
  "Save the etags mark ring, last tag, etc so that etags
functions run as part of BODY will not change globals state"
  `(let ((tags-location-ring (make-ring 1))
         (find-tag-marker-ring (make-ring 1))
         (mark-ring (make-ring 1))
         last-tag find-tag-history)
     ,@body))

(defun hap-filter-symbol (sym)
  "Return non-nil if symbol string should be processed further"
  (if (stringp sym)
      (and (> (length sym) 2)
           (string-match-p "^[_a-zA-Z]" sym)
           (not (string-match-p hap-ignore-symbol-regexp sym))
           sym)
    sym))

(defun hap-variable-at-point-p ()
  (let ((s (variable-at-point)))
    (and (symbolp s) (memq major-mode hap-elisp-help-modes) s)))

(defun hap-function-at-point-p ()
  (and (memq major-mode hap-elisp-help-modes)
       (function-called-at-point)))

(defun hap-face-at-point-p ()
  "See `read-face-name'"
  ;; TODO get the face in use at point, too
  ;; maybe I can use flet to override completing-read-multiple
  (memq (intern-soft (thing-at-point 'symbol)) (face-list)))

(defun hap-doxygen-at-point-p ()
  (and (boundp 'doxymacs-mode) doxymacs-mode
       ;; This is such a bad hack
       (let ((code (cadr (interactive-form 'doxymacs-lookup))))
         (with-no-interactivity
          (car (eval code))))))

(defun hap-opengl-at-point-p ()
  (and (functionp 'opengl-function-at-point) (with-no-warnings (opengl-function-at-point))))

(defun hap-man-page-at-point-p ()
  ;; TODO this adds a tab to the man prompt, for some reason...
  (and (require 'woman nil t)
       (with-no-interactivity
        (let ((woman-use-topic-at-point t))
          (with-no-warnings (woman-file-name nil))))))

(defun hap-tag-at-point-p ()
  "Return non-nil if point is contained in an etags tag.
You would think that this would be a simple, built in operation,
but noooo we have to save tons of state and override built in
functions to avoid scrambling everything."
  (let ((sym (thing-at-point 'symbol))
        message-log-max                 ; inhibit messages going to log
        case-fold-search)               ; case sensitive
    (and sym tags-file-name
         (hap-filter-symbol sym)
         (ignore-errors
           (save-current-buffer
             (save-etags-state
              (catch 'hap-found-the-tag
                ;; override function which would find the file - we already know we have a tag
                (flet ((tag-find-file-of-tag-noselect (file) (throw 'hap-found-the-tag sym)))
                  (visit-tags-table-buffer)
                  (find-tag-in-order (concat "\\<" sym "\\>")
                                     find-tag-regexp-search-function
                                     ;find-tag-tag-order
                                     '(tag-symbol-match-p)
                                     find-tag-next-line-after-failure-p
                                     nil
                                     t)))))))))
    
(defun hap-octave-help2-at-point-p ()
  (and (memq major-mode '(octave-mode inferior-octave-mode))
       (require 'octave-hlp2 nil t)
       (with-no-warnings (octave-help2-at-point))))

(defun hap-python-at-point-p ()
  (and (eq major-mode 'python-mode)
       (thing-at-point 'symbol)))

(defun hap-imenu-c++-comparator (str pat)
  (when (stringp str)
    (string-match-p (concat "^\\([a-zA-Z0-9_]+::\\)?" (regexp-quote str) "$") pat)))

(defun hap-imenu-python-comparator (str pat)
  (string-match-p (concat "^ ?\\(class\\)? ?" (regexp-quote str) "$") pat))

(defvar hap-imenu-comparator-alist
  '((c++-mode . hap-imenu-c++-comparator)
    (python-mode . hap-imenu-python-comparator)))

(defun hap-imenu-at-point (&optional sym)
  "return (SYMBOL . MARKER) for SYM or the symbol at point with `imenu', else nil"
  (unless sym (setq sym (thing-at-point 'symbol)))
  (and sym
       (let ((imenu-auto-rescan t)
             (imenu-name-lookup-function
              (assoc major-mode hap-imenu-comparator-alist)))
         (when imenu-name-lookup-function
           (setq imenu-name-lookup-function
                 (cdr imenu-name-lookup-function)))
         (ignore-errors
           (imenu--in-alist sym (imenu--make-index-alist t))))))

(defun hap-imenu-read (str item)
  ;; TODO improve me
  (let ((sym (car item)))
    (list (completing-read (format "%s (default %s): " str sym)
                           (list sym) nil nil nil nil sym)
          (cdr item))))

(defun hap-imenu (&optional sym marker)
  "Interactively, prompt for Imenu symbol and go to the marker position"
  (interactive (hap-imenu-read "Imenu" (hap-imenu-at-point)))
  (unless marker
    (setq marker (cdr-safe (hap-imenu-at-point))))
  (when marker
    (goto-char marker)))

(defun hap-imenu-at-point-other-file ()
  "return (SYMBOL . MARKER) for the symbol at point using `imenu',
in the file returned by `ff-find-other-file'"
  (let ((sym (thing-at-point 'symbol))
        (buf (current-buffer)))
    (save-current-buffer
      (with-no-interactivity
       (unless (catch 'hap-ff-not-found
                 (let ((ff-not-found-hook (lambda () (throw 'hap-ff-not-found t))))
                   (ff-find-other-file))
                 nil)
         (unless (eq buf (current-buffer))
           (hap-imenu-at-point sym)))))))

(defun hap-imenu-other-file (sym marker)
  "Interactively prompt for confirmation, then go to the
definition found using `hap-imenu-at-point-other-file'"
  (interactive (hap-imenu-read "Imenu other file" (hap-imenu-at-point-other-file)))
  (unless marker
    (setq marker (cdr-safe (hap-imenu-at-point-other-file))))
  (when marker
    (switch-to-buffer (marker-buffer marker))
    (goto-char marker)))

(defun hap-url-at-point-p ()
  (and (require 'w3m nil t)
       (fboundp 'w3m-url-at-point)
       (w3m-url-at-point)))

(defun hap-ebrowse-prepare-trees ()
  (let* ((trees (ebrowse-known-class-trees-buffer-list))
         (treecount (length trees)))
    (cond ((eq treecount 0) nil)
          ((eq treecount 1) (car trees))
          (t (dolist (buf (cdr trees))
               (kill-buffer buf))
             (car trees)))))

(defun hap-ebrowse-at-point-p ()
  (and (eq major-mode 'c++-mode)
       (featurep 'ebrowse)
       (hap-ebrowse-prepare-trees)
       (let* ((header (nth 1 (ebrowse-choose-tree)))
              (members (ebrowse-member-table header))
              (name (nth 1 (ebrowse-tags-read-member+class-name))))
         (and name
              (gethash name members)
              name))))


(defun hap-semantic-at-point-p ()
  (with-no-warnings
    (when (and (featurep 'semantic) semantic-mode)
      (semantic-analyze-current-context (point)))))

(defun hap-semantic-jump ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (with-no-warnings
    (semantic-ia-fast-jump (point))))

(defun hap-gtags-at-point-p ()
  (and (boundp 'gtags-mode) gtags-mode
       (with-no-warnings (gtags-current-token))))

(defvar hippie-help-try-functions-list
  '((hap-opengl-at-point-p . describe-opengl-function)
    (hap-doxygen-at-point-p . doxymacs-lookup)
    (hap-octave-help2-at-point-p . octave-help2)
    (hap-python-at-point-p . python-describe-symbol)
    (hap-variable-at-point-p . describe-variable)
    (hap-function-at-point-p . describe-function)
    (hap-man-page-at-point-p . man)
    (hap-face-at-point-p . describe-face))
  "*Alist of functions to use for `hippie-help'. The car of each
  pair is a predicate that returns non-nil if we can use the cdr
  of that pair.")

(defvar hippie-goto-try-functions-list
  '((hap-imenu-at-point . hap-imenu)
    (hap-imenu-at-point-other-file . hap-imenu-other-file)
    (hap-semantic-at-point-p . hap-semantic-jump)
    (hap-tag-at-point-p . find-tag)
    (hap-ebrowse-at-point-p . ebrowse-tags-find-definition)
    (hap-ebrowse-at-point-p . ebrowse-tags-find-declaration)
    (hap-variable-at-point-p . find-variable)
    (hap-function-at-point-p . find-function)
    (ffap-file-at-point . find-file-at-point))
  "*Alist of functions to use for `hippie-goto'. The car of each
  pair is a predicate that returns non-nil if we can use the cdr
  of that pair.")

;;;###autoload
(defun hippie-goto ()
  "Intelligently go to the definition of the thing at point.
You can return with \\[pop-tag-mark] or C-u \\[set-mark-command].
You can customize the way this works by changing
`hippie-goto-try-functions-list'."
  (interactive)
  (push-mark)
  (ring-insert find-tag-marker-ring (point-marker))
  (hap-do-it hippie-goto-try-functions-list nil))

;;;###autoload
(defun hippie-goto-other-window (&optional arg)
  "Intelligently go to the definition of the thing at point, in another window.
You can return with \\[pop-tag-mark] or C-u \\[set-mark-command].
You can customize the way this works by changing
`hippie-goto-try-functions-list'."
  (interactive "P")
  (let (buf point)
    (save-window-excursion
      (save-excursion
        (when (with-no-interactivity (hippie-goto))
          (setq buf (current-buffer))
          (setq point (point)))))
    (when buf
      (pop-to-buffer buf t)
      (goto-char point))))

;;;###autoload
(defun hippie-help ()
  "Intelligently display help for the thing at point. You can
customize the way this works by changing
`hippie-help-try-functions-list'."
  (interactive)
  (hap-do-it hippie-help-try-functions-list nil))

(defun hap-collapse-spaces (str)
  (setq str (replace-regexp-in-string "^[ \n\t]+" "" str))
  (replace-regexp-in-string "[ \n\t]+" " " str))

(defun hap-trim-to-max-lines (str maxlines &optional terminator)
  "Return the first MAXLINES of STR.
If TERMINATOR is non-nil and string is shortened,  append to the shortened string"
  (let ((chr 0)
        (lines 0))
    (while (and (< lines maxlines) (string-match "\n" str chr))
      (setq chr (match-end 0))
      (setq lines (1+ lines)))
    (if (>= lines maxlines)
        (concat (substring str 0 (1- chr)) terminator)
      str)))

(defun hap-find-prototype (&optional elt)
  "Grab and return the function prototype and any accompanying
comments or relevant context around point. Prototype is returned
as (CONTEXT COMMENT PROTOTYPE), each strings."
  (save-excursion
    (when (save-excursion (back-to-indentation) (looking-at-p "{"))
      (forward-line -1))
    (let ((proto-is-statement t) struct-mid struct comment proto (first-struct t))
      ;; enclosing context (i.e. "struct foo {" or "enum foo {" (recursive)
      (while (setq struct-mid 
                   (save-excursion
                     (when struct-mid (goto-char struct-mid))
                     (ignore-errors (let (forward-sexp-function) (up-list -1)) (point))))
        (save-excursion
          (goto-char struct-mid)
          (let ((struct-beg (or (save-excursion 
                                  (ignore-errors (c-beginning-of-statement 1 nil t) (point)))
                                (line-beginning-position)))
                (struct-end (line-end-position)))
            (when struct-beg ;; (and struct-beg (< struct-beg beg))
              (font-lock-fontify-region struct-beg struct-end)
              (setq struct (concat (hap-collapse-spaces (buffer-substring struct-beg struct-end))
                                   (and struct (concat "...\n" struct))))
              (when (and first-struct (string-match-p "enum" struct))
                (setq proto-is-statement nil))
              (setq first-struct nil)))))
      ;; inside macro definition
      (when (eq (char-before (line-end-position)) ?\\)
        (save-excursion
          (while (eq (char-before (line-end-position)) ?\\)
            (forward-line -1))
          (forward-line 1)
          (setq struct (replace-regexp-in-string
                        "[ \n\t]+$" ""
                        (buffer-substring (line-beginning-position) (- (line-end-position) 2)))))
        (setq proto-is-statement nil))
      (let* ((proto-beg (save-excursion
                          (back-to-indentation)
                          (forward-char 1)
                          (or (and proto-is-statement
                                   (ignore-errors (c-beginning-of-statement 1 nil t) (point)))
                              (line-beginning-position))))
             (beg (save-excursion (goto-char proto-beg)
                                  (forward-comment (- (buffer-size)))
                                  (end-of-line)
                                  (point)))
             (end (max (or (and proto-is-statement
                                (save-excursion (re-search-forward "[{};#/\\]" (point-max) t)))
                           0)
                       (line-end-position))))
        ;; make sure extracted substrings are fontified
        (save-excursion
          (font-lock-fontify-region beg end))
        ;; comment preceding the prototype (i.e. "// foo the thing")
        (when (> proto-beg beg)
          (setq comment (replace-regexp-in-string "^\\([/* \t]*\n\\)?[ \n\t]+" "" 
                                                  (buffer-substring beg proto-beg))))
        ;; proto is the function prototype itself (i.e "void foo(bar, bat)")
        (setq proto (hap-collapse-spaces (buffer-substring proto-beg end)))

        (list (or (when hap-debug-enabled (format "%s/%s: " elt (buffer-name (current-buffer))))
                  (concat struct (and struct "...\n")))
              (or (and comment (hap-trim-to-max-lines comment hap-max-comment-lines "...\n")) "")
              proto)))))

(defun hap-silent-completing-read (prompt collect &optional pred req init hist def)
  "Same arguments and return as `completing-read', but just pick
the default and don't actually prompt user"
  (setq unread-command-events nil)      ; ebrowse inserts a '?' with this!
  (or (and init (try-completion init collect pred) init)
      def
      (car-safe (all-completions "" collect pred))
      ""))

(defun hippie-eldoc-function ()
  "`hippie-eldoc' function for `eldoc-documentation-function'.
return a string representing the prototype for the function under point"
  (unless (or ac-completing                 ; suppress while autocomplete is enabled
              (minibuffer-selected-window)) ; suppress while minibuffer is in use
    ;; try to move out of an argument list onto the function name
    (let ((start (point))
          (search-start (or (save-excursion (re-search-backward "[;{}#]" (point-min) t))
                            (line-beginning-position)))
          forward-sexp-function                     ; work around bug in up-list
          ebrowse-position-stack                    ; save ebrowse stack
          prototypes (scanok t))
      (save-excursion 
        (save-etags-state
         (while (and (not prototypes) scanok (> (point) search-start))
           ;; return the longest prototype, by string length
           (setq prototypes (hap-do-it hippie-goto-try-functions-list 'hap-find-prototype))
           (unless prototypes
             (setq scanok (ignore-errors (up-list -1) (backward-char 1) t))))))
      (if hap-debug-enabled
          (format "%s" prototypes)
        ;; display the prototype with the longest comment
        (let ((best-proto (car-safe (sort prototypes (lambda (a b) (> (length (nth 1 a)) (length (nth 1 b))))))))
          (when best-proto
            (apply 'concat best-proto)))
        ))))

;;;###autoload
(defun hippie-eldoc (&optional arg)
  "Enable `eldoc-mode' using the same mechanism as `hippie-goto' to find function prototypes.
Particularly useful for c/c++, where it can use ebrowse, imenu, and or tag data"
  (interactive)
  (with-no-warnings
    (make-variable-buffer-local 'eldoc-documentation-function))
  (if (or (and (not eldoc-mode) (null arg))
          (and (numberp arg) (> arg 0)))
      (progn
        (setq eldoc-documentation-function 'hippie-eldoc-function)
        (turn-on-eldoc-mode))
    (eldoc-mode -1)))

(defun hap-get-all-buffers-point ()
  "Save current buffer and point for each buffer, for future
restore with `hap-set-all-buffers-point'"
  (let (lst)
    (save-current-buffer
      (dolist (buf (buffer-list))
        (unless (or (string-match-p "^ " (buffer-name buf))
                    (string-match-p "TAGS" (buffer-name buf))
                    (string-match-p "^\\*" (buffer-name buf)))
          (set-buffer buf)
          (setq lst (cons (point-marker) lst)))))
    (cons (current-buffer) lst)))

(defun hap-set-all-buffers-point (config)
  "Restore current buffer and point for buffers previously saved
with `hap-get-all-buffers-point'"
  (let ((curbuf (car config))
        (markers (cdr config)))
    (dolist (marker markers)
      (set-buffer (marker-buffer marker))
      (goto-char marker))
    (set-buffer curbuf)))

(defun hap-do-it (list mapfun)
  "If you don't like the first option you get, you can
\\[keyboard-quit] to try more options. This works by evaluating
the first function in each pair of LIST. If it returns non-nil,
then it calls the second function."
  (catch 'done
    (let ((window-config (current-window-configuration))
          (bufpoint (hap-get-all-buffers-point))
          quit? errorMsg mapresult)
      (dolist (el list)
        (setq errorMsg nil)
        (when hap-debug-enabled
          (setq mapresult (cons (list "starting-from" (point-marker)) mapresult)))
        (when (hap-filter-symbol (ignore-errors (funcall (car el))))
          (setq hap-current-element el)
          (condition-case e
              (if mapfun
                  (with-no-interactivity
                   (call-interactively (cdr el)))
                (call-interactively (cdr el)))
            (quit (setq quit? t))             ; keyboard quit
            (error (setq errorMsg (nth 1 e))  ; error / exception
                   (hap-set-all-buffers-point bufpoint)
                   (set-window-configuration window-config)))
          (when hap-debug-enabled
            (when errorMsg
              (setq mapresult (cons (list "error" el errorMsg) mapresult)))
            (when quit?
              (setq mapresult (cons (list "quit" el) mapresult))))
          (unless (or quit? errorMsg)
            (if mapfun
                (let ((res (funcall mapfun el)))
                  (when res
                    (setq mapresult (cons res mapresult)))
                  (hap-set-all-buffers-point bufpoint))
              (throw 'done t)))))
      (unless mapfun
        (cond (quit? (message "No more help"))
              (errorMsg (message "An error occured: %s" errorMsg))
              (t (message "No help, sorry"))))
      mapresult)))

(defvar hippie-help-mode-map
  (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-?") 'hippie-help)
        (define-key map (kbd "C-M-?") 'hippie-help)
        (define-key map [remap find-tag] 'hippie-goto)                           ; M-.
        (define-key map [remap find-tag-other-window] 'hippie-goto-other-window) ; C-x 4 .
        (define-key map (kbd "C-M-.") 'hippie-goto-other-window)
        map))

;;;###autoload
(define-minor-mode hippie-help-mode
  "Toggle keybindings for hippie help
\\{hippie-help-mode-map}"
  :keymap hippie-help-mode-map
  :global t
  )

(provide 'hippie-help)
