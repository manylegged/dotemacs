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

;; There must be a better way to do this
(defvar hap-elisp-help-modes
  '(emacs-lisp-mode help-mode lisp-interaction-mode)
  "List of modes where `hippie-help' should try to use
  `describe-function' or `describe-variable'")

(defvar hap-ignore-symbol-regexp 
  (regexp-opt (list "const" "float" "double" "long" "int" "else" "return") 'symbols))

(defvar hap-max-comment-lines 4)

(defvar hap-eldoc-current-prototypes nil)
(defvar hap-eldoc-prototype-index 0)
(defvar hap-eldoc-last-symbol nil)
(defvar hap-eldoc-in-progress nil)

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

(defun hap-variable-at-point ()
  (let ((s (variable-at-point)))
    (and (symbolp s) (memq major-mode hap-elisp-help-modes) s)))

(defun hap-function-at-point ()
  (and (memq major-mode hap-elisp-help-modes)
       (function-called-at-point)))

(defun hap-face-at-point ()
  "See `read-face-name'"
  ;; TODO get the face in use at point, too
  ;; maybe I can use flet to override completing-read-multiple
  (memq (intern-soft (thing-at-point 'symbol)) (face-list)))

(defun hap-doxygen-at-point ()
  (and (boundp 'doxymacs-mode) doxymacs-mode
       ;; This is such a bad hack
       (let ((code (cadr (interactive-form 'doxymacs-lookup))))
         (with-no-interactivity
          (car (eval code))))))

(defun hap-opengl-at-point ()
  (and (functionp 'opengl-function-at-point) (with-no-warnings (opengl-function-at-point))))

(defun hap-man-page-at-point ()
  ;; TODO this adds a tab to the man prompt, for some reason...
  (and (require 'woman nil t)
       (with-no-interactivity
        (let ((woman-use-topic-at-point t))
          (with-no-warnings (woman-file-name nil))))))

(defun hap-tag-at-point ()
  "Return non-nil if point is contained in an etags tag."
  (if hap-eldoc-in-progress
      (hap-search-tags)
    (let ((sym (hap-filter-symbol (thing-at-point 'symbol)))
          message-log-max                 ; inhibit messages going to log
          case-fold-search)               ; case sensitive
      (and sym tags-file-name
           (save-current-buffer
             (visit-tags-table-buffer)
             (save-excursion
               (goto-char (point-min))
               (search-forward (concat "" sym "") (point-max) t)))
           sym))))

(defun hap-update-prototype-pos (prototype buffer pos)
  "Return updated position"
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (save-match-data
        (or (and (search-forward prototype (point-max) t)
                 (match-beginning 0))
            (and (goto-char pos)
                 (search-backward prototype (point-min) t)
                 (match-beginning 0)))))))

(defun hap-search-tags ()
  "Search tags file to find definitions for symbol at point.
Only return definitions from files that are not in a buffer.
Return in same format as `hap-find-prototype'."
  (let ((sym (hap-filter-symbol (thing-at-point 'symbol)))
        message-log-max                 ; inhibit messages going to log
        case-fold-search)               ; case sensitive
    (and sym tags-file-name
         (save-current-buffer
           (visit-tags-table-buffer)
           (let (tags)
             (save-excursion
               (goto-char (point-min))
               (while (search-forward (concat "" sym "") (point-max) t)
                 (let* ((prototype (buffer-substring (line-beginning-position) (match-beginning 0)))
                        (pos (and (re-search-forward "\\([0-9]*\\),\\([0-9]*\\)" (line-end-position) t)
                                  (string-to-number (match-string 2))))
                        (line (and pos (string-to-number (match-string 1))))
                        (filename (abbreviate-file-name
                                   (file-truename
                                    (save-excursion
                                      (re-search-backward "\n\\([^,]+\\)," (point-min) t)
                                      (match-string 1)))))
                        (entry (list (cons filename pos) "" ""
                                     (hap-collapse-spaces prototype)
                                     (cons prototype line))))
                   (setq tags (cons entry tags)))))
             tags)))))

(defun hap-find-tag ()
  (interactive)
  (let ((tags-case-fold-search nil))
    (call-interactively 'find-tag)))

(defun hap-octave-help2-at-point ()
  (and (memq major-mode '(octave-mode inferior-octave-mode))
       (require 'octave-hlp2 nil t)
       (with-no-warnings (octave-help2-at-point))))

(defun hap-python-at-point ()
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
         (let ((val (ignore-errors (imenu--in-alist sym (imenu--make-index-alist t)))))
           (if (and hap-eldoc-in-progress (markerp (cdr-safe val)))
               (hap-find-prototype nil (marker-buffer (cdr val)) (marker-position (cdr val)))
             (car val))))))

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

(defun hap-url-at-point ()
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

(defun hap-ebrowse-at-point ()
  (and (eq major-mode 'c++-mode)
       (featurep 'ebrowse)
       (hap-ebrowse-prepare-trees)
       (let* ((header (nth 1 (ebrowse-choose-tree)))
              (members (ebrowse-member-table header))
              (name (nth 1 (ebrowse-tags-read-member+class-name))))
         (and name
              (gethash name members)
              name))))


(defun hap-semantic-at-point ()
  (with-no-warnings
    (when (and (featurep 'semantic) semantic-mode)
      (semantic-analyze-current-context (point)))))

(defun hap-semantic-jump ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (with-no-warnings
    (semantic-ia-fast-jump (point))))

(defun hap-gtags-at-point ()
  (and (boundp 'gtags-mode) gtags-mode
       (with-no-warnings (gtags-current-token))))

(defvar hippie-help-try-functions-list
  '((hap-opengl-at-point . describe-opengl-function)
    (hap-doxygen-at-point . doxymacs-lookup)
    (hap-octave-help2-at-point . octave-help2)
    (hap-python-at-point . python-describe-symbol)
    (hap-variable-at-point . describe-variable)
    (hap-function-at-point . describe-function)
    (hap-man-page-at-point . man)
    (hap-face-at-point . describe-face))
  "*Alist of functions to use for `hippie-help'. The car of each
  pair is a predicate that returns non-nil if we can use the cdr
  of that pair.")

(defvar hippie-goto-try-functions-list
  '((hap-imenu-at-point . hap-imenu)
    (hap-imenu-at-point-other-file . hap-imenu-other-file)
    (hap-semantic-at-point . hap-semantic-jump)
    (hap-tag-at-point . hap-find-tag)
    (hap-ebrowse-at-point . ebrowse-tags-find-definition)
    (hap-ebrowse-at-point . ebrowse-tags-find-declaration)
    (hap-variable-at-point . find-variable)
    (hap-function-at-point . find-function)
    (ffap-file-at-point . find-file-at-point)
    )
  "*Alist of functions to use for `hippie-goto'. The car of each
  pair is a predicate that returns non-nil if we can use the cdr
  of that pair.")

(defsubst hap-marker-buffer (loc open)
  (if (markerp loc) (marker-buffer loc) (funcall open (car loc))))

(defsubst hap-marker-position (loc)
  (if (markerp loc) (marker-position loc) (cdr loc)))
  
(defun hap-switch-to-entry (entry)
  (let* ((loc (car entry))
         (buffer (hap-marker-buffer loc 'find-file))
         (pos (hap-marker-buffer loc)))
    (switch-to-buffer buffer)
    (goto-char pos)))

(defun hap-pop-to-entry (entry)
  (let* ((loc (car entry))
         (buffer (hap-marker-buffer loc 'find-file))
         (pos (hap-marker-buffer loc)))
    (pop-to-buffer buffer)
    (goto-char pos)))

;;;###autoload
(defun hippie-goto ()
  "Intelligently go to the definition of the thing at point.
You can return with \\[pop-tag-mark] or C-u \\[set-mark-command].
You can customize the way this works by changing
`hippie-goto-try-functions-list'."
  (interactive)
  (push-mark nil t)
  (ring-insert find-tag-marker-ring (point-marker))
  (if (and (string-equal (thing-at-point 'symbol) hap-eldoc-last-symbol)
           hap-eldoc-current-prototypes)
      (let* ((count (length hap-eldoc-current-prototypes))
             (index (mod hap-eldoc-prototype-index count))
             (entry (nth index hap-eldoc-current-prototypes)))
        (hap-switch-to-entry entry)
        (setq hap-eldoc-current-prototypes nil))
    (hap-do-it hippie-goto-try-functions-list nil)))

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

(defun hap-find-prototype (&optional elt &optional buffer &optional pos)
  "Grab and return the function prototype and any accompanying
comments or relevant context around point. Prototype is returned
as (MARKER CONTEXT COMMENT PROTOTYPE UNIQUE)
MARKER is either a marker or (FILENAME . POSITION)
UNIQUE is (PROTO-LINE . LINE-NUMBER)
The rest are strings"
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (when pos
        (goto-char pos))
      (when (save-excursion (back-to-indentation) (looking-at-p "{"))
        (forward-line -1))
      (let ((start (point-marker))
            (start-line (line-number-at-pos))
            (line-str (buffer-substring (line-beginning-position) (line-end-position)))
            (proto-is-statement t) struct-mid struct comment proto (first-struct t))
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
        ;; inside macro definition, where each line of macro defines a symbol
        (let ((count 0))
          (save-excursion
            (while (eq (char-before (line-end-position)) ?\\)
              (setq count (1+ count))
              (forward-line -1))
            (when (> count 1)             ; skip normal multi-line macros
              (forward-line 1)
              (setq struct (replace-regexp-in-string
                            "[ \n\t]+$" ""
                            (buffer-substring (line-beginning-position) (- (line-end-position) 2))))
              (setq proto-is-statement nil))))
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

          (list start
                (or (concat struct (and struct "...\n")))
                (or (and comment (hap-trim-to-max-lines comment hap-max-comment-lines "...\n")) "")
                proto
                (cons line-str start-line)
                (if (consp elt) (symbol-name (car elt)) ""))
          )))))

(defun hap-silent-completing-read (prompt collect &optional pred req init hist def)
  "Same arguments and return as `completing-read', but just pick
the default and don't actually prompt user"
  (setq unread-command-events nil)      ; ebrowse inserts a '?' with this!
  (or (and init (try-completion init collect pred) init)
      def
      (car-safe (all-completions "" collect pred))
      ""))

(defun hap-update-entry (entry)
  (let ((buffer (and (consp (car entry))
                     (find-buffer-visiting (caar entry)))))
    (if buffer
        (let* ((prototype (car (nth 4 entry)))
               (pos (hap-update-prototype-pos prototype buffer (cdar entry)))
               (nentry (hap-find-prototype nil buffer pos)))
          (if hap-debug-enabled (append nentry entry)
            nentry))
      entry)))

(defun hap-get-context (entry)
  (let ((ctx (nth 1 entry))
        (loc (car entry)))
    (if (> (length ctx) 0)
        ctx
      (concat (hap-pretty-marker entry) "\n"))))

(defun hap-get-current-entry ()
  (when hap-eldoc-current-prototypes
    (let* ((count (length hap-eldoc-current-prototypes))
           (index (mod hap-eldoc-prototype-index count))
           (entry (hap-update-entry (nth index hap-eldoc-current-prototypes))))
      entry)))

(defun hap-get-current-message ()
  (when hap-eldoc-current-prototypes
    (let* ((count (length hap-eldoc-current-prototypes))
           (index (mod hap-eldoc-prototype-index count))
           (entry (hap-update-entry (nth index hap-eldoc-current-prototypes)))
           (prefix (and (> count 1) (format "(%d/%d) " (1+ index) count))))
      (concat prefix (hap-get-context entry) (nth 2 entry) (nth 3 entry)))))

(define-button-type 'hippie-eldoc-view
  :supertype 'help-xref
  'help-function 'hap-pop-to-entry
  'help-echo (purecopy "mouse-2, RET: visit file"))

(defun hap-pretty-marker (entry)
  (let ((mk (car entry)))
    (format "%s:%d" (if (consp mk) (file-name-nondirectory (car mk)) (marker-buffer mk))
            (cdr (nth 4 entry)))))

(defun hippie-eldoc-view ()
  "create temp buffer in help mode with all hippie-eldoc/`hippie-goto' targets"
  (interactive)
  (hippie-eldoc-function)
  (when (not hap-eldoc-current-prototypes)
    (error "Nothing found"))
  (with-help-window (help-buffer)
    (with-current-buffer (help-buffer)
      (insert "Definitions for tag '" hap-eldoc-last-symbol "'\n")
      (setq hap-eldoc-current-prototypes (mapcar 'hap-update-entry hap-eldoc-current-prototypes))
      (let ((lst (sort hap-eldoc-current-prototypes (lambda (x y)
                                                      (string-lessp (nth 1 y) (nth 1 x)))))
            last-context)
        (dolist (entry lst)
          (when hap-debug-enabled
            (princ entry)
            (insert "\n"))
          (let ((context (nth 1 entry)))
            (unless (equal context last-context)
              (setq last-context context)
              (insert "\n" context)))
          (when (> (length (nth 2 entry)) 0)
            (insert (nth 2 entry)))
          (insert-text-button (concat (hap-pretty-marker entry) ":")
                              'type 'hippie-eldoc-view
                              'help-args (list entry))
          (insert (nth 3 entry) "\n"))))))
    

(defun hippie-eldoc-next ()
  "Show next possible hippie-goto target"
  (interactive)
  (eldoc-add-command 'hippie-eldoc-next)
  (if hap-eldoc-current-prototypes
      (progn
        (setq hap-eldoc-prototype-index (1+ hap-eldoc-prototype-index))
        (eldoc-message (hap-get-current-message)))
    (eldoc-message (hippie-eldoc-function))))

(defun hap-substr-equal (a b)
  (let ((la (length a)) (lb (length b)))
    (cond
     ((eq la lb) (string-equal a b))
     ((> la lb) (string-match-p (regexp-quote b) a))
     (t (string-match-p (regexp-quote a) b)))))

(defun hap-dup-key (x)
  (substring-no-properties
   (concat (hap-collapse-spaces (hap-pretty-marker x))
           (hap-collapse-spaces (car (nth 4 x))))))

(defun hippie-eldoc-function ()
  "`hippie-eldoc' function for `eldoc-documentation-function'.
return a string representing the prototype for the function under point"
  (unless (or ac-completing                 ; suppress while autocomplete is enabled
              (minibuffer-selected-window)) ; suppress while minibuffer is in use
    (when (or hap-debug-enabled
              (not hap-eldoc-current-prototypes)
              (not (string-equal (thing-at-point 'symbol) hap-eldoc-last-symbol)))
      (setq hap-eldoc-last-symbol (thing-at-point 'symbol))
      ;; try to move out of an argument list onto the function name
      (let ((start (point))
            (search-start (or (save-excursion (re-search-backward "[;{}#]" (point-min) t))
                              (line-beginning-position)))
            forward-sexp-function                     ; work around bug in up-list
            ebrowse-position-stack                    ; save ebrowse stack
            prototypes (scanok t)
            (hap-eldoc-in-progress t))
        (save-excursion 
          (save-etags-state
           (while (and (not prototypes) scanok (> (point) search-start))
             ;; return the longest prototype, by string length
             (setq prototypes (hap-do-it hippie-goto-try-functions-list 'hap-find-prototype))
             (unless prototypes
               (setq scanok (ignore-errors (up-list -1) (backward-char 1) t))))))
        ;; display the prototype with the longest comment
        (setq prototypes
              (with-no-warnings
                (delete-duplicates
                 (sort prototypes
                       (lambda (a b) (> (length (nth 1 a)) (length (nth 1 b)))))
                 :key 'hap-dup-key
                 :test 'hap-substr-equal)))
        (unless (equal prototypes hap-eldoc-current-prototypes)
          (setq hap-eldoc-current-prototypes prototypes)
          (setq hap-eldoc-prototype-index 0)
          ;; skip past definition from current line
          (while (and
                  (< hap-eldoc-prototype-index (length prototypes))
                  (let ((entry (hap-get-current-entry)))
                    (and (eq (current-buffer) (hap-marker-buffer (car entry) 'find-buffer-visiting))
                         (< (abs (- (line-number-at-pos) (cdr (nth 4 entry)))) 4))))
            (setq hap-eldoc-prototype-index (1+ hap-eldoc-prototype-index))
            ))))
    (hap-get-current-message)
    ))

;;;###autoload
(defun hippie-eldoc (&optional arg)
  "Enable `eldoc-mode' using the same mechanism as `hippie-goto' to find function prototypes.
Particularly useful for c/c++, where it can use ebrowse, imenu, and or tag data"
  (interactive)
  (with-no-warnings
    (make-variable-buffer-local 'eldoc-documentation-function))
  (if (or (and (not (or (boundp 'eldoc-mode) eldoc-mode)) (null arg))
          (and (numberp arg) (> arg 0)))
      (progn
        (setq eldoc-documentation-function 'hippie-eldoc-function)
        (eldoc-mode 1))
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
          quit? errorMsg mapresult res)
      (dolist (el list)
        (setq errorMsg nil)
        ;; call first function to detect presence of symbol
        (setq res (hap-filter-symbol (ignore-errors (funcall (car el)))))
        (cond
         ((consp (car-safe res))
          (setq mapresult (append res mapresult)))
         (res
          (condition-case e
              ;; call second function to operate on symbol (i.e. change buffer)
              (if mapfun
                  (with-no-interactivity
                   (call-interactively (cdr el)))
                (call-interactively (cdr el)))
            (quit (setq quit? t))             ; keyboard quit
            (error (setq errorMsg (nth 1 e))  ; error / exception
                   (hap-set-all-buffers-point bufpoint)
                   (set-window-configuration window-config)))
          (when (and (not mapfun) hap-debug-enabled)
            (when errorMsg
              (setq mapresult (cons (list "error" el errorMsg) mapresult)))
            (when quit?
              (setq mapresult (cons (list "quit" el) mapresult))))
          (unless (or quit? errorMsg)
            (if mapfun
                (progn
                  ;; call mapfun to collect result
                  (setq res (funcall mapfun el))
                  (when res
                    (setq mapresult (cons res mapresult)))
                  (hap-set-all-buffers-point bufpoint))
              (throw 'done t))))))
      (unless mapfun
        (cond (quit? (message "No more help"))
              (errorMsg (message "An error occured: %s" errorMsg))
              (t (message "No help, sorry"))))
      (setq mapresult (nreverse mapresult))
      mapresult)))

(defvar hippie-help-mode-map
  (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-?") 'hippie-help)
        (define-key map (kbd "C-M-?") 'hippie-help)
        (define-key map [remap find-tag] 'hippie-goto)                           ; M-.
        (define-key map [remap find-tag-other-window] 'hippie-goto-other-window) ; C-x 4 .
        (define-key map (kbd "C-M-.") 'hippie-goto-other-window)
        (define-key map (kbd "s-.") 'hippie-eldoc-next)
        (define-key map (kbd "s-/") 'hippie-eldoc-view)
        map))

;;;###autoload
(define-minor-mode hippie-help-mode
  "Toggle keybindings for hippie help
\\{hippie-help-mode-map}"
  :keymap hippie-help-mode-map
  :global t
  )

(provide 'hippie-help)
