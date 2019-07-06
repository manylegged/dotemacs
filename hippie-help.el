;;; hippie-help.el - Intelligent help and navigation
;;
;;; History:
;; 
;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; Copyright 2008-2014 Arthur Danskin <arthurdanskin@gmail.com>
;; April 2008 - initial version 
;; May   2008 - ebrowse support, fix man support, cleanups
;; July  2013 - hippie-eldoc mode
;; June  2014 - hippie-eldoc-view, cycle through multiple definitions in eldoc
;; July  2019 - improve performance
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
          (find-file-noselect (name &rest args) (set-buffer (hap-find-buffer name))))
     ,@body))

(defun call-interactively-no-prompt (func)
  "Like `call-interactively', but automatically use default for any prompts"
  (with-no-interactivity
   (call-interactively func)))

(defmacro save-etags-state (&rest body)
  "Save the etags mark ring, last tag, etc so that etags
functions run as part of BODY will not change globals state"
  `(let ((tags-location-ring (make-ring 1))
         (xref--marker-ring (make-ring 1))
         (mark-ring (make-ring 1))
         last-tag find-tag-history)
     ,@body))

(defun hap-filter-symbol (sym)
  "Return non-nil if symbol string should be processed further"
  (if (stringp sym)
      (and (> (length sym) 2)
           (string-match-p "^[~_a-zA-Z]" sym)
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
  (memq (intern-soft (hap-symbol-at-point)) (face-list)))

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

(defun hap-find-buffer (name)
  (or (get-file-buffer name)
      (and (functionp 'cygwin-convert-file-name-from-windows)
           (get-file-buffer (cygwin-convert-file-name-from-windows name)))))
;; find-buffer-visiting is really slow
  ;; (or (find-buffer-visiting name)
      ;; (and (functionp 'cygwin-convert-file-name-from-windows)
      ;;      (find-buffer-visiting (cygwin-convert-file-name-from-windows name)))
      ;; ))

(defun hap-truename (name)
  (when (and (functionp 'cygwin-convert-file-name-from-windows)
             (eq (elt name 1) ?:))
    (setq name (cygwin-convert-file-name-from-windows name)))
  (abbreviate-file-name
   (file-truename name)))

(defun forward-c++-symbol (arg)
  "Called from `thing-at-point' c++-symbol"
  (if (natnump arg)
      (re-search-forward "\\(\\sw\\|\\s_\\)+" nil 'move arg)
    (while (< arg 0)
      (if (re-search-backward "\\(\\sw\\|\\s_\\)+" nil 'move)
	  (skip-syntax-backward "w_"))
      (if (eq (char-before) ?~)
          (backward-char))
      (setq arg (1+ arg)))))

(defun hap-symbol-at-point ()
  "same as `symbol-at-point', but properly handles C++ destructors"
  (hap-filter-symbol (thing-at-point 'c++-symbol)))

(defun hap-tag-at-point ()
  "Return non-nil if point is contained in an etags tag."
  (cond 
   ((not (memq major-mode '(c-mode c++-mode objc-mode))) nil)
   (hap-eldoc-in-progress (hap-search-tags))
   (t (let ((sym (hap-symbol-at-point))
            message-log-max                 ; inhibit messages going to log
            case-fold-search)               ; case sensitive
        (and sym tags-file-name
             (save-current-buffer
               (visit-tags-table-buffer)
               (save-excursion
                 (goto-char (point-min))
                 (search-forward (concat "" sym "") (point-max) t)))
             sym)))))

(defun hap-update-prototype-pos (prototype buffer pos)
  "Return updated position"
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (beginning-of-line)
      (save-match-data
        (or (and (search-forward prototype (point-max) t)
                 (match-beginning 0))
            (and (goto-char pos)
                 (search-backward prototype (point-min) t)
                 (match-beginning 0))
            pos)))))

(defun hap-find-etag-filename-make-entry (prototype line pos)
  (let* ((filename (hap-truename
                    (save-excursion
                      (re-search-backward "\n\\([^,]+\\)," (point-min) t)
                      (match-string 1)))))
    (list (cons filename (1+ pos))
          nil
          nil
          (hap-collapse-spaces prototype)
          (cons prototype line)
          'TAGS)))

(defun hap-search-tags ()
  "Search tags file to find definitions for symbol at point.
Return in same format as `hap-find-prototype'."
  (let* ((sym (hap-symbol-at-point))
         message-log-max                 ; inhibit messages going to log
         case-fold-search                ; case sensitive
         (regexp (concat "\\(\\(" sym "\\)\\|\\(" sym "[ (]?\\)\\)\\([0-9]*\\),\\([0-9]*\\)"))
         tags)
    (save-excursion
      (and
       sym tags-file-name
       (visit-tags-table-buffer)
       (progn
         (goto-char (point-min))
         ;; format is 'prototype  symbol  line, byte'
         ;; sometimes 'symbol ' is missing
         (while (re-search-forward regexp (point-max) t)
           (let* ((prototype (buffer-substring (line-beginning-position)
                                               (1- (or (match-end 3) (1+ (match-beginning 0))))))
                  (pos (string-to-number (match-string 5)))
                  (line (string-to-number (match-string 4)))
                  (entry (hap-find-etag-filename-make-entry prototype line pos)))
             (push entry tags)))
         tags)))))

(defun hap-find-tag ()
  (interactive)
  (save-etags-state
   (let ((tags-case-fold-search nil))
     (call-interactively 'find-tag))))

(defun hap-octave-help2-at-point ()
  (and (memq major-mode '(octave-mode inferior-octave-mode))
       (require 'octave-hlp2 nil t)
       (with-no-warnings (octave-help2-at-point))))

(defun hap-python-at-point ()
  (and (eq major-mode 'python-mode)
       (hap-symbol-at-point)))

(defun hap-imenu-c++-comparator (str pat)
  (when (stringp str)
    (string-match-p (concat "^\\([a-zA-Z0-9_]+::\\)?" (regexp-quote str) "$") pat)))

(defun hap-imenu-python-comparator (str pat)
  (string-match-p (concat "^" (regexp-quote str) "\\(.(\\(class\\|def\\))\\)?$") pat))

(defvar hap-imenu-comparator-alist
  '((c++-mode . hap-imenu-c++-comparator)
    (python-mode . hap-imenu-python-comparator)))

(defun hap-imenu-at-point (&optional method)
  "return (SYMBOL . MARKER) for SYM or the symbol at point with `imenu', else nil"
  (let ((sym (hap-symbol-at-point)))
    (and sym
         (let* ((imenu-auto-rescan t)
                (imenu-name-lookup-function (cdr-safe (assoc major-mode hap-imenu-comparator-alist)))
                (val (ignore-errors (imenu--in-alist sym (imenu--make-index-alist t)))))
           (if (and hap-eldoc-in-progress (markerp (cdr-safe val)))
               (list (hap-find-prototype (or method 'imenu)
                                         (marker-buffer (cdr val))
                                         (marker-position (cdr val))))
             val)))))

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
  (let ((sym (hap-symbol-at-point))
        (buf (current-buffer)))
    (save-current-buffer
      (with-no-interactivity
       (unless (catch 'hap-ff-not-found
                 (let ((ff-not-found-hook (lambda () (throw 'hap-ff-not-found t))))
                   (ff-find-other-file))
                 nil)
         (unless (eq buf (current-buffer))
           (hap-imenu-at-point 'imenu-other-file)))))))

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
              (name (hap-symbol-at-point)))
         (and name
              (gethash name members)
              name))))


(defun hap-semantic-at-point ()
  (with-no-warnings
    (when (and (featurep 'semantic) semantic-mode)
      (semantic-analyze-current-context (point)))))

(defun hap-semantic-jump ()
  (interactive)
  (ring-insert xref--marker-ring (point-marker))
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
    ;; (ffap-file-at-point . find-file-at-point)
    )
  "*Alist of functions to use for `hippie-goto'. The car of each
  pair is a predicate that returns non-nil if we can use the cdr
  of that pair.")

(defsubst hap-marker-buffer (loc open)
  (if (markerp loc) (marker-buffer loc) (funcall open (car loc))))

(defsubst hap-marker-filename (loc)
  (if (markerp loc)
      (buffer-file-name (marker-buffer loc))
    (car loc)))

(defsubst hap-marker-position (loc)
  (if (markerp loc) (marker-position loc) (cdr loc)))

(defun hap-window-to-entry (entry x-to-buffer)
  (let* ((loc (car entry))
         (buffer (hap-marker-buffer loc 'find-file-noselect))
         (pos (hap-marker-position loc)))
    (funcall x-to-buffer buffer)
    (goto-char pos)))

(defun hap-pop-to-buffer (buf)
  (pop-to-buffer buf t))

(defun hap-pop-to-entry (entry)
  (hap-window-to-entry entry 'hap-pop-to-buffer))

(defun hap-do-current-entry (x-to-buffer)
  (and (hippie-eldoc-function)
       hap-eldoc-current-prototypes
       (let ((entry (hap-get-current-entry)))
         (hap-window-to-entry entry x-to-buffer)
         (setq hap-eldoc-current-prototypes nil)
         entry)))

(defun hap-test ()
  (interactive)
  (hap-pop-to-buffer (hap-find-buffer "Resources.cpp"))
  )
  
;;;###autoload
(defun hippie-goto ()
  "Intelligently go to the definition of the thing at point.
You can return with \\[pop-tag-mark] or C-u \\[set-mark-command].
You can customize the way this works by changing
`hippie-goto-try-functions-list'."
  (interactive)
  (push-mark nil t)
  (ring-insert xref--marker-ring (point-marker))
  (when (or (hap-do-current-entry 'switch-to-buffer)
            (hap-do-it hippie-goto-try-functions-list nil))
    (run-hooks 'xref-after-jump-hook)))

;;;###autoload
(defun hippie-goto-other-window (&optional arg)
  "Intelligently go to the definition of the thing at point, in another window.
You can return with \\[pop-tag-mark] or C-u \\[set-mark-command].
You can customize the way this works by changing
`hippie-goto-try-functions-list'."
  (interactive "P")
  (push-mark nil t)
  (ring-insert xref--marker-ring (point-marker))
  (or (hap-do-current-entry 'hap-pop-to-buffer)
      (let (buf point)
        (save-window-excursion
          (save-excursion
            (when (with-no-interactivity (hap-do-it hippie-goto-try-functions-list nil))
              (setq buf (current-buffer))
              (setq point (point)))))
        (when buf
          (hap-pop-to-buffer buf)
          (goto-char point)
          (run-hooks 'xref-after-jump-hook)))))

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

(defun hap-find-prototype (&optional elt buffer pos)
  "Grab and return the function prototype and any accompanying
comments or relevant context around point. Prototype is returned
as (MARKER CONTEXT COMMENT PROTOTYPE UNIQUE METHOD)
MARKER is either a marker or (FILENAME . POSITION)
UNIQUE is (PROTO-LINE . LINE-NUMBER)
The rest are strings"
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (when pos
        (goto-char pos))
      (let ((start (point-marker))
            (start-line (line-number-at-pos))
            (line-str (buffer-substring (line-beginning-position) (line-end-position)))
            (proto-is-statement t) struct-mid struct comment proto (first-struct t))
        (cond
         ((memq major-mode '(c-mode c++-mode objc-mode))
          (when (save-excursion (back-to-indentation) (looking-at-p "{"))
            (forward-line -1))
          ;; enclosing context (i.e. "struct foo {" or "enum foo {" (recursive)
          (while (setq struct-mid 
                       (save-excursion
                         (when struct-mid (goto-char struct-mid))
                         (ignore-errors (let (forward-sexp-function) (up-list -1)) (point))))
            (save-excursion
              (goto-char struct-mid)
              (let ((last-struct struct)
                    (struct-beg (or (save-excursion 
                                      (ignore-errors (c-beginning-of-statement 1 nil t) (point)))
                                    (line-beginning-position)))
                    (struct-end (line-end-position)))
                (when struct-beg ;; (and struct-beg (< struct-beg beg))
                  (font-lock-fontify-region struct-beg struct-end)
                  (setq struct (concat (hap-collapse-spaces (buffer-substring struct-beg struct-end))
                                       (and struct (concat "...\n" struct))))
                  (cond
                   ((string-match-p "extern \"C\"" struct) ; not an interesting context
                    (setq struct last-struct))
                   ((and first-struct (string-match-p "enum" struct))
                    (setq proto-is-statement nil))))
                (setq first-struct nil))))
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
            ))

         ((eq major-mode 'python-mode)
          (let ((type (intern (save-excursion (beginning-of-line) (symbol-at-point)))))
            (setq proto (hap-collapse-spaces
                         (buffer-substring (line-beginning-position)
                                           (save-excursion
                                             (re-search-forward ":$" (point-max))))))))
          )
        ;; return prototype
        (list start
              (if struct (concat struct "...") nil)
              (if comment (hap-trim-to-max-lines comment hap-max-comment-lines "...\n") nil)
              proto
              (cons line-str start-line)
              elt)))))

(defun hap-silent-completing-read (prompt collect &optional pred req init hist def)
  "Same arguments and return as `completing-read', but just pick
the default and don't actually prompt user"
  (setq unread-command-events nil)      ; ebrowse inserts a '?' with this!
  (or (and init (try-completion init collect pred) init)
      (car-safe def)
      def
      (car-safe (all-completions "" collect pred))
      ""))

(defun hap-update-entry (entry)
  (let ((buffer (and (consp (car entry))
                     (hap-find-buffer (caar entry)))))
    (if buffer
        (let* ((prototype (car (nth 4 entry)))
               (pos (hap-update-prototype-pos prototype buffer (cdar entry)))
               (nentry (hap-find-prototype (nth 5 entry) buffer pos)))
          (if hap-debug-enabled (append nentry entry)
            nentry))
      entry)))

(defun hap-get-context (entry)
  (let ((ctx (nth 1 entry))
        (loc (car entry)))
    (concat
     (or ctx
         (concat (hap-pretty-marker entry)))
     (if hap-debug-enabled
         (concat " " (symbol-name (nth 5 entry)))
       "")
     "\n")))

(defun hap-get-current-entry ()
  (when hap-eldoc-current-prototypes
    (let* ((count (length hap-eldoc-current-prototypes))
           (index (mod hap-eldoc-prototype-index count))
           (entry (hap-update-entry (nth index hap-eldoc-current-prototypes))))
      entry)))

(defun hap-add-trailing-newline (str)
  (if (or (null str) (eq (length str) 0) (string-match-p "\n$" str))
      str
    (concat str "\n")))

(defun hap-get-current-message ()
  (when hap-eldoc-current-prototypes
    (let* ((count (length hap-eldoc-current-prototypes))
           (index (mod hap-eldoc-prototype-index count))
           (entry (hap-update-entry (nth index hap-eldoc-current-prototypes)))
           (prefix (and (> count 1) (format "(%d/%d) " (1+ index) count))))
      (setcar (nthcdr index hap-eldoc-current-prototypes) entry) ; save updated entry
      (concat prefix
              (hap-get-context entry)
              (hap-add-trailing-newline (nth 2 entry))
              (nth 3 entry)))))

(define-button-type 'hippie-eldoc-view
  'follow-link t
  'action 'help-button-action
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
      (setq hap-eldoc-current-prototypes (hap-sort-filter-entries hap-eldoc-current-prototypes t))
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
              (insert "\n")
              (when context
                (insert context "\n"))))
          (when (> (length (nth 2 entry)) 0)
            (insert (nth 2 entry)))
          (insert-text-button (concat (hap-pretty-marker entry) ":")
                              'type 'hippie-eldoc-view
                              'help-args (list entry))
          (insert (nth 3 entry)
                  (if hap-debug-enabled
                      (format "%s" (symbol-name (nth 5 entry)))
                    "")
                  "\n")
          )))))
    

(defun hippie-eldoc-next (arg)
  "Show next possible `hippie-goto' target"
  (interactive "p")
  (setq arg (or arg +1))
  (if hap-eldoc-current-prototypes
      (progn
        (setq hap-eldoc-prototype-index (+ arg hap-eldoc-prototype-index))
        (eldoc-message (hap-get-current-message)))
    (eldoc-message (hippie-eldoc-function))))

(defun hippie-eldoc-previous ()
  "Show previous `hippie-goto' target"
  (interactive)
  (hippie-eldoc-next -1))

(defun hap-substr-equal (a b)
  (let ((la (length a)) (lb (length b)))
    (cond
     ((eq la lb) (string-equal a b))
     ((> la lb) (string-match-p (regexp-quote b) a))
     (t (string-match-p (regexp-quote a) b)))))

(defun hap-dup-key (x)
  (substring-no-properties
   (concat (hap-pretty-marker x)
           (hap-collapse-spaces (car (nth 4 x))))))

(defvar hap-curline)

(defun hap-sort-key (entry)
  (let ((file (hap-marker-filename (car entry)))
        (buffer (hap-marker-buffer (car entry) 'hap-find-buffer)))
    (+
     ;; current match last, current file forward
     (if (eq (current-buffer) buffer)
         (if (< (abs (- hap-curline (cdr (nth 4 entry)))) 4)
             5 -1)
       0)
     ;; push definitions forward
     (if (string-match-p "{" (nth 3 entry)) -1 0)
     ;; push loaded files forward
     (if buffer -2 0))))

(defun hap-compare (a b)
  (< (hap-sort-key a) (hap-sort-key b)))

(defun hap-sort-filter-entries (entries &optional finalp)
  (when (or finalp (< (length entries) 4))
    (setq entries (mapcar 'hap-update-entry entries)))
  (with-no-warnings
    (let ((hap-curline (line-number-at-pos)))
      (mapcar 'cdr (sort (mapcar (lambda (x) (cons (hap-sort-key x) x))
                                 (delete-duplicates entries :key 'hap-dup-key :test 'hap-substr-equal))
                         (lambda (a b) (< (car a) (car b))))))))
            ;; 'hap-compare))))

(defun hippie-eldoc-function ()
  "`hippie-eldoc' function for `eldoc-documentation-function'.
return a string representing the prototype for the function under point"
  (unless (or (not (eq eldoc-documentation-function 'hippie-eldoc-function))
              ac-completing                 ; suppress while autocomplete is enabled
              (minibuffer-selected-window)) ; suppress while minibuffer is in use
    (when (or hap-debug-enabled
              (not hap-eldoc-current-prototypes)
              (not (string-equal (hap-symbol-at-point) hap-eldoc-last-symbol)))
      (setq hap-eldoc-last-symbol (hap-symbol-at-point))
      ;; try to move out of an argument list onto the function name
      (let ((start (point))
            (search-start (or (save-excursion (re-search-backward "[;{}#]" (point-min) t))
                              (line-beginning-position)))
            forward-sexp-function                     ; work around bug in up-list
            ebrowse-position-stack                    ; save ebrowse stack
            prototypes (scanok t)
            (hap-eldoc-in-progress t))
        (save-excursion 
          (while (and (not prototypes) scanok (> (point) search-start))
            (setq prototypes (hap-do-it hippie-goto-try-functions-list 'hap-find-prototype))
            (unless prototypes
              (setq scanok (ignore-errors (up-list -1) (backward-char 1) t)))))
        (setq prototypes (hap-sort-filter-entries prototypes))
        (unless (equal prototypes hap-eldoc-current-prototypes)
          (setq hap-eldoc-current-prototypes prototypes)
          (setq hap-eldoc-prototype-index 0))))
    (hap-get-current-message)
    ))

;;;###autoload
(defun hippie-eldoc (&optional arg)
  "Enable `eldoc-mode' using the same mechanism as `hippie-goto' to find function prototypes.
Particularly useful for c/c++, where it can use ebrowse, imenu, and or tag data"
  (interactive)
  (with-no-warnings
    (make-variable-buffer-local 'eldoc-documentation-function))
  (if (or (and (not (and (boundp 'eldoc-mode) eldoc-mode)) (null arg))
          (and (numberp arg) (> arg 0)))
      (progn
        (setq eldoc-documentation-function 'hippie-eldoc-function)
        (eldoc-add-command 'hippie-eldoc-next)
        (eldoc-add-command 'hippie-eldoc-previous)
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
                  (call-interactively-no-prompt (cdr el))
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
                  (setq res (funcall mapfun (cdr el)))
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
        ;; (define-key map [remap find-tag] 'hippie-goto)                           ; M-.
        (define-key map (kbd "M-.") 'hippie-goto)              ; M-.
        (define-key map [remap find-tag-other-window] 'hippie-goto-other-window) ; C-M-.
        (define-key map [remap xref-find-definitions-other-window] 'hippie-goto-other-window) ; C-x 4 .
        (define-key map (kbd "C-M-.") 'hippie-goto-other-window)
        (define-key map (kbd "C-<") 'hippie-eldoc-previous)
        (define-key map (kbd "C->") 'hippie-eldoc-next)
        (define-key map (kbd "C-?") 'hippie-eldoc-view)
        map))

;;;###autoload
(define-minor-mode hippie-help-mode
  "Toggle keybindings for hippie help
\\{hippie-help-mode-map}"
  :keymap hippie-help-mode-map
  :global t
  )

(provide 'hippie-help)
