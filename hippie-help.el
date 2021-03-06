;;; -*- lexical-binding: t; -*-
;;; hippie-help.el - Intelligent help and navigation
;;
;;; History:
;; 
;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; Copyright 2008-2020 Arthur Danskin <arthurdanskin@gmail.com>
;; April 2008 - initial version 
;; May   2008 - ebrowse support, fix man support, cleanups
;; July  2013 - hippie-eldoc mode
;; June  2014 - hippie-eldoc-view, cycle through multiple definitions in eldoc
;; July  2019 - improve performance
;; August 2020 - emacs 27 update, reduce consing
;; September 2020 - cleanup
;; 
;;; Commentary:
;; 
;; This package provides a unified interface for context sensitive help and navigation, analogous
;; to `hippie-expand'. `hippie-goto' is a generalized `find-tag' / `imenu' / `find-function' /
;; etc. and `hippie-help' is a generalized `describe-function' / `describe-variable' /
;; `python-describe-symbol' / etc. `hippie-eldoc' uses the machinery of `hippie-goto' to show the
;; definition of the function at point in the minibuffer.
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
(defvar hap-eldoc-current-symbol nil)
(defvar hap-eldoc-in-progress nil)
(defvar hap-current-sym nil)

(defmacro save-etags-state (&rest body)
  "Save the etags mark ring, last tag, etc so that etags
functions run as part of BODY will not change globals state"
  `(let ((tags-location-ring (make-ring 1))
         (xref--marker-ring (make-ring 1))
         (mark-ring nil)
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

(defun hap-opengl-at-point ()
  (and (functionp 'opengl-function-at-point) (with-no-warnings (opengl-function-at-point))))

(defun hap-truename (name)
  (when (and (functionp 'cygwin-convert-file-name-from-windows)
             (eq (elt name 1) ?:))
    (setq name (cygwin-convert-file-name-from-windows name)))
  (abbreviate-file-name name)
  ;; file-truename is really slow
  ;; (abbreviate-file-name (file-truename name))
  )

;; find-buffer-visiting is really slow
;; we skip the file-attribute stuff
(defun hap-find-buffer (filename)
  (or (get-file-buffer filename)
      (let ((list (buffer-list)) found
            (truename (hap-truename filename)))
        (while (and (not found) list)
          (with-current-buffer (car list)
            (if (and buffer-file-truename (string= buffer-file-truename truename))
                (setq found (car list))))
          (setq list (cdr list)))
        found)))

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
  (or hap-current-sym (hap-filter-symbol
                       (save-match-data
                         (save-excursion (thing-at-point 'c++-symbol))))))

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
        (or (and (search-forward prototype nil t)
                 (match-beginning 0))
            (and (goto-char pos)
                 (search-backward prototype nil t)
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

(defun hap-imenu-at-point (&optional method sym)
  "return (SYMBOL . MARKER) for SYM or the symbol at point with `imenu', else nil"
  (setq sym (or sym (hap-symbol-at-point)))
  (and sym
       (let* (;;(imenu-auto-rescan t)
              (imenu-name-lookup-function (cdr-safe (assoc major-mode hap-imenu-comparator-alist)))
              (val (ignore-errors (imenu--in-alist sym (imenu--make-index-alist t)))))
         (if (and hap-eldoc-in-progress (markerp (cdr-safe val)))
             (list (hap-find-prototype (or method 'imenu)
                                       (marker-buffer (cdr val))
                                       (marker-position (cdr val))))
           val))))

(defun hap-imenu-read (str item)
  ;; TODO improve me
  (let ((sym (car item)))
    (list (completing-read (format "%s (default %s): " str sym)
                           (list sym) nil nil nil nil sym)
          (cdr item))))

(defun hap-imenu (&optional _sym marker)
  "Interactively, prompt for Imenu symbol and go to the marker position"
  (interactive (hap-imenu-read "Imenu" (hap-imenu-at-point)))
  (unless marker
    (setq marker (cdr-safe (hap-imenu-at-point))))
  (when marker
    (goto-char marker)))

(defun hap-imenu-at-point-other-file ()
  "return (SYMBOL . MARKER) for the symbol at point using `imenu',
in the file returned by `ff-find-other-file'"
  (with-no-warnings
    (let* ((oname (expand-file-name (let ((inhibit-message t)
                                          (message-log-max nil))
                                      (ff-other-file-name))))
           (obuf (and oname (hap-find-buffer oname)))
           (sym (hap-symbol-at-point)))
      (when (and obuf (not (eq (current-buffer) obuf)))
        (with-current-buffer obuf
          (hap-imenu-at-point 'imenu-other-file sym))))))

(defun hap-imenu-other-file (_sym marker)
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
  (hap-save-location)
  (with-no-warnings
    (semantic-ia-fast-jump (point))))

(defun hap-gtags-at-point ()
  (and (boundp 'gtags-mode) gtags-mode
       (with-no-warnings (gtags-current-token))))

(defvar hippie-help-try-functions-list
  '((hap-opengl-at-point . describe-opengl-function)
    ;; (hap-doxygen-at-point . doxymacs-lookup)
    (hap-octave-help2-at-point . octave-help2)
    (hap-python-at-point . python-describe-symbol)
    (hap-variable-at-point . describe-variable)
    (hap-function-at-point . describe-function)
    ;; (hap-man-page-at-point . man)
    (hap-face-at-point . describe-face))
  "*Alist of functions to use for `hippie-help'. The car of each
  pair is a predicate that returns non-nil if we can use the cdr
  of that pair.")

(defvar hippie-goto-try-functions-list
  '((hap-imenu-at-point . hap-imenu)
    (hap-imenu-at-point-other-file . hap-imenu-other-file)
    ;; (hap-semantic-at-point . hap-semantic-jump)
    (hap-tag-at-point . hap-find-tag)
    ;; (hap-ebrowse-at-point . ebrowse-tags-find-definition)
    ;; (hap-ebrowse-at-point . ebrowse-tags-find-declaration)
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

(defun hap-do-eldoc-entry (x-to-buffer)
  (when (hippie-eldoc-function)
    (let ((entry (hap-get-current-entry)))
      (when entry
        (hap-window-to-entry entry x-to-buffer)
        (setq hap-eldoc-current-prototypes nil
              hap-eldoc-current-symbol nil)
        entry))))

(defun hap-test ()
  (interactive)
  (hap-pop-to-buffer (hap-find-buffer "Resources.cpp"))
  )

(defun hap-save-location ()
  (push-mark nil t)
  (ring-insert (if (boundp 'xref--marker-ring)
                   xref--marker-ring
                   (with-no-warnings find-tag-marker-ring))
               (point-marker)))
  
;;;###autoload
(defun hippie-goto ()
  "Intelligently go to the definition of the thing at point.
You can return with \\[pop-tag-mark] or C-u \\[set-mark-command].
You can customize the way this works by changing
`hippie-goto-try-functions-list'."
  (interactive)
  (hap-save-location)
  (when (or (hap-do-eldoc-entry 'switch-to-buffer)
            (hap-do-it hippie-goto-try-functions-list))
    (run-hooks 'xref-after-jump-hook)))

;;;###autoload
(defun hippie-goto-other-window (&optional _arg)
  "Intelligently go to the definition of the thing at point, in another window.
You can return with \\[pop-tag-mark] or C-u \\[set-mark-command].
You can customize the way this works by changing
`hippie-goto-try-functions-list'."
  (interactive "P")
  (hap-save-location)
  (or (hap-do-eldoc-entry 'hap-pop-to-buffer)
      (let (buf point)
        (save-excursion
          (when (hap-do-it hippie-goto-try-functions-list)
            (setq buf (current-buffer))
            (setq point (point))))
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
  (hap-do-it hippie-help-try-functions-list))

(defun hap-collapse-spaces (str)
  (save-match-data
    (when (string-match "\\`[ \n\t]+" str)
      (setq str (substring str (match-end 0)))))
  (when (string-match-p "[ \n\t]+" str)
    (setq str (replace-regexp-in-string "[ \n\t]+" " " str)))
  str)

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
          ;; (let ((type (intern (save-excursion (beginning-of-line) (symbol-at-point)))))
          (setq proto (hap-collapse-spaces
                       (buffer-substring (line-beginning-position)
                                         (save-excursion
                                           (re-search-forward ":$" (point-max)))))))

         (t
          (setq proto (hap-collapse-spaces
                       (buffer-substring (line-beginning-position)
                                         (line-end-position)))))
          )
        ;; return prototype
        (list start
              (if struct (concat struct "...") nil)
              (if comment (hap-trim-to-max-lines comment hap-max-comment-lines "...\n") nil)
              proto
              (cons line-str start-line)
              elt)))))

(defun hap-silent-completing-read (_prompt collect &optional pred _req init _hist def)
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
  (let ((ctx (nth 1 entry)))
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
      (insert "Definitions for tag '" hap-eldoc-current-symbol "'\n")
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
  (let ((lmin (min (length a) (length b))))
    (eq t (compare-strings a nil lmin b nil lmin))))
  ;; (let ((la (length a)) (lb (length b)))
    ;; (cond
      ;; ((eq la lb) (string-equal a b))
      ;; ((> la lb) (string-equal (substring a 0 lb) b))
      ;; (t (string-equal (substring b 0 la) a))
     ;; ((> la lb) (string-match-p (regexp-quote b) a))
     ;; (t (string-match-p (regexp-quote a) b))
     ;; )))

(defun hap-dup-key (x)
  (concat (hap-pretty-marker x)
          (hap-collapse-spaces (substring-no-properties (car (nth 4 x))))))

(defvar hap-curline)
(defvar hap-curfile)

;; tag entries don't have a buffer! compare filename
(defun hap-sort-key (entry)
  (let ((buffer (hap-marker-buffer (car entry) 'hap-find-buffer))
        (fname (intern (file-name-nondirectory (hap-marker-filename (car entry))))))
    (+
     ;; current match last, current file forward
     (if (eq fname hap-curfile)
         (if (< (abs (- hap-curline (cdr (nth 4 entry)))) 4)
             5 -3)
       0)
     ;; push definitions forward
     (let ((line (nth 3 entry)))
       (if (and line (string-match-p "{" line)) -1 0))
     ;; push loaded files forward
     (if buffer -2 0))))

(defun hap-compare (a b)
  (< (hap-sort-key a) (hap-sort-key b)))

(defun hap-sort-filter-entries (entries &optional finalp)
  (when (or finalp (< (length entries) 4))
    (setq entries (mapcar 'hap-update-entry entries)))
  (with-no-warnings
    (let ((hap-curline (line-number-at-pos))
          (hap-curfile (intern (file-name-nondirectory buffer-file-name))))
      (mapcar 'cdr (sort (mapcar (lambda (x) (cons (hap-sort-key x) x))
                                 (delete-duplicates entries :key 'hap-dup-key :test 'hap-substr-equal))
                         (lambda (a b) (< (car a) (car b))))))))
            ;; 'hap-compare))))

(defun hap-current-sym ()
  (let ((sym (hap-symbol-at-point)))
    (unless sym
      (let ((search-start (or (save-excursion (re-search-backward "[;{}#]" (point-min) t))
                              (line-beginning-position)))
            forward-sexp-function       ; work around bug in up-list
            ebrowse-position-stack)     ; save ebrowse stack
        (save-excursion
          (while (and (not sym)
                      (ignore-errors (up-list -1) (backward-char 1) t)
                      (> (point) search-start))
            (setq sym (hap-symbol-at-point))))))
    sym))

(defun hippie-eldoc-function ()
  "`hippie-eldoc' function for `eldoc-documentation-function'.
return a string representing the prototype for the function under point"
  (let ((hap-current-sym (hap-current-sym))
        (hap-eldoc-in-progress t))
    (unless (or (not (eq eldoc-documentation-function 'hippie-eldoc-function))
                (and (boundp 'ac-completing) ac-completing) ; suppress while autocomplete is enabled
                (minibuffer-selected-window)                ; suppress while minibuffer is in use
                (not hap-current-sym))                      ; early exit if not looking at anything
      (when (or hap-debug-enabled
                (not hap-eldoc-current-prototypes)
                (not (string-equal hap-current-sym hap-eldoc-current-symbol)))
        (let ((prototypes (hap-sort-filter-entries
                           (hap-collect-prototypes hippie-goto-try-functions-list))))
          (unless (equal prototypes hap-eldoc-current-prototypes)
            (setq hap-eldoc-current-symbol hap-current-sym
                  hap-eldoc-current-prototypes prototypes
                  hap-eldoc-prototype-index 0))))
      (hap-get-current-message))))


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
        ;; remove all advice
        (let ((sym eldoc-documentation-function))
          (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))
        (setq eldoc-documentation-function 'hippie-eldoc-function)
        (eldoc-add-command 'hippie-eldoc-next)
        (eldoc-add-command 'hippie-eldoc-previous)
        (eldoc-mode 1))
    (eldoc-mode -1)))

(defun hap-get-wincfg ()
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
    (list (current-window-configuration)
          (window-point)
          (cons (current-buffer) lst))))

(defun hap-set-wincfg (wincfg)
  "Restore current buffer and point for buffers previously saved
with `hap-get-all-buffers-point'"
  (let ((curbuf (car (nth 2 wincfg)))
        (markers (cdr (nth 2 wincfg))))
    (dolist (marker markers)
      (set-buffer (marker-buffer marker))
      (goto-char marker))
    (set-buffer curbuf))
  (set-window-configuration (nth 0 wincfg))
  ;; (set-window-point (nth 1 wincfg))
  )


(defun hap-do-it (list)
  "If you don't like the first option you get, you can
\\[keyboard-quit] to try more options. This works by evaluating
the first function in each pair of LIST. If it returns non-nil,
then it calls the second function."
  (catch 'done
    (let ((wincfg (hap-get-wincfg))
          quit? errorMsg res)
      (dolist (el list)
        (setq errorMsg nil)
        ;; call first function to detect presence of symbol
        (setq res (hap-filter-symbol (ignore-errors (funcall (car el)))))
        (when res
          (condition-case e
              ;; call second function to operate on symbol (i.e. change buffer)
              (call-interactively (cdr el))
            (quit (setq quit? t))             ; keyboard quit
            (error (setq errorMsg (nth 1 e))  ; error / exception
                   (hap-set-wincfg wincfg)))
          (unless (or quit? errorMsg)
            (throw 'done t))))
      (cond (quit? (message "No more help"))
            (errorMsg (message "An error occured: %s" errorMsg))
            (t (message "No help, sorry")))
      nil)))

(defun hap-collect-prototypes (list)
  (let (mapresult)
    (dolist (el list)
      (setq mapresult (append (ignore-errors (funcall (car el))) mapresult)))
    (nreverse mapresult)))


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
