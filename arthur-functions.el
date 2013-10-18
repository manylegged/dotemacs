;; arthur-functions.el
;;
;; misc functions that were cluttering up my .emacs

(with-no-warnings
  (require 'cl))

(defun toggle-debug ()
  (interactive)
  (setq debug-on-error (not debug-on-error))
  (setq debug-on-signal debug-on-error)
  (setq debug-on-quit debug-on-error)
  (message "debug-on-error %sabled" (if debug-on-error "en" "dis")))

(defun print-buffer-file ()
  (interactive)
  (message buffer-file-name))

(defun find-user-init-file ()
  (interactive)
  (find-file user-init-file))


;; (defun in-string-p (face)
;;   "return FACE if we are fontifying a string, else nil"
;;   (if (eq (get-text-property (match-beginning 0) 'face)
;;           'font-lock-string-face)
;;       face
;;     nil))


(defun add-hooks (hooks function &optional append local)
  "Like `add-hooks', but specify a list of hooks instead of just
 one, and add the hook to all of them"
  (mapc (lambda (hook) (add-hook hook function append local))
        hooks))


(defun comment-or-uncomment-line (arg)
  "Comment the current line, unless it is already commented, in
 which case, uncomment it. With numeric prefix, comment that many
 lines"
  (interactive "p")
  (if (save-excursion (beginning-of-line) (looking-at "\\s-*$"))
      (comment-dwim nil)
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position arg))))

(defun comment-sexp (arg)
  "Comment out ARG s-expressions."
  (interactive "p")
  (comment-region
   (point) (save-excursion (forward-sexp arg) (point))))



(defun open-next-line (arg)
  "create a new line in front of the current line, indent, and
 move point to the next line. With a numeric argument, repeat ARG
 times. With a universal argument, copy the current line into the
 new line. Generalized version of `newline-and-indent'"
  (interactive "P")
  (save-excursion
    (end-of-line)
    (if (or (equal arg "-") (consp arg))
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (open-line 1)
          (with-no-warnings (next-line))
          (insert-buffer-substring (current-buffer) beg end))
      (open-line (or arg 1))))
  (with-no-warnings (next-line))
  (indent-according-to-mode))

(defun open-previous-line (arg)
  "Like `open-next-line', but backwards"
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (if (or (equal arg "-") (consp arg))
        (progn
          (open-line 1)
          (insert-buffer-substring (current-buffer)
                                   (line-beginning-position 2)
                                   (line-end-position 2)))
      (open-line (or arg 1))))
  (with-no-warnings (previous-line))
  (indent-according-to-mode))


;; windows

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun toggle-window-split ()
  "Switch between vertical and horizontal split"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                        (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                        (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun get-closest-pathname (file)
  "Return pathname of FILE searching from the cwd towards /.
 This may not do the correct thing in the presence of links. If
 it does not find FILE, then it shall return nil"
  (let* (tramp-mode                     ; don't invoke ssh...
         (d (expand-file-name default-directory)))
    (while (not (or (string-match-p "^\\([a-z]:\\)?/$" d)
                    (file-exists-p (expand-file-name file d))))
      (setq d (expand-file-name ".." d)))
    (if (member d '("c:/" "/"))
        nil
      (expand-file-name file d))))

(defvar compile-makefile nil "makefile of last compilation")

(defun compile-with-makefile (arg)
  "Search for a Makefile upwards from the current directory. If
 one is found, `compile' the program with it."
  (interactive "P") 
  ;; mark "In file included from" clang output as info instead of warning
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  (let ((makefile (or (get-closest-pathname "Makefile") compile-makefile)))
    (if (null makefile)
        (call-interactively 'compile)
      (setq compile-makefile makefile)
      (let* ((default-directory (file-name-directory makefile))
             (command (read-string (format "Compile command (in %s): "
                                          (abbreviate-file-name default-directory))
                                   compile-command nil)))
        (compile command (and arg t))))))

(defvar run-program-command "make -k run")
(defun run-program-with-makefile (arg)
  (interactive "P")
  (let* ((makefile (get-closest-pathname "Makefile"))
         (dir (file-name-directory makefile)))
    (if (null makefile)
        (message "no Makefile found")
      (setq run-program-command
            (read-string (format "Compile command (in %s): "
                                 (abbreviate-file-name dir))
                         run-program-command nil))
      (let ((default-directory dir))
        (compile run-program-command (and arg t))))))


;; per-project settings
(defun enter-project ()
  (interactive)
  (let ((settings (get-closest-pathname ".emacs")))
    (unless (or (null settings) (equal settings "/home/arthur"))
      (load (concat settings ".emacs")))))


;; retchmail

(defun retchmail-sentinel (proc stat)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (if (re-search-forward "\\([0-9]+\\) message" nil t)
        (let ((messages (string-to-number (match-string 1))))
          (if (zerop messages)
              (message "Checking for mail... no mail")
            (message
             (substitute-command-keys
              "Checking for mail... downloaded %d messages (\\[mew] to view)")
             messages)
            ))
      (message "Checking for mail... error")))
  (kill-buffer (process-buffer proc)))

(defun retchmail ()
  "Run retchmail to check for new mail"
  (interactive)
  (let ((proc (start-process "retchmail" "*Retchmail*" "retchmail")))
    (set-process-sentinel proc 'retchmail-sentinel)
    (message "Checking for mail...")))


(defun browse-toggle-w3m/firefox ()
  (interactive)
  (setq browse-url-browser-function
        (if (eq browse-url-browser-function 'w3m-display-url)
            'browse-url-default-browser
          'w3m-display-url))
  (message "Browsing urls with: %s" browse-url-browser-function))


;; elisp

(defun byte-compile-this-file (arg)
  "Byte compile the current emacs lisp file.
With prefix ARG,also load it"
  (interactive "P")
  (require 'bytecomp)
  (when (or (file-exists-p (byte-compile-dest-file buffer-file-name))
            (yes-or-no-p (format "Byte compile %s? "
                                 (abbreviate-file-name buffer-file-name))))
    (byte-compile-file buffer-file-name arg)))

(defun eval-this-buffer ()
  (interactive)
  (eval-buffer nil t)
  (message "Evaluated buffer."))


(defun clock ()
  "Display the time and date in the mode line"
  (interactive)
  (message (format-time-string "It is %l:%M%p on %A, %B %e, %Y.")))

(defun range (begin &optional end step)
  "Return a list where the first element is BEGIN each element is
STEP greater than the element before it, so that no element is
greater than or equal to END. If only one argument is given, it
is taken as END. BEGIN defaults to 0 and STEP defaults to 1,
unless BEGIN is greather than END, in which case it defaults to
-1."
  (when (null end)
    (setq end begin
          begin 0))
  (when (null step)
    (setq step (if (> end begin) 1 -1)))
  (let (list
        (cmp (if (> step 0) '< '>)))
    (while (funcall cmp begin end)
      (setq list (cons begin list)
            begin (+ begin step)))
    (nreverse list)))


(defmacro let-alias (varlist &rest body)
  "Make temporary function aliases.
Neither SYMBOL nor DEFINITION need to be quoted, but they both
need to exist.
 See also `fset', `flet', 'defalias'. \(fn ((SYMBOL
DEFINITION) ..) BODY...)"
  (declare (indent 1))
  (let ((saved-names (mapcar (lambda (a) `(,(cl-gensym) ',(car a) ',(cadr a)))
                             varlist)))
    `(let ,(mapcar (lambda (a) `(,(car a) (symbol-function ,(cadr a))))
                   saved-names)
       (unwind-protect
           (progn
             ,@(mapcar (lambda (a) `(fset ,(cadr a) ,(caddr a)))
                       saved-names)
             ,@body)
         ,@(mapcar (lambda (a) `(fset ,(cadr a) ,(car a)))
                   saved-names)))))


(defun window-mapc (fun frame &optional window-split)
  "call FUN for each window in FRAME."
  (if window-split
      (let ((list (cddr window-split))
            win)
        (while list
          (setq win (car list)
                list (cdr list))
          (if (listp win)
              (window-mapc fun nil win)
            (funcall fun win))))
    (let* ((tree (window-tree frame))
           (root (car tree)))
      (if (listp root)
          (window-mapc fun nil root)
        (funcall fun root)))))

(defun isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp." 
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (if (not isearch-regexp)
      (isearch-toggle-regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol ()
  "Put symbol at current point into search string."
  (interactive)
  (let ((sym (find-tag-default)))
    (if (null sym)
        (message "No symbol at point")
      (isearch-yank-regexp
       (concat "\\_<" (regexp-quote sym) "\\_>")))))

(defun my-minibuffer-insert-thing-at-point (thing)
  "Get word at point in original buffer and insert it to minibuffer."
  (let (word)
    (with-current-buffer (window-buffer (minibuffer-selected-window))
      (setq word (substring-no-properties(thing-at-point thing))))
    (when word
      (insert word))))

(defun my-minibuffer-insert-word-at-point ()
    (interactive)
    (my-minibuffer-insert-thing-at-point 'word))

(defun my-minibuffer-insert-symbol-at-point ()
    (interactive)
    (my-minibuffer-insert-thing-at-point 'symbol))

(defun my-delete-indentation ()
  (interactive)
  (delete-indentation)
  (just-one-space))

(defun my-c-insert-braces ()
  "Insert matching braces, newlines, and reindent"
  (interactive)
  (unless (eq (char-before) ? )
    (insert ? ))
  (insert ?{)
  (indent-according-to-mode)
  (newline)
  (insert ?})
  (backward-char)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (move-end-of-line nil)
  (indent-according-to-mode))

(defun my-c-insert-arrow1 ()
  (cond ((eq (char-after) ?.) (progn (delete-char 1)
                                     (insert "->") t))
        ((eq (char-before) ?.) (progn (delete-char -1)
                                      (insert "->") t))
        ((and (eq (char-before) ?-) (eq (char-after) ?>))
         (delete-char -1)
         (delete-char 1)
         (insert ".") t)
        ((looking-at-p "->") (progn (delete-char 2)
                                    (insert ".") t))))

(defun my-c-insert-arrow()
  "Toggle -> and . around point"
  (interactive)
  (or (my-c-insert-arrow1)
      (save-excursion
        (and (re-search-forward "\\([.]\\|->\\)" (line-end-position) t)
             (goto-char (match-beginning 0))
             (my-c-insert-arrow1)))
      (save-excursion
        (and (re-search-backward "\\([.]\\|->\\)" (line-beginning-position) t)
             (goto-char (match-beginning 0))
             (my-c-insert-arrow1)))
      (insert "->")))

(defun my-c++-transpose-sexps (arg)
  (interactive "*p")
  (transpose-subr 'forward-sexp arg))

(defun my-c++-backward-up-list (&optional arg)
  (interactive "^p")
  (let ((forward-sexp-function nil))
    (up-list (- (or arg 1)))))
  
(defsubst my-c++-skip-syntax (chars count)
  "call `skip-syntax-forward' if COUNT is positive, else  `skip-syntax-backward'"
  (if (> count 0)
      (skip-syntax-forward chars)
    (skip-syntax-backward chars)))

(defsubst my-c++-skip-chars (chars count)
  "call `skip-chars-forward' if COUNT is positive, else  `skip-chars-backward'"
  (if (> count 0)
      (skip-chars-forward chars)
    (skip-chars-backward chars)))

(defun my-c++-forward-sexp (count)
  "Treat things like foo.bar->baz() as a single expression!!"
  ;; FIXME only skip > when preceeded by -
  (let ((skipc "a-zA-Z_0-9\->.:") ;&*")
        (popen "[{[(<\"']")
        (pclose "[]})>\"']"))
    (if nil;; (or (and (> count 0) (looking-at-p popen))
        ;;     (and (< count 0) (string-match-p pclose (char-to-string (char-before)))))
        (goto-char (scan-sexps (point) count))
      (my-c++-skip-syntax " <>!." count)
      ;; FIXME we really want & and * to be only skipped when prefix...
      (while
          (/= 0 (progn
                  (my-c++-skip-chars skipc count)
                  (if (string-match-p (if (> count 0) popen pclose) 
                                      (char-to-string (if (> count 0) (char-after) (char-before))))
                      (goto-char (or (scan-sexps (point) count) (buffer-end count))))
                  (my-c++-skip-chars skipc count))))
      (if (< count 0)
          (backward-prefix-chars)
        (skip-syntax-backward ".")))))


(provide 'arthur-functions)
