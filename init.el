;; -*- mode: emacs-lisp -*-
;; Arthur Danskin <arthurdanskin@gmail.com>
;; .emacs for GNU emacs 24.3

(eval-when-compile
  (require 'grep)
  (require 'shell)
  (require 'cc-mode))

;; os
(defvar ispell-program-name)
(defvar vc-hg-program)
(eval-after-load 'vc-hooks '(progn
	(remove-hook 'find-file-hook 'vc-find-file-hook)
	(remove-hook 'find-file-hook 'vc-refresh-state)))
(declare-function grep-apply-setting "grep")

(cond
 ((eq system-type 'windows-nt)
  (let* ((cygroot (if (file-directory-p "C:/cygwin64") 
                      "C:/cygwin64/" "C:/cygwin/"))
         (cygbin (concat cygroot "bin")))
    (setq shell-file-name (concat cygbin "/bash.exe")
          vc-hg-program (concat cygbin "/hg") 
          ispell-program-name (concat cygbin "/aspell.exe")
          explicit-shell-file-name shell-file-name
          explicit-bash-args '("--login" "-i"))
    (add-to-list 'exec-path cygbin)
    (add-to-list 'Info-default-directory-list (concat cygroot "usr/info/"))
    (setenv "SHELL" shell-file-name)
    (setenv "PATH" (concat (getenv "PATH") ";" (replace-regexp-in-string "/" "\\\\" cygbin)))
    (setq myfont "Consolas-11")
    (set-fontset-font "fontset-default" nil (font-spec :name "FreeMono"))
    ))
 ((eq system-type 'cygwin)
  (require 'grep)
  (grep-apply-setting
   'grep-find-command '("/usr/bin/find . -type f -exec grep -n  {} /dev/null \\;" . 30))
  (setq myfont "Consolas-11"))
 ((eq system-type 'darwin)
  ;; (setq myfont "Dejavu Sans Mono-9")
  (set-frame-font (setq myfont "SF Mono-12"))
  ;; macports directory
  (add-to-list 'exec-path "/opt/local/bin")
  (setenv "PATH" (concat (getenv "PATH") ":" "/opt/local/bin"))
  (setq-default
   explicit-bash-args '("--login" "-i")
   ispell-program-name "/opt/local/bin/aspell"
   vc-hg-program "/opt/local/bin/hg"
   find-function-C-source-directory (expand-file-name "~/Documents/emacs/emacs-24.3.91/src")
   latex-run-command (executable-find "latex"))
  (with-no-warnings
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'super))
  ;; (unless (frame-parameter nil 'fullscreen)
  ;;   (set-frame-parameter nil 'fullscreen 'maximized))
  )
 (t ; linux
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
  (setq ispell-program-name "/usr/bin/aspell")
  ))

; stop Fing up my history
(defvar desktop-globals-to-save)
(setq desktop-globals-to-save
      '(tags-file-name tags-table-list))

(require 'arthur-functions)
(require 'arthur-autoload)
(require 'anisoptera)
(require 'generic-x)

(global-pretty-mode 1)

(when (require 'package nil t)
  (package-initialize)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)

  (unless (require 'auto-complete nil t)
    (package-refresh-contents)
    (dolist (el '(auto-complete color-theme lua-mode parenface hexrgb))
      (package-install el))))

(when (require 'auto-complete nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
  (require 'auto-complete-config)

  (defvar c++-ebrowse-source-table nil "Cached ebrowsed completion table")
  ;; (ac-define-source c++-ebrowse
  ;; 		    '((candidates
  ;; 		       . (lambda ()
  ;; 			   (unless c++-ebrowse-source-table
  ;; 			     (setq c++-ebrowse-source-table (ebrowse-some-member-table)))
  ;; 			   (all-completions ac-prefix c++-ebrowse-source-table)))
  ;; 		      (requires . 0)
  ;; 		      (symbol . "c++")))

  ;; (ac-define-source words-in-buffer
  ;; 		    '((init . ac-update-word-index)
  ;; 		      (candidates . (ac-word-candidates
  ;; 				     (lambda (buffer)
  ;; 				       (eq buffer (current-buffer)))))))

  (setq-default ac-sources '(ac-source-imenu
  			     ac-source-abbrev
  			     ;;ac-source-words-in-buffer
  			     ac-source-words-in-same-mode-buffers
  			     ))
  ;;(ac-set-trigger-key (kbd "TAB"))
  (ac-set-trigger-key nil)
  (global-auto-complete-mode t))

(global-auto-revert-mode 1)
(add-to-list 'global-auto-revert-ignore-modes 'ebrowse-tree-mode)
;(add-to-list 'global-auto-revert-ignore-modes 'tags-table-mode)
;(ac-config-default)
;(yas-global-mode 1)

;; allow c:/ paths on cygwin (load AFTER package-init)
(when (eq system-type 'cygwin)
  (require 'windows-path)
  (windows-path-activate))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun my-really-kill-emacs ()
  (yes-or-no-p "Really kill Emacs? "))

;; window system
(when window-system
  (add-to-list 'kill-emacs-query-functions 'my-really-kill-emacs)
  (ignore-errors
    (when (and (require 'color-theme nil t)
               (require 'arthur-theme))
      (unless arthur-current-theme
        (color-theme-arthur-dark2))
      ;; (let ((color-theme-legal-variables "\\(color\\|face\\)")
      ;;       (hour (string-to-number (format-time-string "%H"))))
      ;;   (if (and (< 8 hour ) (< hour 21))
      ;;       (color-theme-arthur-light)
      ;;     (color-theme-arthur-dark))
      ;;   )
      ))
  
  (blink-cursor-mode 0)
  (if (featurep 'tool-bar) (tool-bar-mode -1))
  (if (featurep 'scroll-bar) (scroll-bar-mode -1))
  (if (featurep 'tooltip) (tooltip-mode -1))

  (when (not (eq system-type 'darwin))
    (ignore-errors
      (set-frame-font myfont)
      (add-to-list 'default-frame-alist (cons 'font myfont)))
    (if (featurep 'menu-bar) (menu-bar-mode -1))
    )

  (setq-default
   select-active-regions t
   x-select-enable-primary t
   x-select-enable-clipboard t)

  (setq initial-frame-alist
        '((vertical-scroll-bars)
          (width . 160)
          (height . 60)))
  )

;; variables
(setq-default
 inhibit-splash-screen t
;;  initial-scratch-message (concat ";; " (shell-command-to-string "fortune")
;;;                                  "\n")
 require-final-newline t
 search-highlight t
 frame-title-format '(multiply-frames "%b" ("%b - Emacs"))

 indent-tabs-mode nil                    ; no tabs!
 vc-follow-symlinks t
 default-indicate-buffer-boundaries 'left
 default-indicate-empty-lines t  ; indicate end of file in fringe
 iswitchb-prompt-newbuffer nil   ; don't prompt to create a new buffer
 mouse-autoselect-window t       ; focus follows mouse!
 focus-follows-mouse nil         ; behavior of window manager
 describe-char-unidata-list '(name general-category
                                   digit-value numeric-value)
 save-place t                           ; remember my place in files
 show-paren-delay 0.0
 sentence-end-double-space nil
 kill-read-only-ok t
 kill-whole-line t
 eval-expression-debug-on-error t
 truncate-partial-width-windows t       ; don't wrap lines in split windows
 enable-recursive-minibuffers t
 ;; print-level 6
 ;; print-length 24
 print-level nil
 print-length nil ; setting this lower breaks savehist!!
 tags-case-fold-search t
 tags-revert-without-query t
 use-dialog-box nil                       ; no gui
 compilation-read-command t
 ;; compilation-scroll-output 'first-error
 compilation-scroll-output nil
 undo-limit 20000000                     ; I have a lot of memory
 find-file-confirm-nonexistent-file t
 find-file-visit-truename nil           ; don't dereference symlinks
 disabled-command-function nil          ; enable all commands
 history-delete-duplicates t
 history-length 10000
 max-specpdl-size 10000
 max-lisp-eval-depth 10000
 backup-directory-alist '(("." . "~/.emacs-backups"))
 Man-notify-method 'pushy
 fill-column 97
 comment-fill-column 132
 comment-column 32
 ring-bell-function (lambda () nil); suppress annoying beeps
 switch-to-buffer-preserve-window-point 'already-displayed
 imenu-auto-rescan-maxout 120000
 split-width-threshold 140
 split-height-threshold 100
 frame-resize-pixelwise t
 )

;(add-to-list 'warning-suppress-types '(undo discard-info))q
;(add-to-list 'warning-suppress-types 'frameset)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'saveplace)
(require 'paren)                    ; parentheses matching like in vim
(show-paren-mode 1)
;; (require 'autopair)
;; (autopair-global-mode)                  ; auto insert closing parentheses
;; (global-set-key (kbd "\"") 'self-insert-command)

;; (defadvice autopair-pair-p (around autopair-no-fucking-paren-unless-space)
;;   (let ((char (following-char)))
;;     (or (eq char nil) (memq (char-syntax char) (list ?\) ?  ?> ?<)))))

;; (defadvice autopair-insert-or-skip-quote (around fucking-quotes)
;;   (self-insert-command 1))

;; (ad-activate 'autopair-pair-p)
;; (ad-activate 'autopair-insert-or-skip-quote)


(transient-mark-mode 1)
(temp-buffer-resize-mode)      ; make temp buffers small
(winner-mode 1)		       ; C-c left/right to undo window changes
(line-number-mode 1)
(which-function-mode 1)
(column-number-mode 1)
(auto-compression-mode 1)
(savehist-mode 1)
(global-pretty-mode 1)

;; (server-start)
;; (setq server-window 'pop-to-buffer)

(add-hook  'after-save-hook             ;  make scripts executable
           'executable-make-buffer-file-executable-if-script-p)
;; (add-hook 'kill-emacs-query-functions (lambda () (yes-or-no-p "Really kill emacs? ")))

;;  keybindings

(windmove-default-keybindings 'super)
(global-set-key (kbd "s-p") `windmove-up)
(global-set-key (kbd "s-n") `windmove-down)
(global-set-key (kbd "s-f") `windmove-right)
(global-set-key (kbd "s-b") `windmove-left)

(global-set-key [(Mouse-2)] 'mouse-yank-primary)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)
(global-set-key (kbd "C-x C-/") 'winner-undo)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(define-key global-map [remap find-file] 'find-file-at-point)
(define-key global-map [remap find-file-read-only] 'ffap-read-only)
(define-key global-map [remap find-file-other-window] 'ffap-other-window)
(define-key global-map [remap find-file-other-window] 'ffap-read-only-other-window)
(global-set-key (kbd "C-x w") 'compare-windows)
(global-set-key (kbd "C-x C-g") 'keyboard-quit) 
(global-set-key (kbd "C-/") 'undo)
(global-set-key (kbd "C-S-s") 'rgrep)
(global-set-key (kbd "s-F") 'rgrep)
(global-set-key (kbd "s-?") 'rgrep-defaults)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "C-S-f") 'rgrep-defaults)
(define-key isearch-mode-map (kbd "M-w") 'isearch-toggle-word)
(define-key isearch-mode-map (kbd "C-M-w") 'isearch-yank-symbol)

(defun my-minibuffer-setup-hook ()
  (local-set-key (kbd "C-w") 'my-minibuffer-insert-word-at-point)
  (local-set-key (kbd "C-M-w") 'my-minibuffer-insert-symbol-at-point))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(global-set-key (kbd "M-/") 'hippie-expand)
;; (global-set-key (kbd "M-,") 'hippie-expand-line)
(global-set-key (kbd "M-,") 'xref-pop-marker-stack)
(global-set-key (kbd "M-*") 'xref-pop-marker-stack)
(ignore-errors
  (hippie-help-mode 1))
(global-set-key (kbd "C-,") 'imenu)
(global-set-key (kbd "<M-backspace>") 'subword-backward-delete)
;; (global-set-key (kbd "<C-backspace>") 'my-delete-indentation)
(global-set-key (kbd "<C-M-backspace>") 'my-delete-indentation)
(global-set-key (kbd "<f12>") 'hippie-help)
(define-key read-expression-map (kbd "TAB") 'completion-at-point)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x E") 'find-user-init-file)
(global-set-key (kbd "C-x D") 'toggle-debug)
(global-set-key (kbd "C-x N") 'print-buffer-file)
(global-set-key (kbd "C-x S") 'shell)
(global-set-key (kbd "C-x g") 'browse-url)
(global-set-key (kbd "C-x B") 'quit-window)
(global-set-key (kbd "C-.") 'repeat)
(require 'misc)
(define-key global-map [remap zap-to-char] 'zap-up-to-char)
(global-set-key (kbd "C-;") 'comment-or-uncomment-line)
(global-set-key (kbd "C-M-;") 'comment-sexp)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "<C-return>") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)
(define-key ctl-x-4-map (kbd "t") 'toggle-window-split)
(define-key ctl-x-4-map (kbd "r") 'transpose-windows)
(global-set-key (kbd "C-x C-o") 'other-frame)

(global-set-key (kbd "C-(") 'insert-pair)
(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "C-\"") 'insert-pair)
(global-set-key (kbd "C-'") 'insert-pair)

(global-set-key [remap backward-kill-word] 'backward-delete-word)
(defun my-subword-hook ()
  (local-set-key (kbd "M-DEL") 'subword-backward-delete))
(add-hook 'subword-mode-hook 'my-subword-hook)

(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "s-m") 'toggle-frame-maximized)
(global-set-key (kbd "C-x c") 'clock)

(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

;; compiling

(defvar compilation-autohide-window nil)
(defun my-compilation-finish-function (buffer exit)
  (when (and compilation-autohide-window
             (equal exit "finished\n")
             (not (equal (buffer-name buffer) "*grep*")))
    (quit-window nil (get-buffer-window buffer))
    ;; (bury-buffer buffer)
    ;; (replace-buffer-in-windows buffer)
    ))
(add-hook 'compilation-finish-functions 'my-compilation-finish-function)
(setq compilation-error-regexp-alist '(ada aix bash python-tracebacks-and-caml comma msft gcc-include gnu))

;; this controls default file pattern for rgrep
(setq grep-files-aliases
  '(("all" .   "* .*")
    ("el" .    "*.el")
    ("c" .     "*.c *.cpp *.h *.hpp *.inc *.inl *.m *.mm")
    ("m" .     "[Mm]akefile*")
    ("tex" .   "*.tex")
    ("texi" .  "*.texi")
    ("asm" .   "*.[sS]")))

(setq grep-find-ignored-files nil)


;; (global-set-key (kbd "<f8>") 'next-error) ; visual studio
(global-set-key (kbd "s-'") 'next-error)  ; xcode 
(global-set-key (kbd "s-\"") 'previous-error)  ; xcode 
(global-set-key (kbd "M-'") 'next-error)
(global-set-key (kbd "M-\"") 'previous-error)
(defun my-compile-keys ()
  ;; (local-set-key (kbd "s-b") 'compile-with-makefile)
  (local-set-key (kbd "C-c C-c") 'compile-with-makefile))
(add-hooks '(sh-mode-hook makefile-mode-hook c-mode-common-hook python-mode-hook)
           'my-compile-keys)


;; hippie-expand
;; TODO tags? 
(setq-default hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-line
        try-expand-dabbrev-all-buffers
        try-expand-line-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name))

(defun hippie-expand-line ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line
           try-expand-line-all-buffers
           try-complete-file-name-partially
           try-complete-file-name
           try-expand-whole-kill)))
    (hippie-expand nil)))


;;; language / major modes

(add-to-list 'magic-mode-alist '("^ELF" . hexl-mode))
(add-to-list 'auto-mode-alist '("\.o\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\.vdf\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\.po\\'" . default-generic-mode))

;; minor modes

(defun toggle-partial-truncate (&optional arg)
  (interactive)
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows
        (cond
         ((not arg) (not truncate-partial-width-windows))
         ((> 0 arg) t)
         ((<= 0 arg) nil)))
  (message "Partial width windows: %s" (if truncate-partial-width-windows "Enabled" "Disabled")))

(defun my-disable-partial-truncate ()
  (toggle-partial-truncate -1))

(add-hooks '(compilation-mode-hook nxml-mode-hook) 'my-disable-partial-truncate)
(add-hook 'markdown-mode-hook 'visual-line-mode)

(defun my-temp-buffer-hook ()
  (local-set-key (kbd "n") 'my-anisoptera-isearch-point)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "k") 'kill-this-buffer))
(add-hooks '(compilation-mode-hook completion-list-mode-hook)
           'my-temp-buffer-hook)

;; diary and calendar
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)


;; buffer switching
(setq-default ido-use-filename-at-point t
              ido-decorations '("(" ")" ", " ", ..." "[" "]"
                                " [No match]" " [Matched]" " [Not readable]"
                                " [Too big]")
              ido-max-prospects 32
              ido-save-directory-list-file nil
              ido-default-buffer-method 'selected-window)
(ido-mode 'buffers)

(defun dont-munge-buffer-order-damnit ()
  (eval-when-compile
    (defvar ido-default-item)
    (defvar ido-temp-list))
  (let ((curname (buffer-name (current-buffer))))
    (setq ido-temp-list
          (sort (mapcar 'buffer-name (buffer-list (selected-frame)))
                (lambda (a b) (or (equal b curname) (string-match "^ " b)))))
    (if ido-default-item
        (setq ido-temp-list
              (cons ido-default-item (delete ido-default-item ido-temp-list))))))
(add-hook 'ido-make-buffer-list-hook 'dont-munge-buffer-order-damnit)

(defun other-window-prev ()
  (interactive)
  (other-window -1))

;; (global-set-key (kbd "<M-tab>") (lambda () 
;;                                   (interactive) 
;;                                   (switch-to-buffer (other-buffer (current-buffer)) nil t)))
;(global-set-key (kbd "<M-S-tab>") 'previous-buffer)
(global-set-key (kbd "<M-tab>") 'ido-switch-buffer)
(define-key ido-common-completion-map (kbd "<M-tab>") 'ido-next-match)
(define-key ido-common-completion-map (kbd "<M-S-tab>") 'ido-prev-match)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") 'other-window-prev)
(global-set-key (kbd "<C-M-tab>")
                (lambda ()
                  "Make current buffer visible in another window, and switch to that window"
                  (interactive)
                  (pop-to-buffer (current-buffer) t)))


;; elisp

(defun my-elisp-hook ()
  (font-lock-add-keywords
   nil `((,(concat "(" (regexp-opt '("and" "or" "setq" "setq-default") 'words))
          (1 font-lock-keyword-face))) t)
  (eldoc-mode 1)
  (local-set-key (kbd "C-c C-c") 'byte-compile-this-file)
  (local-set-key (kbd "C-c C-e") 'eval-this-buffer)
  (local-set-key (kbd "C-c p p") 'elp-instrument-function)
  (local-set-key (kbd "C-c p a") 'elp-instrument-package)
  (local-set-key (kbd "C-c p r") 'elp-results)
  (setq eval-expression-print-length 100)
  (setq eval-expression-print-level 10)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially t)
  (local-set-key (kbd "C-M-j") 'eval-print-last-sexp)
  (local-set-key (kbd "C-j") 'newline))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-hook)


(defun my-help-hook ()
  (local-set-key [backspace] 'help-go-back)
  (local-set-key (kbd "l") 'help-go-back)
  (local-set-key (kbd "k") 'kill-this-buffer))
(add-hook 'help-mode-hook 'my-help-hook)

;; javascript
(setq js2-use-font-lock-faces t)

;; gdb

;; (defun my-gdb-hook ()
  ;; (set (make-local-variable 'truncate-partial-width-windows) nil))
;; (add-hook 'gud-mode-hook 'my-gdb-hook)

(defun my-lua-hook ()
  (font-lock-add-keywords nil '(("#.+" . font-lock-comment-face)))
  (local-set-key (kbd "C-c C-l") 'align-dwim)
  (local-set-key (kbd "C-{") 'my-c-insert-braces)
  (hexcolor-mode 1)
  (subword-mode 1)
  (setq tab-width 4))
(add-hook 'lua-mode-hook 'my-lua-hook)

;; c / c++

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . c++-mode))
(add-to-list 'completion-ignored-extensions ".dep")

(define-derived-mode gamemonkey-mode javascript-mode "GM"
  (font-lock-add-keywords
   nil `((,(concat (regexp-opt '("local" "global" "member") 'symbols)
                   " *\\([a-zA-Z_0-9.]*\\)")
          (1 'font-lock-type-face)
          (2 'font-lock-variable-name-face))
         ("\\(\\_<fork\\_>\\) *\\([a-zA-Z_0-9.]*\\)"
          (1 'font-lock-keyword-face)
          (2 'font-lock-variable-name-face))
         ("\\(\\_<foreach\\_>\\) *( *\\([a-zA-Z0-9_]*\\) *\\(and\\) *\\([a-zA-Z0-9_]*\\) *\\(\\<in\\>\\)"
          (1 font-lock-keyword-face)
          (2 font-lock-variable-name-face)
          (3 font-lock-keyword-face)
          (4 font-lock-variable-name-face)
          (5 font-lock-keyword-face))
         ("\\(\\_<foreach\\_>\\) *( *\\([a-zA-Z0-9_]*\\) *\\(\\<in\\>\\)"
          (1 font-lock-keyword-face)
          (2 font-lock-variable-name-face)
          (3 font-lock-keyword-face))
         (,(regexp-opt '("assert" "yield" "sleep" "exit" "debug" "select"
                         "threadKill" "threadKillAll" "threadTime" "threadId"
                         "threadAllIds" "signal" "block"
                         "stateSet" "stateGet" "stateGetLast" "stateSetExitFunction"
                         "sysTime" "doString" "typeId" "typeName"
                         "typeRegisterOperator" "typeRegisterVariable"
                         "sysCollectGarbage"
                         "tableCount" "tableDuplicate" "tableClear" "tableFirst"
                         "tableAtIndex" "print" "format"
                         "v2" "v3" "v2i" "v3i" "table" "globals")
                       'symbols) . font-lock-builtin-face)
         ("\\(!\\)[^=]" 1 font-lock-negation-char-face))))

(defun my-scripting-hook ()
  (local-set-key (kbd "C-c C-l") 'align-dwim)
  (local-set-key (kbd "C-{") 'my-c-insert-braces)
  ;; (hexcolor-mode 1)
  (subword-mode 1)
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(add-hook 'gamemonkey-mode-hook 'my-scripting-hook)

(add-to-list 'auto-mode-alist '("\\.gm\\'" . gamemonkey-mode))
(add-to-list 'auto-mode-alist '("\\.fp\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vp\\'" . glsl-mode))


(define-derived-mode squirrel-mode javascript-mode "Sq"
  (font-lock-add-keywords
   nil `(("\\(\\<local\\>\\) *\\([a-zA-Z_0-9.]*\\)"
          (1 'font-lock-type-face)
          (2 'font-lock-variable-name-face))
         ("\\(\\<foreach\\>\\) *( *\\([a-zA-Z0-9_]*\\) *, *\\([a-zA-Z0-9_]*\\) *\\(\\<in\\>\\)"
          (1 font-lock-keyword-face)
          (2 font-lock-variable-name-face)
          (3 font-lock-keyword-face)
          (4 font-lock-variable-name-face)
          (5 font-lock-keyword-face))
         ("\\(\\<foreach\\>\\) *( *\\([a-zA-Z0-9_]*\\) *\\(\\<in\\>\\)"
          (1 font-lock-keyword-face)
          (2 font-lock-variable-name-face)
          (3 font-lock-keyword-face))
         (,(regexp-opt (list "base" "catch" "class" "clone"
                             "continue" "const" "default" "delete" "enum"
                             "extends" "in" "null" "resume"
                             "throw" "try" "typeof" "yield" "constructor"
                             "instanceof" "static")
                       'symbols) . font-lock-keyword-face)
         ("\\(!\\)[^=]" 1 font-lock-negation-char-face))))

(add-hook 'squirrel-mode-hook 'my-scripting-hook)
(add-to-list 'auto-mode-alist '("\\.nut\\'" . squirrel-mode))

(setq cc-other-file-alist
      '(("\\.c\\'" (".h"))
        ("\\.m\\'" (".h"))
        ("\\.h\\'" (".c" ".cpp" ".m"))
        ("\\.cpp\\'" (".hpp"".h"))
        ("\\.hpp\\'" (".cpp"))))
(defun my-c-common-hook ()
  (c-set-style "stroustrup")
  (c-set-offset 'statement-cont '(c-lineup-assignments +))
  (c-set-offset 'inline-open 0)
  (c-set-offset 'inextern-lang 0)
  (c-set-offset 'innamespace 0)
  (local-set-key [remap newline-and-indent] 'c-context-line-break)
  (local-set-key (kbd "C-c o") 'ff-get-other-file)
  (local-set-key (kbd "TAB") 'c-indent-line-or-region)
  (local-set-key (kbd "C-c C-l") 'align-dwim)
  (imenu-add-menubar-index)
  (hexcolor-mode 1)
  (subword-mode 1)
  (setq tab-width 4)
  (when (and buffer-file-name (string-match-p "FunkEngin" buffer-file-name))
    (setq indent-tabs-mode t))
  (make-local-variable 'forward-sexp-function)
  (setq forward-sexp-function 'my-c++-forward-sexp)
  (local-set-key [remap backward-up-list] 'my-c++-backward-up-list)
  (local-set-key [remap transpose-sexps] 'my-c++-transpose-sexps)
  (local-set-key (kbd "C-c C-k") 'my-c++-kill-decl)
  (hippie-eldoc 1)
  (setq parens-require-spaces nil)
  (local-set-key (kbd "C-{") 'my-c-insert-braces)
  ;; (local-set-key (kbd "C-<") 'insert-pair)
  ;; (local-set-key (kbd "C->") 'my-c-insert-arrow)
  (abbrev-mode -1)
  (local-set-key (kbd "C-j") 'newline-and-indent)

  ;; (setq cpp-known-face 'default)
  ;; (setq cpp-unknown-face 'default)
  ;; (setq cpp-known-writable 't)
  ;; (setq cpp-unknown-writable 't)
  ;; (setq cpp-edit-list '(("0" font-lock-comment-face default both)
  ;;                       ("1" default font-lock-comment-face both)))
  ;; (cpp-highlight-buffer t)
  )
(add-hook 'c-mode-common-hook 'my-c-common-hook)

(defun my-c++-hook ()
  (font-lock-add-keywords
   nil `(
         (,(regexp-opt
            (list 
             ;; C++11 keywords
             "alignof" "alignas" "constexpr" "decltype" 
             "noexcept" "nullptr" "static_assert" "thread_local"
             "override" "final"
             ;; things xcode thinks are keywords (objective-c keywords)
             "in" "out" "inout" "oneway" "export";; "self"
             "super"
             ;; things I use
             "foreach" "for_" "unless" ;; "lambda"
             )
            'symbols) . font-lock-keyword-face)
         (,(regexp-opt
            (list "float2" "float3" "float4" "vec2" "vec3" "vec4" "mat2" "mat3" "mat4"
                  "dmat2" "dmat3" "dmat4"
                  "double2" "double3" "double4" "f2" "f3" "f4" "d2" "d3" "d4" "i2" "i3" "i4"
                  "uchar" "ushort" "uint" "uint64" "trit" "lstring" "int2" "int3" "int4"
                  "id") 'symbols) . font-lock-type-face)
         (,(regexp-opt
            (list "nil" "YES" "NO" "epsilon") 'symbols). font-lock-constant-face)
         ;("~" (0 font-lock-negation-char-face prepend))
         ("\\_<0[xX][0-9a-fA-f]+\\_>" . font-lock-constant-face) ; hex
         (;"\\(\\(\\_<\\|[.]\\)[0-9]+\\([eE][+-]?[0-9.]+\\)?\\)\\([.]?[lfLFuU]?\\)\\_>"
          "\\(\\([.]+\\|\\_<\\)[0-9]+\\([eE][+-]?[0-9.]+\\)?\\)\\([.]?[lfLFuU]?\\)\\_>"
          (1 font-lock-constant-face) (4 font-lock-comment-face)) ; dec floats and ints
         ("\\_<\\([A-Z_][A-Z_0-9][A-Z_0-9]+\\)\\(f?\\)\\_>[^(]"
          (1 font-lock-constant-face) (2 font-lock-comment-face)) ; preprocessor constants
         ("\\_<_[A-Za-z_][a-zA-Z_0-9]*\\_>" . font-lock-constant-face) ; preprocessor constants beginning with underscore
         ))
  (when (and (featurep 'semantic) semantic-mode)
    (add-to-list 'ac-sources 'ac-source-semantic))
  (let ((path (get-closest-pathname "BROWSE")))
    (when path
      (find-file-noselect path t)))
  (let ((path (get-closest-pathname "TAGS")))
    (when path
      (visit-tags-table path)))
  ;; (when (get-buffer "*Tree*")
  ;;   (add-to-list 'ac-sources 'ac-source-c++-ebrowse))
  )
(add-hook 'c++-mode-hook 'my-c++-hook)


(defun my-objc-hook ()
  (font-lock-add-keywords
   nil '(("@property" . font-lock-keyword-face))))
(add-hook 'objc-mode-hook 'my-objc-hook)

;; cedet
;; (progn
;;   (require 'semantic)
;;   (require 'semantic/sb)
;;   (global-ede-mode 1)
;;   (semantic-mode 1)
;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-scheduler-mode 1)
;;   (global-semantic-stickyfunc-mode 1))

;; asm

(defun my-asm-hook ()
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq comment-start "/* "
        comment-end   " */")
  (local-set-key (kbd ";") 'self-insert-command)
  (modify-syntax-entry ?\; "."))
(add-hook 'asm-mode-hook 'my-asm-hook)


;; python
(add-to-list 'auto-mode-alist '("SCons\\(truct\\|script\\)\\'" . python-mode))

(defun my-python-hook ()
  (font-lock-add-keywords
   nil '(("\\_<\\(True\\|False\\)\\_>" . font-lock-constant-face)
         ("\\_<self\\_>" . font-lock-variable-name-face)
;;;          ("\\_<print\\_>" . font-lock-builtin-face)
         ))
  (local-set-key (kbd "C-j") 'newline)
  (hippie-eldoc 1))
(add-hook 'python-mode-hook 'my-python-hook)


(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

