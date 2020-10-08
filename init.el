;; -*- mode: emacs-lisp -*-
;; Arthur Danskin <arthurdanskin@gmail.com>
;; .emacs for GNU emacs 24.3 - 27.1

(eval-when-compile
  (require 'grep)
  (require 'shell)
  (require 'cc-mode))

;; os
(defvar ispell-program-name)
(defvar vc-hg-program)
(with-eval-after-load 'vc-hooks
  ;; avoid hanging when opening files
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (remove-hook 'find-file-hook 'vc-refresh-state) )
  ;; avoid hanging when doing tramp stuff
(with-eval-after-load 'tramp
  (setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp)))
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
    ;; (setq myfont "Consolas-11")
    (setq myfont "PragmataPro Liga")
    (set-fontset-font "fontset-default" nil (font-spec :name "FreeMono"))
    ))
 ((eq system-type 'cygwin)
  (require 'grep)
  (grep-apply-setting
   'grep-find-command '("/usr/bin/find . -type f -exec grep -n  {} /dev/null \\;" . 30))
  (setq myfont "PragmataPro Liga")
  (ignore-errors (set-frame-font (setq myfont "PragmataPro-11")) t)

  ;; this function breaks etags by mysteriously  replacing : with !
  (defun convert-standard-filename (filename)
    filename)

  )
  ;; (setq myfont "Consolas-11"))
 ((eq system-type 'darwin)
  ;; (setq myfont "Dejavu Sans Mono-9")
  (or
   (ignore-errors (set-frame-font (setq myfont "PragmataPro-13")) t)
   (ignore-errors (set-frame-font (setq myfont "SF Mono-12")) t)
   (ignore-errors (set-frame-font (setq myfont "Menlo-12")) t))
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
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; (unless (frame-parameter nil 'fullscreen)
  ;;   (set-frame-parameter nil 'fullscreen 'maximized))
  )
 (t ; linux
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
  (setq ispell-program-name "/usr/bin/aspell")
  ))

(require 'arthur-functions)
(require 'arthur-autoload)
(require 'anisoptera)
(require 'generic-x)

(when (require 'package nil t)
  (when (< emacs-major-version 27)
    (package-initialize))
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)
  ;; (package-refresh-contents)
  (dolist (el '(auto-complete color-theme lua-mode ag unicode-fonts glsl-mode))
    (package-install el)))

(with-eval-after-load 'desktop
  ;; stop Fing up my history
  (setq desktop-globals-to-save (delq 'file-name-history desktop-globals-to-save))
  
  (defun my-desktop-read-hook ()
    (unicode-fonts-setup))
  (add-hook 'desktop-after-read-hook 'my-desktop-read-hook))

(require 'idle-highlight-in-visible-buffers-mode)
(idle-highlight-in-visible-buffers-mode t)
(global-pretty-mode 1)
(global-auto-revert-mode 1)
(add-to-list 'global-auto-revert-ignore-modes 'ebrowse-tree-mode)


;; allow c:/ paths on cygwin (load AFTER package-init)
(when (eq system-type 'cygwin)
  (require 'windows-path)
  (with-no-warnings (windows-path-activate)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun my-really-kill-emacs ()
  (yes-or-no-p "Really kill Emacs? "))

;; window system
(when window-system
  (add-to-list 'kill-emacs-query-functions 'my-really-kill-emacs)
  (setq-default color-theme-obsolete nil)
  (ignore-errors
    (when (and (require 'color-theme nil t)
               (require 'arthur-theme))
      (unless (eq arthur-current-theme 'dark2)
        (color-theme-arthur-dark2))))
  
  (blink-cursor-mode 0)
  (if (and (featurep 'tool-bar) tool-bar-mode) (tool-bar-mode -1))
  (if (featurep 'scroll-bar) (scroll-bar-mode -1))
  (if (featurep 'tooltip) (tooltip-mode -1))

  ;; (when (not (eq system-type 'darwin))
  (ignore-errors
    (set-frame-font myfont)
    (add-to-list 'default-frame-alist (cons 'font myfont)))
  (if (featurep 'menu-bar) (menu-bar-mode -1))
    ;; )

  (setq-default
   select-active-regions t
   x-select-enable-primary t
   x-select-enable-clipboard t)

  (setq initial-frame-alist
        '((vertical-scroll-bars)
          (left . 1)
          (top . 1)
          (width . 100)
          (height . 45)))
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
 vc-hg-diff-switches "-w"
 vc-hg-symbolic-revision-styles '("{rev}")
 default-indicate-buffer-boundaries 'left
 default-indicate-empty-lines t  ; indicate end of file in fringe
 iswitchb-prompt-newbuffer nil   ; don't prompt to create a new buffer
 mouse-autoselect-window t       ; focus follows mouse!
 focus-follows-mouse nil         ; behavior of window manager
 describe-char-unidata-list '(name general-category
                                   digit-value numeric-value)
 ;; save-place t                           ; remember my place in files
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
 tags-add-tables t                      ; stop asking me to add tags tables repeatedly
 use-dialog-box nil                       ; no gui
 compilation-read-command t
 ;; compilation-scroll-output 'first-error
 compilation-scroll-output nil
 undo-limit 20000000                     ; I have a lot of memory
 find-file-confirm-nonexistent-file t
 find-file-visit-truename nil           ; don't dereference symlinks
 disabled-command-function nil          ; enable all commands
 history-delete-duplicates t
 history-length 500
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
 split-width-threshold 160
 split-height-threshold 100
 frame-resize-pixelwise t
 eldoc-idle-delay 0.25
 idle-highlight-in-visible-buffers-idle-time 0.25
 dabbrev-case-fold-search nil
 gc-cons-threshold 1600000
 find-file-suppress-same-file-warnings t
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
(which-function-mode 0)
(column-number-mode 1)
(auto-compression-mode 1)
(savehist-mode 1)
(global-pretty-mode 1)
(when (functionp 'global-display-line-numbers-mode)
  (global-display-line-numbers-mode 1))

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
(global-set-key (kbd "C-S-f") 'ag-project)
(setq-default ag-highlight-search t
              ag-reuse-buffers t)
(defvar ag-arguments)
(with-eval-after-load 'ag
  (add-to-list 'ag-arguments "--search-zip"))
(define-key isearch-mode-map (kbd "M-w") 'isearch-toggle-word)
(define-key isearch-mode-map (kbd "C-M-w") 'isearch-yank-symbol)

;; unset scroll-left/right. I always hit these by mistake when trying to do C-c < in python mode
(global-unset-key (kbd "C-x <"))
(global-unset-key (kbd "C-x >"))

(defun my-minibuffer-setup-hook ()
  (local-set-key (kbd "C-w") 'my-minibuffer-insert-word-at-point)
  (local-set-key (kbd "C-M-w") 'my-minibuffer-insert-symbol-at-point))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(defun my-do-nothing () (interactive) nil)
(global-set-key [remap set-goal-column] 'my-do-nothing)
;; (global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
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

(with-eval-after-load 'hippie-exp
  (defvar dabbrev-case-fold-search)
  (defun my-he-dabbrev-search-wrapper (fun &rest args)
    (let ((case-fold-search dabbrev-case-fold-search))
      (apply fun args)))
  (advice-add 'he-dabbrev-search :around 'my-he-dabbrev-search-wrapper)
  )

;; compiling

(defvar compilation-autohide-window t)
(defun my-compilation-finish-function (buffer exit)
  (when (and compilation-autohide-window
             (equal exit "finished\n")
             (equal (buffer-name buffer) "*compilation*"))
    (quit-window nil (get-buffer-window buffer))
    ;; (bury-buffer buffer)
    ;; (replace-buffer-in-windows buffer)
    ) )

(with-eval-after-load 'compile

  (when (require 'xterm-color nil t)
    (setq compilation-environment '("TERM=xterm-256color"))
    (defun comint-term-environment () (list))
    (with-no-warnings
      (defun my/advice-compilation-filter (f proc string)
        (funcall f proc (xterm-color-filter string)))
      (advice-add 'compilation-filter :around #'my/advice-compilation-filter))
    )
  
  (add-hook 'compilation-finish-functions 'my-compilation-finish-function)

  ;; this regexp is for the new error message format in visual studio 2019 that includes the column number
  (setq compilation-error-regexp-alist-alist
        (cons '(arthur "^[ \t\n>0-9]*\\([^>\n]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) ?: \\(?:error\\|warnin\\(g\\)\\|messa\\(g\\)e\\)"
                1 2 4 (5 . 6))
              (assq-delete-all 'arthur compilation-error-regexp-alist-alist)))
  (setq compilation-error-regexp-alist-alist
        (cons '(arthur-edg-2 "at line \\([0-9]+\\) of \"\\([^\"\n]+\\)\"$" 2 1 nil 0)
              (assq-delete-all 'arthur-edg-2 compilation-error-regexp-alist-alist)))
  
  (setq compilation-error-regexp-alist '(arthur ada aix bash python-tracebacks-and-caml comma msft arthur-edg-2 gcc-include gnu))
  )
  

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

(defun my-mouse-goto (event)
  (interactive "e")
  (mouse-set-point event)
  (hippie-goto))

(global-set-key (kbd "<C-s-mouse-1>") 'my-mouse-goto)
(global-set-key (kbd "<C-s-left>") 'xref-pop-marker-stack)
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
         ((> arg 0) t)
         ((<= arg 0) nil)))
  (unless arg
    (message "Partial width windows: %s" (if truncate-partial-width-windows "Enabled" "Disabled"))))

(defun my-set-partial-truncate ()
  (toggle-partial-truncate -1))

(add-hook 'compilation-mode-hook 'my-set-partial-truncate)
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
    (defvar ido-temp-list)
    (defvar ido-default-item)
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

;; indent :plists properly
(setq-default lisp-indent-function 'common-lisp-indent-function)

;; avoid random judgy "warning" face
(advice-add 'lisp--match-hidden-arg :around (lambda (fun limit) nil))

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

(defun my-glsl-hook ()
  (font-lock-add-keywords nil '(("#include" . font-lock-preprocessor-face))))
(add-hook 'glsl-mode-hook 'my-glsl-hook)

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
        ("\\.h\\'" (".c" ".cpp" ".m" ".cc"))
        ("\\.cpp\\'" (".hpp"".h"))
        ("\\.cc\\'" (".hh"".h"))
        ("\\.hpp\\'" (".cpp" ".inl"))
        ("\\.inl\\'" (".hpp"))))
(defun my-c-common-hook ()
  (c-set-style "stroustrup")
  (c-set-offset 'statement-cont '(c-lineup-assignments +))
  (c-set-offset 'inline-open 0)
  (c-set-offset 'inextern-lang 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inlambda 0)
  (local-set-key [remap newline-and-indent] 'c-context-line-break)
  (local-set-key (kbd "C-c o") 'ff-get-other-file)
  (local-set-key (kbd "TAB") 'c-indent-line-or-region)
  (local-set-key (kbd "C-c C-l") 'align-dwim)
  (local-set-key (kbd "C-c n") 'my-renumber-list)
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

  (make-local-variable 'idle-highlight-in-visible-buffers-exceptions)
  (setq idle-highlight-in-visible-buffers-exceptions
        (append idle-highlight-in-visible-buffers-exceptions
                (list "if" "else" "while" "for" "do" "struct" "public" "private" "virtual"
                      "return" "inline" "const" "override" "static"
                      "switch" "case" "break" "continue" "void" "this" "true" "false")))
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
             "in" "out" "inout" "export";; "self"
             "super"
             ;; things I use
             "foreach" "for_" "unless" ;; "lambda"
             )
            'symbols) . font-lock-keyword-face)
         (,(regexp-opt
            (list "float2" "float3" "float4" "vec2" "vec3" "vec4" "mat2" "mat3" "mat4"
                  "dmat2" "dmat3" "dmat4"
                  "double2" "double3" "double4" "f2" "f3" "f4" "d2" "d3" "d4" "i2" "i3" "i4"
                  "uchar" "ushort" "uint" "uint64" "trit" "lstring" "int2" "int3" "int4")
            'symbols) . font-lock-type-face)
         (,(regexp-opt
            (list "nil" "YES" "NO" "epsilon") 'symbols). font-lock-constant-face)
         ;("~" (0 font-lock-negation-char-face prepend))
         ("\\_<\\(0[xX]\\)\\([0-9a-fA-f]+\\)\\([ulUL]*\\)\\_>"
          (1 font-lock-comment-face) (2 font-lock-constant-face) (3 font-lock-comment-face)) ; hex
         ("[^.]\\_<\\(0\\)\\([0-9]+[ulUL]*\\)\\_>" (1 font-lock-negation-char-face) (2 font-lock-constant-face)) ; octal
         (;"\\(\\(\\_<\\|[.]\\)[0-9]+\\([eE][+-]?[0-9.]+\\)?\\)\\([.]?[lfLFuU]?\\)\\_>"
          "\\(\\([.]+\\|\\_<\\)[0-9]+\\([eE][+-]?[0-9.]+\\)?\\)\\([.]?[lfLFuU]?\\)\\_>"
          (1 font-lock-constant-face) (4 font-lock-comment-face)) ; dec floats and ints
         ("\\_<\\([A-Z_][A-Z_0-9][A-Z_0-9]*\\)\\(f?\\)\\_>[^(]"
          (1 font-lock-constant-face) (2 font-lock-comment-face)) ; preprocessor constants
         ("\\_<_[A-Za-z_][a-zA-Z_0-9]*\\_>" . font-lock-constant-face) ; preprocessor constants beginning with underscore
         ))
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

(setq graphviz-dot-preview-extension "pdf"
      graphviz-dot-indent-width 4)

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


;; (setq twittering-icon-mode t
      ;; twittering-convert-fix-size 100)
