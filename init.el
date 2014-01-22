;; -*- mode: emacs-lisp -*-
;; Arthur Danskin <arthurdanskin@gmail.com>
;; .emacs for GNU emacs 23

(eval-when-compile
  (require 'shell)
  (require 'cc-mode))

;; os
(if (eq system-type 'windows-nt)
    (progn
      (setq shell-file-name "C:/cygwin/bin/bash.exe"
            ispell-program-name "c:/cygwin/bin/aspell.exe"
            explicit-shell-file-name shell-file-name
            explicit-bash-args '("--login" "-i"))
      (add-to-list 'exec-path "c:/cygwin/bin")
      (add-to-list 'load-path "d:/Arthur/life/configs/emacs.d")
      (add-to-list 'Info-default-directory-list "c:/cygwin/usr/info/")
      (setenv "SHELL" shell-file-name)
      (setenv "PATH" (concat (getenv "PATH") ";C:\\cygwin\\bin"))    
      (setq myfont "Dejavu Sans Mono-10")
      )
  (setq myfont "Dejavu Sans Mono-9")
  (add-to-list 'load-path "~/.emacs.d/")
  (if (eq system-type 'darwin)
      (progn
        ;; macports directory
        (add-to-list 'exec-path "/opt/local/bin")
        (setq-default ispell-program-name "/opt/local/bin/aspell"
                      vc-hg-program "/opt/local/bin/hg"
                      find-function-C-source-directory (expand-file-name "~/Documents/emacs-24.3/src")))

    ; linux
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")
    (setq ispell-program-name "/usr/bin/aspell")
    )
  )

(require 'arthur-functions)
(require 'arthur-autoload)

(require 'generic-x)

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)

(defvar c++-ebrowse-source-table nil "Cached ebrowsed completion table")
(ac-define-source c++-ebrowse
  '((candidates
     . (lambda ()
         (unless c++-ebrowse-source-table
           (setq c++-ebrowse-source-table (ebrowse-some-member-table)))
         (all-completions ac-prefix c++-ebrowse-source-table)))
    (requires . 0)
    (symbol . "c++")))

(ac-define-source words-in-buffer
  '((init . ac-update-word-index)
    (candidates . (ac-word-candidates
                   (lambda (buffer)
                     (eq buffer (current-buffer)))))))

(setq-default ac-sources '(ac-source-imenu
                           ac-source-abbrev
                           ;;ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ))
;(ac-set-trigger-key (kbd "TAB"))
(ac-set-trigger-key nil)
(global-auto-complete-mode t)
(global-auto-revert-mode 1)
(add-to-list 'global-auto-revert-ignore-modes 'ebrowse-tree-mode)
(add-to-list 'global-auto-revert-ignore-modes 'tags-table-mode)
(desktop-save-mode 1)
;(ac-config-default)
;(yas-global-mode 1)

(global-set-key (kbd "C-M-i") 'company-complete)

;; window system
(when window-system
  (ignore-errors
    (when (require 'color-theme nil t)
      (require 'arthur-theme)
      (require 'parenface)
      ;(color-theme-arthur-dark)
      (color-theme-arthur-light)
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
    (set-frame-font myfont)
    (add-to-list 'default-frame-alist (cons 'font myfont))
    (if (featurep 'menu-bar) (menu-bar-mode -1))
    )

  (setq-default
   select-active-regions t
   x-select-enable-primary t
   x-select-enable-clipboard t)
  )

;; variables
(setq-default
 inhibit-splash-screen t
;;  initial-scratch-message (concat ";; " (shell-command-to-string "fortune")
;;;                                  "\n")
 require-final-newline t
 search-highlight t
 frame-title-format '(multiply-frames
                      "%b" ("%b - " invocation-name "@" system-name))

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
 print-level 6
 print-length 24
 tags-case-fold-search t
 tags-revert-without-query t
 use-dialog-box nil                       ; no gui
 compilation-read-command t
 compilation-scroll-output 'first-error
 undo-limit 20000000                     ; I have a lot of memory
 find-file-confirm-nonexistent-file t
 find-file-visit-truename t             ; follow symlinks
 disabled-command-function nil            ; enable all commands
 history-delete-duplicates t
 history-length 1000
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
 )

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

;; (server-start)
;; (setq server-window 'pop-to-buffer)

(add-hook  'after-save-hook             ;  make scripts executable
           'executable-make-buffer-file-executable-if-script-p)
;; (add-hook 'kill-emacs-query-functions (lambda () (yes-or-no-p "Really kill emacs? ")))

;;  keybindings

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
(define-key isearch-mode-map (kbd "M-w") 'isearch-toggle-word)
(define-key isearch-mode-map (kbd "C-M-w") 'isearch-yank-symbol)

(defun my-minibuffer-setup-hook ()
  (local-set-key (kbd "C-w") 'my-minibuffer-insert-word-at-point)
  (local-set-key (kbd "C-M-w") 'my-minibuffer-insert-symbol-at-point))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-,") 'hippie-expand-line)
(hippie-help-mode 1)
(global-set-key (kbd "C-,") 'imenu)
(global-set-key (kbd "<C-M-backspace>") 'my-delete-indentation)
(global-set-key (kbd "<f12>") 'hippie-help)
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x E") 'find-user-init-file)
(global-set-key (kbd "C-x D") 'toggle-debug)
(global-set-key (kbd "C-x N") 'print-buffer-file)
(global-set-key (kbd "C-x S") 'shell)
(global-set-key (kbd "C-x g") 'browse-url)
(global-set-key (kbd "C-x G") 'browse-toggle-w3m/firefox)
(global-set-key (kbd "C-x B") 'quit-window)
(global-set-key (kbd "C-x M") 'retchmail)
(global-set-key (kbd "C-x M-m") 'mew)
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

(setq grep-files-aliases
  '(("all" .   "* .*")
    ("el" .    "*.el")
    ("c" .     "*.c *.cpp *.h *.hpp *.inc *.m *.mm")
    ("m" .     "[Mm]akefile*")
    ("tex" .   "*.tex")
    ("texi" .  "*.texi")
    ("asm" .   "*.[sS]")))

(setq grep-find-ignored-files nil)


(global-set-key (kbd "<f8>") 'next-error)
(global-set-key (kbd "s-'") 'next-error)
(global-set-key (kbd "M-'") 'next-error)
(global-set-key (kbd "M-\"") 'previous-error)
(defun my-compile-keys ()
  (local-set-key (kbd "s-b") 'compile-with-makefile)
  (local-set-key (kbd "C-c C-c") 'compile-with-makefile))
(add-hooks '(sh-mode-hook makefile-mode-hook python-mode-hook
             ruby-mode-hook scheme-mode-hook haskell-mode-hook
             asm-mode-hook c-mode-common-hook)
           'my-compile-keys)


;; alignment
(defun align-dwim ()
  "Align region, or current block if region is not active"
  (interactive)
  (require 'align)
  (if (use-region-p)
      (let ((align-region-separate 'entire))
        (align (region-beginning) (region-end)))
    (let ((align-region-separate 'group))
      (align-current))))

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

(when window-system
  (add-to-list 'kill-emacs-query-functions (lambda () (yes-or-no-p "Really kill Emacs? "))))

(defvar outlaws-base)
(defvar outlaws-platform)
(defvar outlaws-platform-inc)

(defun outlaws-compilation-finish (buffer status)
  (when (and (equal status "finished\n")
             (equal (buffer-name buffer) "*compilation*")
             (not (string-match-p "run" compile-command)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert "\nNow Reloading BROWSE and TAGS..."))
    (redisplay)
    (save-window-excursion
      (if (get-buffer "*Tree*")
          (with-current-buffer "*Tree*"
            (find-alternate-file (concat outlaws-platform "BROWSE"))
            (setq c++-ebrowse-source-table nil)
            (bury-buffer))
        (find-file (concat outlaws-platform "BROWSE"))
        (bury-buffer))
      (visit-tags-table (concat outlaws-platform "TAGS")))
    (quit-window t (get-buffer-window buffer))
    (message "Compilation and tags reload complete")))

(defun outlaws ()
  (interactive)
  (let (tot-file)
    (when (eq system-type 'darwin)
      (setq outlaws-base "/Users/arthur/Documents/outlaws/")
      (setq outlaws-platform "/Users/arthur/Documents/outlaws/osx/")
      (setq outlaws-platform-inc (concat outlaws-platform "Outlaws")))
    (when (eq system-type 'gnu/linux)
      (setq outlaws-base "/home/arthur/outlaws/")
      (setq outlaws-platform "/home/arthur/outlaws/linux/")
      (setq outlaws-platform-inc (concat outlaws-platform "src")))
    (setq tot-file (concat outlaws-base "TOT"))

    (add-to-list 'desktop-path outlaws-base)

    (save-window-excursion
      (find-file (concat outlaws-platform "Makefile"))
      (find-file (concat outlaws-platform "BROWSE"))
      (visit-tags-table (concat outlaws-platform "TAGS"))
      ;; (progn
      ;;   (message "Loading source files... ")
      ;;   (find-file (concat outlaws-base "core/*.h") t)
      ;;   (find-file (concat outlaws-base "game/*.h") t)
      ;;   (find-file (concat outlaws-base "game/*.cpp") t))
      )
    (message "Outlaws loaded")

    (setq compile-makefile (concat outlaws-platform "Makefile"))
    (add-hook 'compilation-finish-functions 'outlaws-compilation-finish)
    (desktop-read outlaws-base)
    ))


;;; language / major modes

(defalias 'run-caml 'tuareg-run-caml)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-to-list 'magic-mode-alist '("^ELF" . hexl-mode))
(add-to-list 'auto-mode-alist '("\.o\\'" . hexl-mode))

;; mode specific variables
(setq-default
 inferior-lisp-program "clisp"
 haskell-program-name "ghci"
 prolog-system 'swi
 graphviz-dot-indent-width 2
 graphviz-dot-view-command "firefox %s"
 ;; gdb-many-windows t
 nethack-use-tiles nil
 nethack-program "/usr/bin/nethack"
 doxymacs-use-external-xml-parser nil
 )

;; minor modes

(defun my-disable-partial-truncate ()
  (interactive)
  (make-local-variable 'truncate-partial-width-windows)
  (setq truncate-partial-width-windows nil))

(add-hook 'compilation-mode-hook 'my-disable-partial-truncate)

(defun my-temp-buffer-hook ()
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "k") 'kill-this-buffer))
(add-hooks '(compilation-mode-hook completion-list-mode-hook)
           'my-temp-buffer-hook)

(add-hooks '(ruby-mode-hook javascript-mode-hook)
           'flymake-extra-enable)

(add-hooks '(emacs-lisp-mode-hook lisp-interaction-mode-hook
            ruby-mode-hook) 'turn-on-eldoc-mode)

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
    (defvar ido-temp-list))
  (let ((curname (buffer-name (current-buffer))))
    (setq ido-temp-list
          (sort (mapcar 'buffer-name (buffer-list (selected-frame)))
                (lambda (a b) (or (equal b curname) (string-match "^ " b)))))
    (with-no-warnings
      (if default
          (setq ido-temp-list
                (cons default (delete default ido-temp-list)))))))
(add-hook 'ido-make-buffer-list-hook 'dont-munge-buffer-order-damnit)

;; (global-set-key (kbd "<M-tab>") (lambda () 
;;                                   (interactive) 
;;                                   (switch-to-buffer (other-buffer (current-buffer)) nil t)))
;(global-set-key (kbd "<M-S-tab>") 'previous-buffer)
(global-set-key (kbd "<M-tab>") 'ido-switch-buffer)
(define-key ido-common-completion-map (kbd "<M-tab>") 'ido-next-match)
(define-key ido-common-completion-map (kbd "<M-S-tab>") 'ido-prev-match)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-tab>") 'other-frame)
(global-set-key (kbd "<C-M-tab>")
                (lambda ()
                  "Make current buffer visible in another window, and switch to that window"
                  (interactive)
                  (pop-to-buffer (current-buffer) t)))


;; elisp

(defun fontify-elisp-better ()
  (font-lock-add-keywords
   nil `((,(concat "(" (regexp-opt '("and" "or" "setq" "setq-default") 'words))
          (1 font-lock-keyword-face))) t))
(add-hook 'emacs-lisp-mode-hook 'fontify-elisp-better)

(defun my-elisp-hook ()
  (local-set-key (kbd "C-c C-c") 'byte-compile-this-file)
  (local-set-key (kbd "C-c C-e") 'eval-this-buffer)
  (local-set-key (kbd "C-c p p") 'elp-instrument-function)
  (local-set-key (kbd "C-c p a") 'elp-instrument-package)
  (local-set-key (kbd "C-c p r") 'elp-results)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially t))
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

(defun c-indent-align-for-tab ()
  (interactive)
  (c-indent-line-or-region)
  (align-dwim))
  
(defun my-c-common-hook ()
  (c-set-style "stroustrup")
  (c-set-offset 'statement-cont '(c-lineup-assignments +))
  (c-set-offset 'inline-open 0)
  (c-set-offset 'inextern-lang 0)
  (local-set-key [remap newline-and-indent] 'c-context-line-break)
  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  ;(local-set-key (kbd "TAB") 'c-indent-align-for-tab)
  (local-set-key (kbd "TAB") 'c-indent-line-or-region)
  (local-set-key (kbd "C-c C-l") 'align-dwim)
  (imenu-add-menubar-index)
  (hexcolor-mode 1)
  (subword-mode 1)
  (setq tab-width 4)
  (make-local-variable 'forward-sexp-function)
  (setq forward-sexp-function 'my-c++-forward-sexp)
  (local-set-key [remap backward-up-list] 'my-c++-backward-up-list)
  (local-set-key [remap transpose-sexps] 'my-c++-transpose-sexps)
  (hippie-eldoc 1)
  (setq parens-require-spaces nil)
  (local-set-key (kbd "C-{") 'my-c-insert-braces)
  (local-set-key (kbd "C-<") 'insert-pair)
  (local-set-key (kbd "C->") 'my-c-insert-arrow)
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
             "in" "out" "inout" "oneway" ;; "self"
             "super"
             ;; things I use
             "foreach" "unless" "lambda")
            'symbols) . font-lock-keyword-face)
         (,(regexp-opt
            (list "float2" "float3" "float4" "vec2" "vec3" "vec4" "mat2" "mat3" "mat4"
                  "ushort" "uint" "trit"
                  "id") 'symbols) . font-lock-type-face)
         (,(regexp-opt
            (list "nil" "YES" "NO" 
                  "epsilon" "M_PIf" "M_PI" "M_TAO" "M_TAOf") 'symbols). font-lock-constant-face)
         ;("~" (0 font-lock-negation-char-face prepend))
         ("\\_<0x[0-9a-fA-f]+\\_>" . font-lock-constant-face) ; hex
         ("\\_<[0-9]*\\.?[0-9]*f?\\_>" . font-lock-constant-face) ; dec floats and ints
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

(defun python-better-highlighting ()
  "highlight string escapes and keywords in the proper face for python"
  (font-lock-add-keywords
   nil '(("\\_<\\(True\\|False\\)\\_>" . font-lock-constant-face)
         ("\\_<self\\_>" . font-lock-variable-name-face)
;;;          ("\\_<print\\_>" . font-lock-builtin-face)
         )))
(add-hook 'python-mode-hook 'python-better-highlighting)
