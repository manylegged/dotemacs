;; Anisoptera Games
;; project oriented stuff

(require 'arthur-functions)

(defvar anisoptera-base)
(defvar anisoptera-platform)

(defun anisoptera-compilation-finish (buffer status)
  (when (and (equal status "finished\n")
             (equal (buffer-name buffer) "*compilation*")
             (not (string-match-p "run" compile-command))
             (let ((case-fold-search t))
               (string-match-p "tag" compile-command)))
    (with-current-buffer buffer
      (if (not (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "Nothing to be done" (point-max) t)))
          (progn
            (goto-char (point-max))
            (insert "\nNow Reloading BROWSE and TAGS...")
            (redisplay)
            (save-window-excursion
              (if (get-buffer "*Tree*")
                  (with-current-buffer "*Tree*"
                    (find-alternate-file (concat anisoptera-platform "BROWSE"))
                    (setq c++-ebrowse-source-table nil)
                    (bury-buffer))
                (find-file-noselect (concat anisoptera-platform "BROWSE"))
                (bury-buffer))
              (visit-tags-table (concat anisoptera-platform "TAGS")))
            (message "Compilation and tags reload complete"))
        (message "Nothing changed, skipping tags reload")))
    (quit-window nil (get-buffer-window buffer))))

(defun anisoptera-setup (base platform-ext)
  "Load all the tags files and set up paths for the project"
  (setq anisoptera-base (concat base "/")
        anisoptera-platform (concat base "/" platform-ext "/"))
  (ignore-errors
    (save-window-excursion
      (find-file-noselect (concat anisoptera-platform "Makefile"))
      (find-file-noselect (concat anisoptera-platform "BROWSE"))
      (visit-tags-table (concat anisoptera-platform "TAGS")))
    ;; (progn
    ;;   (message "Loading source files... ")
    ;;   (find-file (concat anisoptera-base "core/*.h") t)
    ;;   (find-file (concat anisoptera-base "game/*.h") t)
    ;;   (find-file (concat anisoptera-base "game/*.cpp") t))
    )

  (setq compile-makefile (concat anisoptera-platform "Makefile"))
  (add-hook 'compilation-finish-functions 'anisoptera-compilation-finish)
  (when (get-buffer "*Warnings*")
    (bury-buffer "*Warnings*"))
  (message "Anisoptera loaded: %s" (file-name-base base)))

(defun anisoptera-stack-lookup-sentinel (process event)
  (when (string= event "finished")
    (let ((buf (process-buffer process)))
      (set-buffer buf)
      (compilation-mode)
      (pop-to-buffer buf)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "\nsentinel recieved event: " event))
      (goto-char (point-min))
      (unless (search-forward "Dumping stack" (point-max) t)
        (goto-char (point-max))
        (forward-line (- (/ (frame-height) 2))))
      (recenter))))

(defun anisoptera-stack-lookup (arg)
  (interactive "p")
  (let* ((buf (get-buffer-create "*crashlog*"))
         (args (append (list "*stack-lookup*" buf (concat anisoptera-base "scripts/stack_lookup.py")
                             (and arg "-a")))))
    (when (and buffer-file-name
               (or (string-match-p "/Reassembly.*[.]txt$" buffer-file-name)
                   (string-match-p "/201[56789].*[.]txt[.]gz$" buffer-file-name)))
      (setq args (append args (list buffer-file-name))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (insert "Running: ")
      (print args buf))
    (let ((process (apply 'start-process args)))
      (set-process-sentinel process 'anisoptera-stack-lookup-sentinel)
      )))
(global-set-key (kbd "C-c s") 'anisoptera-stack-lookup)


(defun my-anisoptera-isearch-point ()
  "Jump to log file in stack_lookup.py triage output"
  (interactive)
  (push-mark)
  (let ((end (line-end-position))
        (beg (line-beginning-position)))
    (goto-char end)
    (let ((slash (search-backward "/" beg t)))
      (when slash
        (setq beg (1+ slash))))
    (message (buffer-substring beg end))
    (goto-char end)
    (search-forward (buffer-substring beg end))
    (recenter (min (max 0 scroll-margin)
                   (truncate (/ (window-body-height) 4.0))))))

(defun reassembly (arg)
  (interactive "P")
  (cond
   ((eq system-type 'darwin) (anisoptera-setup "/Users/arthur/Documents/outlaws" "osx"))
   ((eq system-type 'gnu/linux) (anisoptera-setup "/home/arthur/outlaws" "linux"))
   ((eq system-type 'windows-nt) (anisoptera-setup "C:/Users/Arthur/Documents/outlaws" "win32"))
   ((eq system-type 'cygwin) (anisoptera-setup "/cygdrive/c/Users/Arthur/Documents/outlaws" "win32"))
   (t (error "unsupported system"))))

(defun helios (arg)
  (interactive "P")
  (cond
   ((eq system-type 'darwin) (anisoptera-setup "/Users/arthur/Documents/helios" "platform/osx"))
   ((eq system-type 'gnu/linux) (anisoptera-setup "/home/arthur/helios" "platform/linux"))
   ((eq system-type 'windows-nt) (anisoptera-setup "C:/Users/Arthur/Documents/helios" "platform/win32"))
   ((eq system-type 'cygwin) (anisoptera-setup "/cygdrive/c/Users/Arthur/Documents/helios" "platform/win32"))
   (t (error "unsupported system")))
  (find-file (concat anisoptera-base "game/Helios.cpp")))

(provide 'anisoptera)