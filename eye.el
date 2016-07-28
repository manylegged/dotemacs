;;; eye.el --- Display images, comic books, and documents
;; 
;; Released under the GPL. No implied warranties, etc. Use at your own risk.
;; 
;;; History:
;; Copyright (C) Arthur Danskin <arthurdanskin@gmail.com>
;; 
;; April 2008: Initial version
;; May 2008: better process interruption handling, `find-file' support
;;           ansi-art
;;
;;; Commentary:
;; 
;; Eye of Emacs is a major mode for displaying images, slideshows,
;; comic books, and documents. It should seem familar to users of
;; desktop image viewing programs, such as eye of gnome, gqview,
;; mirage, or evince. It also borrows some keybindings from `dired'.
;; It can display multiple images in one window. This is useful for
;; reading comic books and documents. Unfortunately, because of its
;; reliance on external processes, it is a little slower than these
;; programs. It can also display images as color ascii (ansi) art.
;;
;; In contrast to `doc-view-mode', eye supports (optional) resizing,
;; rendering with poppler (xpdf) instead of ghostview, and multiple
;; page display. Docview support postscript and dvi, cropping pages,
;; searching within documents, and has many fewer bugs.
;;
;; Eye avoids hanging emacs by passing continuations to asyncrounous
;; process sentinels. There are several backends, which list and fetch
;; files, and frontends, which display files. In the middle, images
;; are resized and rotated.
;;
;; BUGS:
;; * emacs seems to load images asyncronously, so their size is not
;;   known until a short time after they are loaded. This kind of
;;   breaks my packing algorithm.
;; 
;; TODO:
;; * No 1. problem: it's too slow!
;; * continuous mode like in evince?
;;   - could just load all the images and put them in the buffer
;;   - this is too slow
;; * write a little program using libspectre to render postscript to png
;; * image based rather than window based zoom?
;; * text view thing
;; * some kind of thumbnail mode (maybe just zoomed out)
;;   - support for switching between thumbnail and normal mode
;; * different sort modes
;; * get rid of (eq eye-frontend 'ansi)...
;; * use convert OR netpbm
;; * eye-source, eye-resize, eye-rotate, etc.. should be per image
;;   - exim support can set rotation for one image
;; 
;;; Features:
;; * resizes images to fit your window (asyncronously)
;; * go to the next, previous, nth, or a random image
;; * slideshow repeats your last manual movement
;; * image preloading
;; * view comic book archives and pdfs, extracting images on demand
;; * multiple image/page display
;; * manga right-to-left mode
;; * rotate images arbitrarily
;; * view images as ANSI art on a text terminal
;;
;;; Requirements:
;; Eye is only tested with emacs 23/24, but it might work with emacs 22
;; External programs are required for various features.
;; * ImageMagick for resize/rotate/pdf support
;; * pdftoppm/pdfinfo (comes with xpdf or poppler) for pdf support
;; * unrar for rar/cbr support
;; * unzip/zipinfo for zip/cbz support
;; * img2txt (comes with libcaca 1.x) for ansi art support
;; * curl for web support
;; 
;; 
;;; Installation:
;; 
;; (require 'eye)
;; (add-to-list 'auto-mode-alist (cons eye-image-type-regexp 'eye-mode))
;; (mapc (lambda (x) (add-to-list 'auto-mode-alist (cons (car x) 'eye-mode)))
;;;       eye-backend-alist)
;; (add-to-list 'magic-fallback-mode-alist (cons 'image-type-auto-detected-p 'eye-mode))
;;
;; C-x C-f your file.
;; 
;; You can also use autoloads:
;; 
;; (autoload 'eye "eye" "View images or comicbooks." t)
;;
;; M-x eye
;;
;;
;; You might want to use M-x eye to view very large documents or
;; comics, since it does not require emacs to read the whole archive.
;;
;;; Code:

(require 'image-mode)
(require 'dired)
(with-no-warnings
  (require 'cl))
(require 'ffap)

(defsubst eye-emacs-has-imagemagick () nil)
(defsubst eye-can-resize ()
  (and (or (executable-find "convert") (imagemagick-filter-types)) t))

;; ;; user variables
(defvar eye-resize (eye-can-resize)
  "Should images be resized to fit the window?
If a number, zoom relative to window size.")
(defvar eye-rotate 0
  "How many degrees counterclockwise should image be rotated?")
(defvar eye-manga nil "Should we read right to left?
This has no effect unless `eye-multi' is non-nil")
(defvar eye-multi t "Display multiple images at a time?
Images are displayed left to right, unless `eye-manga' is non-nil")
(defvar eye-invert nil)


;; internal variables
(defvar eye-files nil "Vector of files or number of files.")
(defvar eye-source nil "File or directory where we get our images from")
(defvar eye-index 0 "Current page number we are viewing.")
(defvar eye-process nil "The current subprocess.")
(defvar eye-process-queue nil "List of commands to run")
(defvar eye-last-ndisplay nil "Number of images displayed on this screen.")
(defvar eye-error nil "Did the last redisplay trigger an error?")

(defvar eye-text-files nil "List of text files in `eye-source'.")
(defvar eye-text-regexp "\\.\\(txt\\|nfo\\)\\'"
  "Regexp for matching text files")

(defvar eye-temp-dir-base (concat temporary-file-directory "emacs-eye/")
  "Directory for cached resized images and extracted images.
Keep in mind that this will be rm -rf ed when we clear the cache.")
(defvar eye-temp-dir nil)
(defvar eye-convert-program (executable-find "convert"))
(defvar eye-convert-args '(-filter "Quadratic"))


(defconst eye-image-types
  (apply 'nconc (mapcar (lambda (x) (cdr (assoc x '((xbm "xbm") (xpm "xpm") (gif "gif")
                                                    (pbm "pbm") (png "png") (svg "svg")
                                                    (jpeg "jpg" "jpeg" "JPG") (tiff "tiff")))))
                        image-types)) "List of supported image types")
(defconst eye-image-type-regexp (format "\\.%s\\'" (regexp-opt eye-image-types t))
  "Regexp matching supported image types.")

(defvar eye-unzip (executable-find "unzip"))
(defvar eye-unrar (executable-find "unrar"))
(defvar eye-pdftoppm (executable-find "pdftoppm"))
(defvar eye-convert (executable-find "convert"))
(defvar eye-curl (executable-find "curl"))
(defvar eye-zipinfo (executable-find "zipinfo"))

(defvar eye-img2txt (executable-find "img2txt"))

(defvar eye-backend-alist
  (append
   '(("/\\'" :list eye-dir-list-files :extract eye-dir-with-image))
   `((,eye-image-type-regexp :list eye-dir-initial-list-files))
   (and eye-unzip
        '(("\\.\\(cbz\\|zip\\)\\'" :list eye-zip-list-files
           :extract eye-zip-with-image)))
   (and eye-unrar
        '(("\\.\\(cb\\|ra\\)r\\'" :list eye-rar-list-files
           :extract eye-rar-with-image)))
   (and eye-pdftoppm
        eye-convert
        '(("\\.pdf\\'" :list eye-pdf-pages :extract eye-pdf-with-image
           :noresize t)))
   (and eye-curl
        '(("\\`https?://" :list eye-web-list-links :extract eye-web-with-image)
          ("\\.html"      :list eye-web-file-list-links :extract eye-web-with-image)
          )))
  "Alist of (REGEXP . BACKEND) for different file types.")

(defvar eye-frontend nil "Element from `eye-frontend-alist'.")
(defvar eye-frontend-alist
  '((image :load eye-image-load :display eye-image-display
           :init eye-image-init)
    (ansi :load eye-ansi-load :display eye-ansi-display :init eye-ansi-init))
  "Alist of frontends.")

(defconst eye-subprocess-buffer " *eye subprocess*"
  "Name of debugging buffer for process output")

;; slideshow
(defvar eye-slideshow-timer nil "Timer for slideshow.")
(defvar eye-slideshow-delay 3 "Seconds between slide advance.")
(defvar eye-slideshow-command 'eye-find-next "Command to auto advance")
(defvar eye-slideshow-arg 1 "Argument to `eye-slideshow-command'.")

;; preloading 
(defvar eye-preload t "Should images be preloaded?
Special value 'all means preload continously.")
(defvar eye-preload-next nil "Image index queued for preload.")


(defun eye-temp-buffer-kill ()
  (let ((buf (get-buffer (concat " * eye temp" eye-source))))
    (when buf
      (kill-buffer buf))))

(defun eye-temp-buffer ()
  (get-buffer-create (concat " * eye temp" eye-source)))
    

;; general

(defun index (el seq)
  "Return the index of EL in SEQ, or nil if EL is not in SEQ."
  (let ((i 0) done)
    (cond
     ((listp seq)
      (while (and (null done) seq)
        (if (equal el (car seq))
            (setq done t)
          (setq seq (cdr seq)
                i (1+ i)))))
   ((vectorp seq)
    (while (and (not done) (< i (length seq)))
      (if (equal el (elt seq i))
          (setq done t)
        (setq i (1+ i)))))
   (t (error "not a list or vector: %s" seq)))
    (if done i nil)))


(defun plist-remove (plist prop)
  "Destructively remove PROP from PLIST and returns PLIST."
  (if (eq (car plist) prop)
      (cddr plist)
    (let ((p plist))
      (while p
        (if (not (eq (caddr p) prop))
            (setq p (cddr p))
          (setcdr (cdr p) (cddddr p))
          (setq p nil))))
    plist))

(defun eye-command-to-string (command &rest args)
  "Execute command COMMAND with ARGS and return its output as a string."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'call-process command nil t nil args))))


(defvar eye-image-size-cache nil "Cache for `eye-image-size'.")

(defun eye-image-size (image)
  "Return (WIDTH . HEIGHT) of file IMAGE in pixels."
  (setq image (file-truename image))
  (or (gethash image eye-image-size-cache)
      (let ((str (eye-command-to-string "identify" image)))
        (if (string-match "\\([0-9]+\\)x\\([0-9]+\\)" str)
            (puthash image (cons (string-to-number (match-string 1 str))
                                 (string-to-number (match-string 2 str)))
                     eye-image-size-cache)
          (error "Size of %s not found in '%s'" image str)))))

(defsubst eye-ratio (size) (/ (float (car size)) (cdr size)))

(defsubst eye-nfiles ()
  "Number of files currently being displayed."
  (cond ((numberp eye-files) eye-files)
        ((vectorp eye-files) (length eye-files))
        (t 1)))

(defsubst eye-file (&optional index)
  "Return the file at INDEX or `eye-index'."
  (if (eq (eye-nfiles) 0)
      nil
    (setq eye-index  (mod eye-index (eye-nfiles)))
    (if (sequencep eye-files)
        (elt eye-files (if index (mod index (eye-nfiles)) eye-index))
      eye-source)))

(defun eye-window-size (&optional pixels)
  "Return window size as (WIDTH . HEIGHT).
In pixels unless `eye-ansi' is non-nil."
  (let ((window (or (get-buffer-window (current-buffer) t)
                    (car (window-list nil 'no-mini)))))
    (if pixels
        (let* ((window-size (window-inside-pixel-edges window))
               (width (- (caddr window-size) (car window-size)))
               (height (- (cadddr window-size) (cadr window-size))))
          (cons width height))
    (cons (window-width window)
          (window-height window)))))


;; resize and extract

(defun eye-sentinel (process state)
  "Sentinel for `eye-start-filter-process'"
  (let ((buf (process-get process 'buffer)))
    ;; Check if buffer was killed while we were running
    (when (buffer-live-p buf)
      (set-buffer buf)
      (setq eye-process nil
            eye-error (not (equal state "finished\n")))
      (eye-mode-line-update)
      (when eye-error
        (progn
;;;             (pop-to-buffer eye-subprocess-buffer)
          (message "%s: %s" process state)))
      (apply (process-get process 'callback)
             (process-get process 'args))
      (when (and eye-process-queue (not eye-process))
        (let ((args (car eye-process-queue)))
          (setq eye-process-queue (cdr eye-process-queue))
          (apply 'eye-start-filter-process args))))))

(defun eye-start-filter-process (command filter-or-buffer callback &rest args)
  "Run COMMAND list asyncronously, with output to FILTER-OR-BUFFER.
Set CALLBACK as the sentinel, called with ARGS."
;;;   (when eye-process (error "One process at a time!"))
  (when eye-error
    (setq eye-error nil))
  (if eye-process
      (add-to-list 'eye-process-queue
                   (nconc (list command filter-or-buffer callback) args)
                   'append)
    (unless eye-temp-dir
      (setq eye-temp-dir (concat eye-temp-dir-base
                                 (replace-regexp-in-string
                                  "/" "=" eye-source) "/")))
    (make-directory eye-temp-dir t)
    (let ((buf (get-buffer-create eye-subprocess-buffer))
          (default-directory eye-temp-dir)
          (process-connection-type nil))
      (setq eye-process
            (apply 'start-process (format "eye process: %s" command)
                   (if (bufferp filter-or-buffer)
                       filter-or-buffer
                     eye-subprocess-buffer)
                   command))
      (set-process-sentinel eye-process 'eye-sentinel)
      (process-put eye-process 'buffer (current-buffer))
      (process-put eye-process 'callback callback)
      (process-put eye-process 'args args)
      (when (functionp filter-or-buffer)
        (set-process-filter eye-process filter-or-buffer))
      (print command buf)))
  (eye-mode-line-update))

(defun eye-start-process (command callback outfile)
  "Start an asyncronous process with COMMAND list.
Process should create OUTFILE. If OUTFILE already exists, just
call callback directly. When it's done, call CALLBACK with OUTFILE"
  (unless (file-name-absolute-p outfile)
    (setq outfile (concat eye-temp-dir outfile)))
  (if (file-exists-p outfile)
      (funcall callback outfile)
    (eye-start-filter-process command nil callback outfile)))

(defsubst eye-backend-get (fun)
  "Return the element of the current backend named FUN"
  (let ((backend (assoc-default
                  eye-source eye-backend-alist 'string-match-p)))
    (eye-assert backend "Eye does not support this file type")
    (plist-get backend fun)))

(defsubst eye-resize-p () (and eye-resize (not (eq eye-frontend 'ansi))
                               (not (eye-backend-get :noresize))))
(defsubst eye-rotate-p () (/= 0 eye-rotate))

(defun eye-convert-args ()
  "Argument list for ImageMagick convert"
  (let (args)
    (dolist (x eye-convert-args)
      (cond ((eq t x))
            ((symbolp x) (push (symbol-name x) args))
            ((stringp x) (push x args))))
    (setq args (nreverse args))

    (let ((no-bigger t) width height)
      (when (eye-resize-p)
        (let ((size (eye-window-size t)))
          (setq width (car size)
                height (cdr size)))
        (when (numberp eye-resize)
          (setq width (* width eye-resize)
                height (* height eye-resize)
                no-bigger nil))
        (setq args (nconc args (list "-resize"
                                     (format "%dx%d%s" width height
                                             (if no-bigger ">" "")))))))
    (when (eye-rotate-p)
      (setq args (nconc args (list "-background" (face-background 'default)
                                   "-rotate" (number-to-string eye-rotate)))))
    args))

(defun eye-temp-file-name (base args)
  "Temp name based on BASE and ARGS
Unique for these variables."
  (let* ((basename (file-name-sans-extension (file-name-nondirectory base)))
         (argstr (if args 
                     (concat (replace-regexp-in-string "[/:]" "_" (mapconcat 'identity args "_")) ";")
                   ""))
         (ext (if (string-match-p "\.gif\\'" base) "gif" "png")))
    (format "%s%s.%s" basename argstr ext)))

(defun eye-with-converted-image (image callback &optional pipe)
  "Resize/rotate IMAGE appropriately and call CALLBACK.
If PIPE is non-nil, use it as a command that produces the image
on stdout. Then, IMAGE should be a temp file name with the right
extension."
  (if (or (not (or (eye-resize-p) (eye-rotate-p)))
          (eye-emacs-has-imagemagick))
      (if pipe
          (eye-start-process
           (list "/bin/sh" "-c" (format "%s > %s" pipe image))
           callback image)
        (funcall callback image))
    (let* ((args (eye-convert-args))
           (temp-file (eye-temp-file-name image args)))
      (if pipe
            (eye-start-process
             `("/bin/sh" "-c"
               ,(format "%s | %s - %s %s"
                        pipe eye-convert-program (mapconcat 'shell-quote-argument args " ")
                        (shell-quote-argument temp-file)))
             callback temp-file)
          (eye-start-process (append (list eye-convert-program) args (list image temp-file))
                             callback temp-file)))))


;; directory backend
(defun eye-dir-with-image (callback dir index)
  (eye-with-converted-image (eye-file index) callback))

(defun eye-dir-list-files (path callback)
  "Return a vector of files below PATH matching `eye-image-type-regexp'."
  (let* ((files (process-lines
                 "find" (expand-file-name path) "-type" "f"))
         (imgfiles (delete-if-not (lambda (x) (string-match eye-image-type-regexp x)) files)))
    (funcall callback (sort imgfiles 'string<))))


(defun eye-dir-initial-list-files (path callback)
  "Special backend for when we have an initial image.
Basically `eye-dir-list-files', but set up `eye-index' and switch
to the directory backend."
  (let ((img eye-source))
    (setq eye-source (file-name-directory eye-source))
    (eye-dir-list-files eye-source
                        `(lambda (files)
                           (setq eye-index (index ,img files))
                           (rename-buffer (eye-buffer-name eye-source) t)
                           (funcall ',callback files)))))


;; zip backend
(defun eye-zip-with-image (callback zipfile index)
  (let* ((file (replace-regexp-in-string "\\([][*+?]\\)" "\\\\\\1" (eye-file index)))
         (tmp (format "page%d.%s" index (file-name-extension file))))
    (eye-with-converted-image
     tmp callback
     (format "%s -p %s '%s'"
             eye-unzip (shell-quote-argument (expand-file-name zipfile)) file))))

(defun eye-zip-list-files (zipf callback)
  (funcall callback
           (sort (delete-if-not
                  'eye-image-name-p
                  (process-lines eye-zipinfo "-1" (expand-file-name zipf)))
                 'string<)))

;; rar backend
(defun eye-rar-with-image (callback rarfile index)
  ;; FIXME We don't seem to have any way to determine the
  ;; path of a file in the archive.
  (let* ((file (replace-regexp-in-string "\\([*?]\\)" "\\\\\\1" (eye-file index)))
         (tmp (format "page%d.%s" index (file-name-extension file))))
    (eye-with-converted-image
     tmp callback
     (format "%s p -ierr %s '%s'" eye-unrar (shell-quote-argument rarfile)
             (concat "*" file)))))

(defun eye-rar-list-files (rarf callback)
  ;; FIXME unrar lists files without directories !!!
  (funcall callback
           (sort (delete-if-not
                  'eye-image-name-p
                  (process-lines eye-unrar "lb" (expand-file-name rarf)))
                 'string<)))

;; html/web backend

(defun eye-web-temp-file-name (url)
  "Temp name based on URL."
  (replace-regexp-in-string "[/:]" "_" url))

(defun eye-web-fetch (url callback &rest args)
  "Download URL and call (CALLBACK file).
If ARGS are given, call (apply CALLBACK buffer-with-url-source args)."
  (if args
      (progn
        (apply 'eye-start-filter-process
               (list eye-curl "--compressed" "--silent" url)
               (eye-temp-buffer) callback (cons (eye-temp-buffer) args)))
    (let ((outfile (eye-web-temp-file-name url)))
      (eye-start-process (list eye-curl "--silent" url "--output" outfile) callback outfile))))

(defvar eye-ignore-link-regexp (regexp-opt (list "bits\."      ; wikipedia logos, etc
                                                 "/wiki/File:" ; actually a text file...
                                                 "thumb"
                                                 "4chan-ads"))
  "List of regular expressions matching links to ignore when scanning html for image links")

(defun eye-fix-mediawiki (link)
  (if (string-match "\\(.*commons/\\)thumb/\\(.*\\)/[^/]+" link)
      (concat (match-string 1 link) (match-string 2 link))
    link))

(defvar eye-fix-link-functions (list 'eye-fix-mediawiki)
  "list of string -> string functions to run on image links before download")

(defun eye-url-domain (url)
  "Return just the domain name part of URL (i.e. everything up to the .com)"
  (if (string-match "https?://[^/ ]+" url)
      (match-string 0 url)
    nil))

(defun eye-web-find-images-in-html (source-url)
  (let ((linkre (concat "\\(href\\|src\\)=\"\\([^&\"' ]+"
                        (substring eye-image-type-regexp 0 -2)
                        "\\)"))
        (domain (eye-url-domain source-url))
        links link)
    (goto-char (point-min))
    (while (re-search-forward linkre nil t)
      (let ((link (match-string 2)))
        (setq link (cond ((string-match-p "^https?://" link) link)
                         ((string-match-p "^//" link) (concat "http:" link))
                         ((string-match-p "^/" link) (concat domain link))
                         (t (concat source-url "/" link))))
        (dolist (fun eye-fix-link-functions)
          (setq link (funcall fun link)))
        (unless (string-match-p eye-ignore-link-regexp link)
          (setq links (cons link links)))))
    (delete-dups (nreverse links))))

(defun eye-web-list-links-1 (page callback)
    (funcall callback 
             (let ((source-url eye-source))
               (with-current-buffer page
                 (eye-web-find-images-in-html source-url)))))

(defun eye-web-list-links (url callback)
  (eye-web-fetch url 'eye-web-list-links-1 callback))

(defun eye-web-file-list-links (name callback &rest args)
  (let ((buf (find-file-noselect name nil)))
    (unwind-protect
        (apply 'eye-web-list-links-1 buf callback args)
      (kill-buffer buf))))

(defun eye-web-with-image (callback url index)
  (eye-web-fetch
   (eye-file index)
   `(lambda (f) (eye-with-converted-image f ',callback))))

(defun eye-w3m-load ()
  "Load the current link into an `eye-mode' buffer"
  (interactive)
  (let ((a (and (fboundp 'w3m-anchor) (w3m-anchor))))
    (if a
        (save-window-excursion
          (with-current-buffer (eye a t)
            (eye-preload-all)
            (message "Eye is watching %s" a)))
      (message "No link"))))


;; pdf backend
(defvar eye-pdf-ratio nil "WIDTH / HEIGHT of PDF pages.")

(defun eye-pdf-pdftoppm-command (file page)
  (let* ((window-size (eye-window-size t))
         (window-ratio (eye-ratio window-size))
         (size (if (> window-ratio eye-pdf-ratio)
                   (if (> eye-pdf-ratio 1)
                       (* eye-pdf-ratio (cdr window-size))
                     (cdr window-size))
                 (if (> eye-pdf-ratio 1)
                     (car window-size)
                   (/ (car window-size) eye-pdf-ratio)))))
    (when (numberp eye-resize)
      (setq size (* size eye-resize)))
    ;; pages count from 1
    (setq page (1+ (mod page (eye-nfiles))))
    (split-string
     (format "pdftoppm -f %d -l %d -scale-to %d %s"
             page page size (shell-quote-argument file)))))

(defun eye-pdf-with-image (callback pdffile page)
  (let* ((pdftoppm-cmd (eye-pdf-pdftoppm-command pdffile page))
         (convert-args (eye-convert-args))
         (tmpfile (eye-temp-file-name
                   pdffile (append pdftoppm-cmd convert-args))))
    (eye-start-process
     (list "/bin/sh" "-c"
           (format "%s | %s %s - %s"
                   (mapconcat 'shell-quote-argument pdftoppm-cmd " ")
                   eye-convert-program
                   (mapconcat 'shell-quote-argument convert-args " ")
                   (shell-quote-argument tmpfile)))
     callback tmpfile)))

(defun eye-pdf-pages (file callback)
  "Find the number of pages in pdf FILE.
Also set `eye-pdf-ratio'."
  (let ((str (eye-command-to-string "pdfinfo" file)))
    (if (string-match "^Page size: +\\([0-9.]+\\) x \\([0-9.]+\\)" str)
        (let ((w (string-to-number (match-string 1 str)))
              (h (string-to-number (match-string 2 str))))
          (setq eye-pdf-ratio (/ (float w) h)))
      (error "Can't discover page size"))
    (if (string-match "^Pages: +\\([0-9]+\\)$" str)
            (funcall callback (string-to-number (match-string 1 str)))
      (error "Can't discover number of pages"))))

;; (defun eye-with-ps-image (callback psfile page)
;;   (eye-start-process
;;    `("gs" "-dSAFER" "-r150" "-s" "-dNOPAUSE" "-sDEVICE=png16m"
;;      "-dTextAlphaBits=4" "-dBATCH" "-dGRAPHICSAlphaBits=4")
;;    callback ))

;; (defun eye-doc-with-image (callback docfile page)
;;   (eye-start-process `("convert" "-identify"
;;                        "-geometry" ,(format "x%d" (frame-pixel-height))
;;                        ,docfile `tmpfile)
;;                      callback (format "%s-%d.png"
;;                                        (file-name-sans-extension
;;                                         (file-name-nondirectory docfile))
;;                                        page)))


(defsubst eye-with-image (callback index)
  "Call CALLBACK with the file name of the INDEX th file
extracted from `eye-source' and properly resized, rotated, etc.
and ready to display."
  (funcall (eye-backend-get :extract) callback eye-source index))



;;; ansi frontend
;; 
;; this section borrows from ansi-color.el. I didn't use the
;; ansi-color code because it doesn't support highlight colors and is
;; too slow. We only support the small subset of ansi escape sequences
;; that libcaca uses.

;; global vars
(defconst eye-ansi-escape-regexp "\\[\\([0-9;]*\\)m")
(defvar eye-ansi-faces [default (:weight . bold) (:weight . light)
                         (:slant . italic) (:underline . t)
                         (:weight . bold) nil (:inverse-video . t)]
  "See `ansi-color-faces-vector'")
(defvar eye-ansi-colors ["black" "firebrick4" "green4" "goldenrod3"
                         "RoyalBlue3" "VioletRed4" "cyan4" "grey90"]
  "See `ansi-color-names-vector'")
(defvar eye-ansi-hi-colors ["grey30" "firebrick2" "green2" "goldenrod1"
                            "RoyalBlue1" "VioletRed3" "cyan3" "white"]
  "High intensity version of `eye-ansi-colors'")
(defvar eye-ansi-code-map nil "ANSI code map.
Set from `eye-ansi-faces', `eye-ansi-colors', and `eye-ansi-hi-colors'
 by `eye-ansi-make-map'.")


;; local
;; (defvar eye-ansi-data nil "Escape codes go in here while img2txt is running.")
(defvar eye-ansi-current-face nil "Current face for ANSI escapes.")
(defvar eye-ansi-cache nil
  "Cache for propertized ansi images")
(defvar eye-ansi-charset 0 "Index into `eye-ansi-charsets'")
(defconst eye-ansi-charsets ["ascii" "shades" "blocks"])
(defvar eye-ansi-point nil "(X% . Y%) point location.")

(defun eye-ansi-cache-get (name width height)
  (gethash (list name width height eye-ansi-charset) eye-ansi-cache))

(defun eye-ansi-cache-put (name width height val)
  (puthash (list name width height eye-ansi-charset) val eye-ansi-cache))

(defun eye-ansi-cache-clear ()
  (setq eye-ansi-cache (make-hash-table :test 'equal)
        eye-image-size-cache (make-hash-table :test 'equal)))

(defun eye-ansi-make-map ()
  (let ((vec (make-vector 110 nil)))
    (dotimes (i 8) (aset vec i (aref eye-ansi-faces i)))
    (dotimes (i 8) (aset vec (+ i 30) (cons :foreground (aref eye-ansi-colors i))))
    (dotimes (i 8) (aset vec (+ i 40) (cons :background (aref eye-ansi-colors i))))
    (dotimes (i 8) (aset vec (+ i 90) (cons :foreground (aref eye-ansi-hi-colors i))))
    (dotimes (i 8) (aset vec (+ i 100) (cons :background (aref eye-ansi-hi-colors i))))
    vec))

      
(defsubst eye-ansi-escape-to-face (escape)
  "Convert the n;n;n part of an ANSI escape code to text
properties, updating `eye-ansi-current-face'."
  (dolist (code (split-string escape ";" t))
    (setq code (string-to-number code))
    (unless eye-ansi-code-map
      (setq eye-ansi-code-map (eye-ansi-make-map)))
    (let ((face (aref eye-ansi-code-map code)))
      (case face
        (default
          (setq eye-ansi-current-face nil))
        (nil nil)
        (otherwise
         (setq eye-ansi-current-face
               (plist-put eye-ansi-current-face
                          (car face) (cdr face))))))))





;; (defun eye-ansi-point-save ()
;;   (setq eye-ansi-point
;;         (cons (/ (float (current-column))
;;                  (- (line-end-position) (line-beginning-position)))
;;               (/ (float (line-number-at-pos))
;;                  (line-number-at-pos (point-max)) 2))))

;; (defun eye-ansi-point-restore ()
;;   (goto-line (round (* (cdr eye-ansi-point)
;;                        (line-number-at-pos (point-max)) 2)))
;;   (forward-char (round (* (car eye-ansi-point)
;;                           (- (line-end-position) (line-beginning-position)))))
;;   (recenter))
     

(defsubst eye-insert-rectangle (str)
  "Like `insert-rectangle', but STR is a string."
  ;; (insert-rectangle (split-string str)) is too slow.
  (save-match-data
    (let ((beg 0)
          (column (current-column)))
      (while (string-match "\n" str beg)
        (insert (substring str beg (match-beginning 0)))
        (unless (zerop (forward-line 1))
          (insert ?\n))
        (move-to-column column)
        (setq beg (match-end 0)))
      (insert (substring str beg)))))

(defun eye-ansi-colorize (str)
  "Interpret ANSI codes in STR and return a propertized string."
  (with-temp-buffer
    (let ((beg 0)
          (start-m (point)))
      (while (string-match eye-ansi-escape-regexp str beg)
        ;; highlight before the escape with the old face
        (let ((pre-str (substring str beg (match-beginning 0))))
          (insert
           (if eye-ansi-current-face
               (propertize pre-str 'face eye-ansi-current-face)
             pre-str)))
        (setq beg (match-end 0))
        ;; process the escape to setup the new face
        (eye-ansi-escape-to-face (match-string 1 str)))
      (delete-and-extract-region start-m (point)))))


(defun eye-ansi-init ()
  (setq cursor-type t))

(defun eye-ansi-display (data size linep)
  "Display DATA. On a new line if LINEP."
  (when linep
    (goto-char (point-max))
    (insert ?\n))
  (if eye-manga
      (beginning-of-line)
    (end-of-line))
  (eye-insert-rectangle data)
  (forward-line (- 1 (cdr size))))

(defun eye-ansi-load-callback (buf callback file width height)
  "Callback for when img2txt is done running."
  (funcall callback
           (eye-ansi-cache-put
            file width height
            ;; FIXME colorize should take a region
            (eye-ansi-colorize
             (with-current-buffer buf
               (delete-and-extract-region (point-min) (point-max)))))
           (cons width height)))


(defun eye-ansi-load (file callback)
  "Load FILE using 'img2txt' from libcaca.
Call (callback image size)."
;;;   (setq eye-ansi-data nil)
  (let* ((i-ratio (* 2 (eye-ratio (eye-image-size file))))
         (width (1- (window-width)))
         (height (window-height))
         (w-ratio (/ (float width) height)))
    (when (numberp eye-resize)
      (setq width (round (* eye-resize width))
            height (round (* eye-resize height))))
    (if (> w-ratio i-ratio)
        (setq width (round (* height i-ratio)))
      (setq height (round (/ width i-ratio))))
    (let ((cached (eye-ansi-cache-get file width height)))
      (if cached
          (funcall callback cached (cons width height))
        (eye-start-filter-process
         (list eye-img2txt "--format=utf8"
               (format "--width=%d" width) (format "--height=%d" height)
;;;                  (format "--charset=%s" (elt eye-ansi-charsets
;;;                                              eye-ansi-charset))
               file)
         (eye-temp-buffer) 'eye-ansi-load-callback
         (eye-temp-buffer) callback file width height)))))


;; image frontend

(defun eye-image-init ()
  (setq cursor-type nil))

(defun eye-image-load (file callback)
  (let* ((args (if (eye-emacs-has-imagemagick)
                   (append
                    (when (eye-rotate-p) (list :rotation eye-rotate))
                    (when (eye-resize-p)
                      (let* ((size (eye-window-size t))
                             (width (car size))
                             (height (cdr size)))
                        (if (numberp eye-resize)
                            (list :width (* width eye-resize) :height (* height eye-resize))
                          (list :max-width width :max-height height)))))))
         (img (apply 'create-image file nil nil args)))
    (image-animate img)
    (funcall callback img (image-size img))))

(defun eye-image-display (image size linep)
  "Actually display IMAGE."
  (let* ((inhibit-read-only t))
    (goto-char (point-max))
    (when linep
      (insert "\n"))
    (when eye-manga
      (beginning-of-line))
    (let ((index (+ eye-index eye-last-ndisplay)))
      (insert (propertize
               " " 'display image 'eye-index index
               'help-echo (format "%s (index %d)"
                                  (abbreviate-file-name
                                   (eye-file index))
                                  index))))))


(defsubst eye-frontend-get (fun)
  (let* ((front (assoc eye-frontend eye-frontend-alist))
         (fn (plist-get (cdr-safe front) fun))
          ;; (nth (case fun (:load 1) (:display 2) (:init 3)) front)
         )
    (unless fn
      (error "Frontend or method not found! %s %s" eye-frontend fun))
    fn))
            

;; packing

(defvar eye-pack-remaining-width 0 "Width in pixels left on the screen.")
(defvar eye-pack-remaining-height 0 "Height in pixels left on the screen.")
(defvar eye-pack-row-height 0 "Pixels of height in the last row.")
(defvar eye-pack-displayed 0 "Number of images packed into this window.")


(defun eye-pack-init ()
  (setq eye-pack-remaining-width (car (eye-window-size))
        eye-pack-remaining-height (cdr (eye-window-size))
        eye-pack-row-height 0
        eye-pack-displayed 0)
  (funcall (eye-frontend-get :init)))

(defun eye-pack (size)
  "Pack image of SIZE onto the screen.
Return nil if this image does not fit. Return 'line if we started
a new line. Otherwise, return t."
  (let* ((width (car size))
         (height (cdr size))
         (fits t))
    (setq eye-pack-remaining-width (- eye-pack-remaining-width width))
    (when (< eye-pack-remaining-width 0)
      (setq eye-pack-remaining-width (- (car (eye-window-size)) width)
            eye-pack-remaining-height (- eye-pack-remaining-height
                                         eye-pack-row-height)
            eye-pack-row-height 0)
      (setq fits 'line))
    (setq eye-pack-row-height (max eye-pack-row-height height))
    (when (or (and (eq fits 'line)
                   (< eye-pack-remaining-height eye-pack-row-height))
              (not eye-multi)
              (> eye-pack-displayed (eye-nfiles)))
      (setq fits nil))
    (when (zerop eye-pack-displayed)
      (setq fits t))
    (setq eye-pack-displayed (1+ eye-pack-displayed))
    fits))


(defun eye-pack-display (image size)
  "Use the current frontend to display IMAGE with SIZE."
  (when (= eye-last-ndisplay 0)
    (eye-pack-init))
  (let ((packp (eye-pack size)))
    (if packp
        (progn
          (let ((inhibit-read-only t))
            (when (= eye-last-ndisplay 0)
              (erase-buffer))
            (funcall (eye-frontend-get :display)
                     image size (eq packp 'line)))
          (set-buffer-modified-p nil)
          (setq eye-last-ndisplay (1+ eye-last-ndisplay))
          (eye-mode-line-update)
          (eye-with-image 'eye-display (+ eye-index eye-last-ndisplay)))
      (unless (eye-redisplay)
        (eye-mode-line-update)
        (goto-char (point-min))
        (when eye-slideshow-timer
          (eye-slideshow-start))
        (eye-preload)))))

(defun eye-display (file)
  "Start display of FILE using the current frontend."
  (funcall (eye-frontend-get :load) file 'eye-pack-display))

(defun eye-files-set (files)
  (if (or (null files) (equal files []))
      (progn
        (setq eye-files nil)
        (error "No images found"))
    (setq eye-files (if (listp files) (vconcat files) files))
    (eye-redisplay)))

(defvar eye-last-config nil "Config when we last finished displaying.")

(defun eye-config ()
  "Return a list unique for the current display state."
  (list eye-resize eye-rotate eye-index eye-multi eye-manga
        (null eye-files) (eye-window-size)
        eye-frontend (and (eq eye-frontend 'ansi) eye-ansi-charset)
        (eye-convert-args)))


(defun eye-redisplay (&optional force)
  "Redisplay the images if something changed.
Only redisplay if some settings actually changed, unless FORCE.
Interactively, FORCE is always true. Return non-nil if we
actually start a process."
  (interactive (list t))
  (eye-mode-line-update)
;;;   (unless (file-readable-p eye-source)
;;;     (error "Lost source: %s" (abbreviate-file-name eye-source)))
  (and
   (or force (not (equal (eye-config) eye-last-config)))
   (prog1 t
     (setq eye-last-config (eye-config)
           eye-last-ndisplay 0)
     (if eye-files
         (eye-with-image 'eye-display eye-index)
       (funcall (eye-backend-get :list) eye-source 'eye-files-set)))))


(defun eye-preload-next (image size)
  (if (and eye-preload-next (eye-pack size))
      (progn
        (setq eye-preload-next (1+ eye-preload-next))
        (eye-with-image 'eye-preload-pack eye-preload-next))
    (when (eq eye-preload 'all)
      (setq eye-preload t))
    (setq eye-preload-next nil)))

(defun eye-preload-pack (file)
  "Continue preloading if FILE will be displayed next."
  (unless (eye-redisplay)
    (funcall (eye-frontend-get :load) file 'eye-preload-next)))

(defun eye-preload ()
  "Preload `eye-preload-next'."
  (when (and eye-preload eye-preload-next)
    (eye-pack-init)
    (if (eq eye-preload 'all)
        (let* ((nfiles (eye-nfiles))
               (idx eye-preload-next)
               (last (mod (1- eye-preload-next) nfiles)))
          (while (not (eq idx last))
            (eye-with-image (lambda (file) nil) idx)
            (setq idx (mod (1+ idx) nfiles))))
      (eye-with-image 'eye-preload-pack eye-preload-next))))


;; slideshow

(defun eye-slideshow-callback (buffer)
  (set-buffer buffer)
  (funcall eye-slideshow-command
           eye-slideshow-arg))

(defun eye-slideshow-start ()
  "(Re)Start the slideshow timer.
It will be restarted when the image is done loading."
  (when eye-slideshow-timer
    (cancel-timer eye-slideshow-timer))
  (setq eye-slideshow-timer (run-at-time eye-slideshow-delay
                                     nil 'eye-slideshow-callback
                                     (current-buffer))))

(defun eye-slideshow-toggle (arg)
  "Toggle slideshow state.
With a positive prefix ARG, turn on slideshow and set the
timout to the prefix value. With a negative prefix ARG, disable
slideshow. Each advance will use the same command as the last
`eye-find-next' or `eye-find-random'."
  (interactive "P")
  (when (null arg)
    (setq arg (if eye-slideshow-timer -1 eye-slideshow-delay)))
  (if (> (prefix-numeric-value arg) 0)
      (progn
        (setq eye-slideshow-delay arg)
        (eye-slideshow-start))
    (when eye-slideshow-timer
      (cancel-timer eye-slideshow-timer)
      (setq eye-slideshow-timer nil)))
  (eye-mode-line-update))


;; interactive

(defvar eye-cache-auto-clear t
  "Should we automatically clear the cache?
'never means don't clear and don't prompt.
other non-nil means clear and don't prompt
nil means prompt")

(defun eye-cache-clear (&optional force)
  "Clear the image cache.
With FORCE, don't prompt and override `eye-cache-auto-clear'."
  (ignore-errors
    (when (and (or force
                   (not (eq eye-cache-auto-clear 'never)))
               (file-directory-p eye-temp-dir)
               (or force
                   eye-cache-auto-clear
                   (yes-or-no-p "Clear the image cache? ")))
      (message "Clearing image cache...")
      (clear-image-cache)
      (eye-ansi-cache-clear)
      (eye-temp-buffer-kill)
      (call-process "rm" nil nil nil "-rf" eye-temp-dir)
      (message "Clearing image cache... done"))))

(defun eye-kill-process ()
  "Kill the currently running process."
  (interactive)
  (when eye-process
    (delete-process eye-process)))

(defun eye-alternate-source (file &optional switch)
  "Call `eye' if current buffer is from a file or `eye-url' if from a url"
  (interactive (list (if (eye-source-url eye-source)
                         (eye-read-url eye-source)
                       (eye-read-file-name eye-source nil))
                     (not current-prefix-arg)))
  (let ((old (current-buffer)))
    (if (eye-source-url file)
        (eye-url file t)
      (eye file t))
    (kill-buffer old)))

(defun eye-file-copy-name ()
  "Put the current file in the kill ring."
  (interactive)
  (message "In kill ring: %s" (eye-file))
  (kill-new (eye-file)))

(defvar eye-file-copy-last-dir nil)

(defun eye-file-copy (file)
  "Copy the current image to FILE"
  (interactive
   (list (read-file-name "Save this file to: " eye-file-copy-last-dir
                         nil nil (file-name-nondirectory (eye-file)))))
;;;   (cd (file-name-directory file))
  (setq file (expand-file-name file)
        eye-file-copy-last-dir (file-name-directory file))
  (let (eye-resize
        (eye-rotate 0))
    (eye-with-image
     `(lambda (f) (copy-file f ,file 1)
        (message "Saved to %s" ,(abbreviate-file-name file)))
     eye-index)))

(defun eye-file-rename ()
  "Rename the current image to FILE"
  (interactive)
  (if (file-exists-p (eye-file))
      (let ((file (read-file-name "Rename to: ")))
        (rename-file (eye-file) file 1)
        (message "Renamed %s to %s" (abbreviate-file-name (eye-file))
                 (abbreviate-file-name file)))
    (message "%s is not a local file" (eye-file))))


(defun eye-file-delete ()
  "Delete the current file"
  (interactive)
  (if (file-exists-p (eye-file))
      (when (yes-or-no-p (format "Delete %s? " (eye-file)))
        (delete-file (eye-file))
        (setq eye-files nil)
        (eye-redisplay))
    (message "%s is not a local file" (eye-file))))


(defun eye-mode-line-update ()
  "Redraw all the information in the mode-line"
  (when eye-files
    (setq mode-line-buffer-identification
          (format "%12s/%s" 
                  (file-name-nondirectory eye-source)
                  (if (eye-file) (file-name-nondirectory (eye-file)) "")))
    (setq mode-line-position
          (if (> (eye-nfiles) 0)
              (format "%2d%% (%s of %d)"
                      (* 100 (/ (float eye-index) (eye-nfiles)))
                      (if (> eye-last-ndisplay 1)
                          (format "%d-%d" eye-index
                                  (mod (+ eye-index eye-last-ndisplay -1)
                                   (eye-nfiles)))
                        eye-index)
                      (eye-nfiles))
            "No Files"))
    (setq mode-name
          (concat
           "Eye"
           (and (or eye-preload eye-resize eye-multi
                    (eye-rotate-p) eye-slideshow-timer) "/")
           (and eye-preload "P")
           (and (eq eye-preload 'all) "a")
           (and eye-multi (if eye-manga "J" "M"))
           (if (numberp eye-resize)
               (format "(%.0f%%)" (* 100 eye-resize))
             (and eye-resize "x"))
           (and (eye-rotate-p) (format "[%d°]" eye-rotate))
           (and eye-slideshow-timer
                (format "{%s, %.2gs}"
                        (if (eq eye-slideshow-command 'eye-find-next)
                            (format "next %s"
                                    (if eye-slideshow-arg
                                        (format "%+d" eye-slideshow-arg)
                                      "page"))
                          "random")
                        eye-slideshow-delay))))
    (setq mode-line-process
          (let* ((queue-len (length eye-process-queue))
                 (run-str (cond ((> queue-len 0) (format ":run(%d)" queue-len))
                                (eye-process ":run")
                                (t nil))))
            (and run-str
                 (propertize
                  run-str 'face 'mode-line-emphasis))))
    (force-mode-line-update)))

;; movement
(defvar eye-next-random nil "Next random number.")

(defun eye-find-random (arg)
  "Go to a random image."
  (interactive "p")
  (setq eye-index (or eye-next-random (random (eye-nfiles)))
        eye-next-random (random (eye-nfiles))
        eye-slideshow-command 'eye-find-random
        eye-preload-next eye-next-random)
  (eye-nav-bob)
  (eye-redisplay))

(defun eye-find-next (arg)
  "Go to the next page of images.
With ARG, go to the ARG th next image."
  (interactive "P")
  (setq eye-slideshow-command 'eye-find-next
        eye-slideshow-arg arg
        arg (if arg (prefix-numeric-value arg)
              (max 1 eye-last-ndisplay))
        eye-index (+ eye-index arg)
        eye-preload-next (+ eye-index arg))
  (eye-nav-bob)
  (eye-redisplay))

(defun eye-find-previous (arg)
  "Go to the ARG th previous image."
  (interactive "p")
  (eye-find-next (- arg)))

(defun eye-find-right (arg)
  "Go to the slide to the right of this one."
  (interactive "p")
  (eye-find-next (if eye-manga (- arg) arg)))

(defun eye-find-left (arg)
  "Go to the slide to the left of this one."
  (interactive "p")
  (eye-find-right (- arg)))

(defun eye-find-nth (arg)
  "Go to the ARG th image."
  (interactive
   (list (read-number "Jump to index: "
                      (mod (+ eye-last-ndisplay eye-index) (eye-nfiles)))))
  (assert (numberp arg))
  (setq eye-index arg)
  (eye-nav-bob)
  (eye-redisplay))


(defun eye-find-by-name (file)
  "Go to the image named FILE.
Return t if we find it, or signal an error."
  ;;  In pdfs, `eye-files' is a number, so this function doesn't
  ;; really make sense. We just ignore it.
  (interactive
   (list (completing-read "Jump to file: "
                          (if (sequencep eye-files)
                              (mapcar 'abbreviate-file-name eye-files)
                            (list eye-source))
                          nil t)))
  (when (sequencep eye-files)
    (setq eye-index
          (or (index (if (file-name-absolute-p file)
                         (expand-file-name file)
                       file)
                     eye-files)
              (error "Eye is not watching %s" file))))
  (eye-nav-bob)
  (eye-redisplay)
  t)


(defun eye-view-text (file)
  "View text FILE in the archive"
  (interactive
   (list (if eye-text-files
             (completing-read "Read commentary: " eye-text-files nil t nil nil
                              (car-safe eye-text-files))
           nil)))
  (if file
      (view-file-other-window file)
    (message "No text available")))

(defun eye-find-file-on-source ()
  "Call `find-file' on `eye-source'."
  (interactive)
  (if (eye-source-file-p)
      (let ((file (eye-file)))
        (find-file eye-source)
        (when (eq major-mode 'dired-mode)
          (search-forward (file-name-nondirectory file))
          (local-set-key (kbd "c") 'eye)
          (message (substitute-command-keys
                    "Jump to file in Eye with \\[eye]"))))
    (message "Eye Source %s is not a file" eye-source)))


(defun eye-find-image-at-point ()
  (interactive)
  (setq eye-index
        (or (get-text-property (point) 'eye-index)
            (get-text-property (1- (point)) 'eye-index)
            (error "Can't find index")))
  (eye-redisplay))
    


(defmacro define-eye-nav (direction point-move image-move)
  (declare (debug (&define name name name)))
  `(defun ,(intern (concat "eye-nav-" (symbol-name direction)))
     ()
     ,(format "Call `%s' or `%s' as appropriate" point-move image-move)
     (interactive)
     (call-interactively
      (if (or (eq eye-frontend 'ansi) (> eye-last-ndisplay 1))
          ',point-move
        ',image-move))))

(define-eye-nav forward forward-char image-forward-hscroll)
(define-eye-nav backward backward-char image-backward-hscroll)
(define-eye-nav down next-line image-next-line)
(define-eye-nav up previous-line image-previous-line)
(define-eye-nav page-up scroll-up image-scroll-up)
(define-eye-nav page-down scroll-down image-scroll-down)
(define-eye-nav bol beginning-of-line image-bol)
(define-eye-nav eol end-of-line image-eol)
(define-eye-nav bob beginning-of-buffer image-bob)
(define-eye-nav eob end-of-buffer image-eob)


;; entrance

(defsubst eye-image-name-p (name)
  (string-match-p eye-image-type-regexp name))

(defun eye-source-name (name)
  "Return (maybe mangled) NAME if eye-mode can handle it, or nil."
  (when (stringp name)
    (when (file-directory-p name)
      (setq name (concat name "/")))
    (unless (ffap-url-p name)
      (setq name (expand-file-name name)))
;;;     (when (string-match-p eye-image-type-regexp name)
;;;       (setq name (or (file-name-directory name) default-directory)))
    (and (assoc-default name eye-backend-alist 'string-match-p)
         name)))

(defun eye-read-file-name (&optional initial noprompt)
  "Ask the user for a file name, trying to find a good default.
Try INITIAL as the default. If NOPROMPT is non-nil, don't prompt
if we can guess a good default."
  (setq initial
        (or (eye-source-name initial)
          (and (eq major-mode 'dired-mode)
               (eye-source-name (ignore-errors (dired-get-file-for-visit))))
          (eye-source-name (ffap-file-at-point))))
  (if (and noprompt initial)
      initial
    (read-file-name "Direct the Eye of Emacs: "
                    (and initial (file-name-directory initial)) nil t
                    (and initial (file-name-nondirectory initial))
                    'eye-source-name)
;;;     (completing-read "Direct the Eye of Emacs: "
;;;                      'read-file-name-internal 'eye-source-name
;;;                      nil initial 'file-name-history nil)
;;;       (completing-read "Direct the Eye of Emacs: "
;;;                        'ffap-read-file-or-url-internal
;;;                        'eye-source-name nil init (list 'file-name-history))
    ))

(defun eye-source-url (str)
  (if (not (stringp str))
      nil
    (let ((name (if (string-match-p "https?://" str)
                    str (concat "http://" str))))
      (and (assoc-default name eye-backend-alist 'string-match-p)
           name))))

(defun eye-source-file-p ()
  (or (file-readable-p eye-source)
      (file-directory-p eye-source)))
  
(defun eye-read-url (&optional initial)
  "Ask the user for a url, trying to find a good default.
Try INITIAL as the default. If NOPROMPT is non-nil, don't prompt
if we can guess a good default."
  (setq initial (or initial (eye-source-url (ffap-url-at-point))))
  (eye-source-url
   (completing-read "Direct the Eye of Emacs: "
                    'ffap-read-url-internal
                    'eye-source-url nil initial (list 'file-name-history))))

(defun eye (file &optional switch)
  "FILE is an image, comic book archive, pdf, or directory of images.
See `eye-mode' for more information. Return the eye buffer, or
switch to it if SWITCH. Tries to reuse an eye buffer if
possible."
  (interactive (list (eye-read-file-name nil nil)
                     (not current-prefix-arg)))
  (let* ((name (eye-buffer-name file))
         (buf (get-buffer name)))
    (unless (and buf (ignore-errors
                       (set-buffer buf)
                       (eye-find-by-name file)))
      (setq buf (get-buffer-create name))
      (set-buffer buf)
      (let ((buffer-file-name file))
        (eye-mode)))
    (when switch
      (switch-to-buffer buf))
    buf))

(defun eye-url (url &optional switch)
  "URL is a web page with images or image links to view.
See `eye-mode' for more information. Return the eye buffer, or
switch to it if SWITCH. Tries to reuse an eye buffer if
possible."
  (interactive (list (eye-read-url)
                     (not current-prefix-arg)))
  (let* ((name (eye-buffer-name url))
         (buf (get-buffer name)))
    (unless (and buf (ignore-errors
                       (set-buffer buf)
                       (eye-find-by-name url)))
      (setq buf (get-buffer-create name))
      (set-buffer buf)
      (let ((buffer-file-name url))
        (eye-mode)))
    (when switch
      (switch-to-buffer buf))
    buf))


(defun eye-size-change-function ()
  "Hook for `window-configuration-change-hook'."
  (unless eye-error
    (with-current-buffer (window-buffer)
      (condition-case e
          (eye-redisplay)
        (error (setq eye-error t))))))


(defun eye-buffer-name (file)
  "Return a buffer name for displaying FILE."
  (setq file (file-truename (expand-file-name file)))
  (format "%s *eye*"
          (cond ((string-match-p "^https?://" file) file)
                ((file-directory-p file)
                 (file-name-nondirectory
                  (substring (file-name-directory file) 0 -1)))
                (t (file-name-nondirectory file)))))

(defun eye-assert (condition message)
  (unless condition
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert message)
      (set-buffer-modified-p nil))
    (error message)))


(define-derived-mode eye-mode fundamental-mode "Eye"
  "The Eye of Emacs views images, comicbooks, and documents.

Press \\[eye-find-next] to go to the next image.
If you update your file or directory, press \\[eye-refresh] to
let eye know.

You can zoom in and out with \\[eye-zoom-in] and
\\[eye-zoom-out]. Zoom sizes are relative to the size of your
window. You can toggle resizing off with \\[eye-resize-toggle].

Keybindings are vaguely reminiscent of dired and GQview. There
are a lot of duplicates, so hopefully you can just mash the
keyboard and it will do what you expect.

The eye will display all images in `eye-source', or the current
file by default. You can enter this mode with \\[eye] or just
\\[find-file] if you have added eye to `auto-mode-alist'. If you
want to have more than one eye on the same directory or file, you
can rename one with \\[rename-buffer].

Mode line flags / toggle keys / customization variables:
\\[eye-multi-toggle] or \\[eye-manga-toggle]  `eye-multi'. \\[eye-manga-toggle]  means `eye-manga'
\\[eye-resize-toggle]       `eye-resize'
\\[eye-preload-toggle]       `eye-preload'
\[NUM\]   `eye-rotate'

Resized images are cached in `eye-temp-dir'. See
`eye-cache-auto-clear'.

This mode depends on a number of external programs. You need
ImageMagick to resize and rotate images, and pdftoppm (part of
xpdf or poppler) to view pdfs.

\\{eye-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  ;; There has got to be a better way to do this
  (mapc 'make-local-variable '(mode-line-buffer-identification
        eye-base-mode-name eye-preload-next eye-process
        eye-slideshow-timer eye-slideshow-delay
        eye-slideshow-command eye-slideshow-arg
        mode-line-position eye-multi eye-manga
        eye-pack-remaining-width eye-pack-remaining-height
        eye-pack-row-height eye-pack-displayed eye-last-ndisplay
        eye-index eye-next-random cursor-type eye-source
        eye-files eye-resize eye-rotate eye-temp-dir eye-error
        eye-last-config eye-pdf-ratio
        eye-ansi-current-face eye-ansi-point eye-frontend))

  ;; deal with URLs and stuff
  (unless eye-source
    (setq eye-source (eye-source-name buffer-file-name)))

  (setq buffer-read-only t
        buffer-undo-list t
        ;; This should probably be nil, but it causes problems in
        ;; `find-file-hooks'.
;;;         buffer-file-name eye-source
        cursor-type nil
        truncate-lines t
        eye-last-ndisplay 0
        eye-files nil
        eye-index 0
        eye-preload-next nil

        ;; FIXME this should be done with `eye-frontend-alist'
        eye-frontend (cond ((display-images-p) 'image)
                           (eye-img2txt 'ansi))
        )

  (eye-assert
   eye-frontend
   "Please use a GUI version of Emacs or install 'img2txt' from libcaca.")

  (eye-assert
   (eye-source-name eye-source)
   (case (file-name-extension eye-source)
     ((rar cbr) "Install the 'unrar' program to view this file.")
     ((zip cbz) "Install the 'unzip' program to view this file.")
     (pdf "Install the 'pdftoppm' and 'convert' programs to view this file.")
     (otherwise "Eye can not view this file type.")))

  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "Loading %s..."
                    (abbreviate-file-name eye-source)))
    (set-buffer-modified-p nil))

  (rename-buffer (eye-buffer-name eye-source) t)
  (eye-ansi-cache-clear)
;;;   (add-hook 'window-configuration-change-hook
;;;             'eye-size-change-function nil t)
  (add-hook 'kill-buffer-hook 'eye-cache-clear nil t)
  (image-mode-setup-winprops)
  (eye-redisplay))
  


(defmacro define-eye-toggle (var init doc &rest body)
  (declare (indent defun))
  (let ((name (intern (concat (symbol-name var) "-toggle"))))
    `(progn
       (defvar ,var ,init ,doc)
       (defun ,name (&optional arg)
       ,(format "With no prefix ARG, toggle `%s'.
 With prefix ARG, turn it on if ARG is positive." var)
       (interactive "P")
       (if arg
           (setq ,var (> (prefix-numeric-value arg) 0))
         (setq ,var (not ,var)))
       ,@body
       (eye-redisplay)))))

(define-eye-toggle eye-resize (eye-can-resize)
  "Should images be resized to fit the window?
If a number, zoom relative to window size."
  (when eye-resize (eye-nav-bob)))
(define-eye-toggle eye-preload t "Should images be preloaded?
Special value 'all means preload continously.")
(define-eye-toggle eye-multi t "Display multiple images at a time?
Images are displayed left to right, unless `eye-manga' is non-nil")
(define-eye-toggle eye-manga nil "Should we read right to left?
This has no effect unless `eye-multi' is non-nil")

(defun eye-invert-toggle () (interactive) "Invert image colors?"
  (if (plist-member eye-convert-args '-negate)
      (plist-remove eye-convert-args '-negate)
    (plist-put eye-convert-args '-negate t)))

(defun eye-preload-all ()
  "Preload the rest of the images."
  ;; FIXME we really maybe just curl everything with one process
  (interactive)
  (setq eye-preload 'all
        eye-preload-next (1+ eye-index))
  (if eye-files
      (eye-preload)
    (eye-redisplay)))

(defun eye-choose (cur seq msg)
  (let ((def (nth (mod (1+ (index cur seq)) (length seq)) seq)))
    (completing-read (format "%s (default %s): " msg def)
                     (mapcar (lambda (x) (format "%s" x)) seq)
                     nil t nil nil (format "%s" def))))

(defun eye-frontend-set (frontend)
  "Set the frontend."
  (interactive (list (intern (eye-choose eye-frontend
                                         (mapcar 'car eye-frontend-alist)
                                         "Use frontend"))))
  (assert (assoc frontend eye-frontend-alist))
  (setq eye-frontend frontend)
  (when (called-interactively-p 'interactive)
    (message "Using %s frontend" eye-frontend))
  (eye-redisplay))

(defun eye-ansi-charset-toggle ()
  "Cycle through `eye-ansi-charsets'."
  (interactive)
  (setq eye-ansi-charset (mod (1+ eye-ansi-charset)
                              (length eye-ansi-charsets)))
  (eye-redisplay))

(defun eye-zoom-in (arg)
  "Zoom in prefix ARG amount."
  (interactive "p")
  (unless (numberp eye-resize)
    (setq eye-resize 1.0))
  (setq eye-resize (+ (* 0.1 arg) eye-resize))
  (eye-redisplay))

(defun eye-zoom-out (arg)
  (interactive "p")
  (eye-zoom-in (- arg)))


;; (defconst eye-zoom-set-completions
;;   '("none" "fit" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))

;; (defun eye-zoom-set-complete (str pred flag)
;;   (let ((numberp (string-match-p "[0-9.]" str)))
;;     (case flag
;;       (t (if numberp
;;              (mapcar (lambda (n) (concat str n))
;;                      (cddr eye-zoom-set-completions))
;;            (all-completions str eye-zoom-set-completions)))
;;       (lambda (if numberp
;;              t
;;            (and (member str eye-zoom-set-completions) t)))
;;       ((nil) (try-completion str eye-zoom-set-completions)))))

(defun eye-zoom-set (arg)
  "Zoom to ARG percent"
  (interactive "nZoom percent: "
;;;    (let ((read 
;;;           (completing-read
;;;            (format "Zoom (currently %s): "
;;;                    (cond
;;;                      ((numberp eye-resize)
;;;                       (format "%d%% of window" eye-resize))
;;;                      (eye-resize "fit to window")
;;;                      (t "none")))
;;;            'eye-zoom-set-complete nil nil nil eye-resize)))
;;;      (list (if (string-match-p "[0-9. \t]" read)
;;;                (string-to-number read)
;;;              (equal read "fit"))))
   )
  (setq eye-resize (if (numberp arg)
                       (/ arg 100.0)
                     arg))
  (eye-redisplay))

(defun eye-rotate-right (arg)
  "Increase rotation by ARG * 90 degrees."
  (interactive "p")
  (setq eye-rotate (mod (+ eye-rotate (* 90 arg)) 360))
  (eye-redisplay))
  
(defun eye-rotate-left (arg)
  (interactive "p")
  (eye-rotate-right (- arg)))

(defun eye-rotate-right-set (arg)
  "Set rotation to ARG degrees to the right."
  (interactive "nSet right rotation: ")
  (setq eye-rotate (mod arg 360))
  (eye-redisplay))

(defun eye-rotate-left-set (arg)
  (interactive "nSet left rotation: ")
  (eye-rotate-right-set (- arg)))

(defun eye-refresh (force)
  "Clear the cache, reload the source, and redisplay.
   With prefix, force reload"
  (interactive "P")
  (eye-cache-clear force)
  (setq eye-files nil)
  (eye-redisplay force))

(add-to-list 'auto-mode-alist '("\\.JPG\\'" . image-mode))

(mapc
 (lambda (z) (define-key eye-mode-map (car z) (cadr z)))
 `((,(kbd "SPC") eye-find-next)
   (,(kbd "<C-right>") eye-find-right)  (,(kbd "<C-left>") eye-find-left)
   (,(kbd "n") eye-find-next)       (,(kbd "f") eye-find-next)
   (,(kbd "p") eye-find-previous)   (,(kbd "b") eye-find-previous)
   (,(kbd "<backspace>") eye-find-previous)
   (,(kbd "r") eye-find-random)     ([remap goto-line] eye-find-nth)
   (,(kbd "g") eye-find-nth)        (,(kbd "j") eye-find-by-name)
   (,(kbd "C-f") eye-find-file-on-source)

   (,(kbd "o") eye-alternate-source)
   ([remap recenter-top-bottom] eye-redisplay)
   (,(kbd "C-c C-l") eye-refresh)
   (,(kbd "q") quit-window)         (,(kbd "k") kill-buffer)
   (,(kbd "v") eye-view-text)       (,(kbd "h") describe-mode)

   (,(kbd "x") eye-resize-toggle)   (,(kbd "a") eye-frontend-set)
   (,(kbd "C") eye-ansi-charset-toggle)
   (,(kbd "z") eye-zoom-set)        (,(kbd "-") eye-zoom-out)
   (,(kbd "+") eye-zoom-in)         (,(kbd "=") eye-zoom-in)
   (,(kbd "P") eye-preload-toggle)  (,(kbd "M-p") eye-preload-all)
   (,(kbd "s") eye-slideshow-toggle)
   (,(kbd "M") eye-multi-toggle)    (,(kbd "J") eye-manga-toggle)
   (,(kbd "[") eye-roatate-left)     (,(kbd "]") eye-rotate-right)
   (,(kbd "{") eye-rotate-left-set) (,(kbd "}") eye-rotate-right-set)

   (,(kbd "C-c C-k") eye-kill-process)
   (,(kbd "C-g") eye-kill-process)
   ([remap kill-ring-save] eye-file-copy-name)
   ([remap kill-region] eye-file-copy-name)
   (,(kbd "S") eye-file-copy) 
   (,(kbd "R") eye-file-rename)
   (,(kbd "D") eye-file-delete)
   
;;;    (,(kbd "<down-mouse-1>") eye-find-image-at-point)
   (,(kbd "RET") eye-find-image-at-point)
   ([remap forward-char] eye-nav-forward)
   ([remap backward-char] eye-nav-backward)
   ([remap right-char] eye-nav-forward)
   ([remap left-char] eye-nav-backward)
   ([remap next-line] eye-nav-down)    ([remap previous-line] eye-nav-up)
   ([remap scroll-up] eye-nav-page-up) ([remap scroll-down] eye-nav-page-down)
   ([remap move-beginning-of-line] eye-nav-bol)
   ([remap move-end-of-line] eye-nav-eol)
   ([remap move-beginning-of-buffer] eye-nav-bob)
   ([remap move-end-of-buffer] eye-nav-eob)
   ))

(dotimes (i 10)
  (define-key eye-mode-map (number-to-string i) 'digit-argument))
  

(put 'eye-mode 'mode-class 'special)
(add-to-list 'auto-mode-alist '("\\.cb[rz]\\'" . eye-mode))


(provide 'eye)

;;; eye.el ends here
