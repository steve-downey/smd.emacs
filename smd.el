(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;(require 'thingatpt)
(require 'imenu)

;; Network

(defun view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;; Buffer-related

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (cl-flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (setq save-place t)
  (auto-fill-mode) ;; in comments only
  (if window-system (hl-line-mode t))
  (pretty-lambdas)
  ;; TODO: this breaks in js2-mode!
  ;;(if (functionp 'idle-highlight) (idle-highlight))
  )

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; Cosmetic

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

;; Other

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory dotfiles-dir 0)
  ;; TODO: remove elpa-to-submit once everything's submitted.
  (byte-recompile-directory (concat dotfiles-dir "elpa-to-submit/" 0)))

(defun regen-autoloads (&optional force-regen)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "P")
  (let ((autoload-dir (concat dotfiles-dir "/elpa-to-submit"))
        (generated-autoload-file autoload-file))
    (when (or force-regen
              (not (file-exists-p autoload-file))
              (some (lambda (f) (file-newer-than-file-p f autoload-file))
                    (directory-files autoload-dir t "\\.el$")))
      (message "Updating autoloads...")
      (update-directory-autoloads autoload-dir)))
  (load autoload-file))

;; TODO: fix this
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun switch-or-start (function buffer)
  "If the buffer is current, bury it, otherwise invoke the function."
  (if (equal (buffer-name (current-buffer)) buffer)
      (bury-buffer)
    (if (get-buffer buffer)
        (switch-to-buffer buffer)
      (funcall function))))


(defun smd:fullscreen-toggle ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))


(defvar smd:header-suffixes '("h" "hxx" "hpp" "H" "hh"))
(defvar smd:source-suffixes '("c" "cpp" "cxx" "C" "CC" "cc"))

(defun smd:find-complement-file ()
  "Find the file complementing the source or header file in the current buffer."
  (interactive)
  (let* ((dirlist (list (file-name-directory (buffer-file-name))))
         (fn (file-name-nondirectory (buffer-file-name)))
         (prefix (file-name-sans-extension fn))
         (suffix (file-name-extension fn))
         pathname)
    (or (and prefix suffix)
	(error "%s: Not a recognized file type." fn))
    (setq pathname (locate-file
		    (concat prefix ".") dirlist
		    (cond
		     ((member suffix smd:header-suffixes) smd:source-suffixes)
		     ((member suffix smd:source-suffixes) smd:header-suffixes))))
    (if pathname
	(find-file pathname)
      (error "%s: Complementary file not found" fn))))

(defun smd:update-packages ()
  (interactive)
  (setq smd:packages
        '(ac-etags
          ack
          ample-theme
          auto-complete
          auto-complete-c-headers
          auto-complete-clang
          auto-complete-clang-async
          browse-kill-ring
          clang-format
          column-marker
          dash
          dired+
          dired-details+
          dired-details
          dtrt-indent
          ecb
          flycheck
          flycheck-google-cpplint
          git-commit-mode
          git-rebase-mode
          google-c-style
          haskell-mode
          htmlize
          icicles
          icomplete+
          inf-ruby
          json-mode
          json-reformat
          json-snatcher
          lua-mode
          magit
          magit-svn
          markdown-mode+
          markdown-mode
          markdown-toc
          multi-term
          org
          org-ac
          org-magit
          org2blog
          pkg-info
          psvn
          rainbow-delimiters
          ruby-electric
          sml-mode
          soft-stone-theme
          solarized-theme
          twilight-theme
          zenburn-theme
          ))


  (package-initialize)
  ;;; install missing packages
  (let ((not-installed (remove-if 'package-installed-p smd:packages)))
    (if not-installed
        (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                              (length not-installed)))
            (progn (package-refresh-contents)
                   (dolist (package not-installed)
                     (package-install package))))))
  )

(provide 'smd)
