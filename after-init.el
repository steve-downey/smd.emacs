;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))


;; further customization, not part of default
;;(require 'smd)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))


(use-package google-c-style)

;; I like to know what time it is. These lines show the clock in
;; the status bar. Comment out first line if you prefer to show
;; time in 12 hour format
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)


;; Show trailing white spaces
(setq-default show-trailing-whitespace t)

(use-package calc)
(use-package calc-ext :ensure nil)
(defvar calc-command-flags)


;;=========================================================================
;;
;; Customize the Emacs Window titlebar like: "user@hostname:/path/to/file".
;; Replace path to ~ with ~, display buffer name if no buffer file name
;;
;;=========================================================================
;; (setq frame-title-format (format "%s@%s:%%f"
;;                                  (user-login-name)
;;                                  (system-name)))

(setq frame-title-format
      '("" user-login-name "@" system-name ":"
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;=========================================================================
;;
;; Toggle truncated long lines
;;
;;=========================================================================
(defun truncate-lines-toggle ()
  "Toggle truncate-lines variable.  If truncate-lines is non-nil, lines longer
than the window-width are displayed with a continuation symbol."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (message "Long lines will %sbe truncated"
           (if truncate-lines
               ""
             "NOT "))
  (redraw-display))

(global-set-key "\C-xt" 'truncate-lines-toggle)


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)



(global-set-key (quote [f12]) (quote ff-find-related-file))
(global-set-key "\C-c\C-c" (quote comment-region))

(global-set-key (quote [f10]) (quote next-error))
(global-set-key (quote [(control f10)]) (quote previous-error))


;; Turn on the menu bar for exploring new modes
(global-set-key [f1] 'menu-bar-mode)
(global-set-key [f2] 'tool-bar-mode)


;; Use regex searches by default.
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)


;;color escape sequence support in shell mode
(use-package ansi-color)



;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, a for this file) to jump to it.

;; You should add registers here for the files you edit most often.

(dolist (r `((?i (file . ,(locate-user-emacs-file "init.el")))
             (?m (file . ,"/bb/mbig/mbig77/msgsvn/trunk/msgbig/msgbig_objects.list"))
             (?p (file . ,"~/.profile"))
             (?b (file . ,"~/.bashrc"))
             (?a (file . ,(locate-user-emacs-file "taps/smd/after-init.el")))
             ))
  (set-register (car r) (cadr r)))


;; Always add a final newline
(setq require-trailing-newline t)



;;
;; reindent on paste
;;
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))



;; truncate shell buffers
(setq comint-buffer-maximum-size 2048)
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

;; Clean up unused buffers after 8 days
(use-package midnight)
(setq clean-buffer-list-delay-general 8)



(use-package newcomment :ensure nil)
(setq comment-auto-fill-only-comments 1)



(setq major-mode 'text-mode)


(global-set-key [delete] 'delete-char)
(normal-erase-is-backspace-mode 1)


;; TRAMP config

(use-package tramp
  :init
  (setq tramp-default-method "ssh")
  )


;; ;; after mouse selection in X11 apps, you can paste by `yank' in emacs
(setq select-enable-primary t)
(setq mouse-drag-copy-region nil)
(setq select-active-regions nil)




;; clang format
(use-package clang-format
  :bind ("C-c f" . 'clang-format-region)
  )
;;(global-set-key [C-tab] 'clang-format-region)
;;(global-set-key (kbd "C-c f") 'clang-format-region)
(global-set-key (kbd "C-x \\") 'align-entire)

(setq user-mail-address "sdowney@sdowney.org")

(when (getenv "BBENV")
  (setq exordium-bloomberg 't))

;; BLP specific
(when exordium-bloomberg
  (add-to-list 'load-path "~/elisp/blp-emacs")
  (require 'blp-console)
  (require 'sql3-comdb2)

  (add-to-list 'tramp-default-proxies-alist
               '("balab1" nil "/ssh:sdowney@ony-bam1:"))

  (add-to-list 'tramp-default-proxies-alist
               '("labafn3" nil "/ssh:sdowney@ony-bam1:"))

  (add-to-list 'tramp-restricted-shell-hosts-alist
               "\\`ony-bam1\\'")

  (setq mail-host-address "bloomberg.net")
  (setq user-mail-address "sdowney2@bloomberg.net")

  (setq magit-git-executable "/opt/bb/bin/git")
  (setq magit-git-output-coding-system (quote utf-8))

  (require 'ib-c-style)
  (defun my-c-mode-common-hook ()
    (ib-set-c-style)
    (ib-make-newline-indent)
    ;; I like auto-newline and hungry-delete
    (c-toggle-auto-state 1)
    (c-toggle-hungry-state 1)
    )
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  )

(use-package find-dired
  :init
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
  )


;; ;; DAEMON MODE
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (load-theme exordium-theme t))))
;;   (load-theme exordium-theme t))

;;; SERVER MODE
(use-package server)
(if (server-running-p)
    (load-theme 'tomorrow-night-blue t)
  (setq confirm-kill-emacs #'yes-or-no-p)
  (server-start)
  (global-set-key (kbd "C-x C-3") 'server-edit))


;; compatibility hacks
(setq x-gtk-use-system-tooltips nil)
(tooltip-mode nil)
(setq show-help-function nil)


;; ;; org-mode and babel config
;; (eval-after-load "org"
;;   '(require 'ox-md nil t))

;; (setq org-completion-use-ido t)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '( (perl . t)
;;     (ruby . t)
;;     (shell . t)
;;     (python . t)
;;     (emacs-lisp . t)
;;     (C . t)
;;     (R . t)
;;     (latex . t)
;;     ))
(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  :ensure graphviz-dot-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(setq org-support-shift-select 'always)

;; Do not prompt to confirm evaluation
;; This may be dangerous - make sure you understand the consequences
;; of setting this -- see the docstring for details
;;(setq org-confirm-babel-evaluate nil)
;; (setq org-src-fontify-natively t)
;; (setq org-src-preserve-indentation t)



(use-package org-ref
  :config
  (setq org-capture-templates
        '(
          ("c" "BibTex" plain (file "~/org/ref.bib") "\n\n\n\n%?")
          ))

  (setq reftex-default-bibliography '("~/org/ref.bib"))

  (setq org-ref-bibliography-notes "~/org/notes.org"
        org-ref-default-bibliography '("~/org/ref.bib")
        org-ref-pdf-directory "~/org/bibtex-pdfs/")

  (setq bibtex-completion-bibliography "~/org/ref.bib"
        bibtex-completion-library-path "~/org/bibtex-pdfs"
        bibtex-completion-notes-path "~/org/helm-bibtex-notes")
  )

(use-package ox-bibtex :ensure org-plus-contrib)

;; (add-hook 'org-src-mode-hook
;;           (lambda ()
;;             (turn-off-fci-mode)))

;; Reveal.js + Org mode
(use-package org-re-reveal
  :config
  (setq org-re-reveal-root "file:////home/sdowney/bld/reveal.js")
  )
;; (require 'oer-reveal-publish)
;; (oer-reveal-setup-submodules t)
;; (oer-reveal-generate-include-files t)
;; (oer-reveal-publish-setq-defaults)

;; (require 'ox-reveal)
;; (setq Org-Reveal-root "file:////home/sdowney/bld/reveal.js")
;; (setq Org-Reveal-title-slide nil)

;; ;; Google Calendar integration
;; (require 'org-gcal)
;; (setq org-gcal-client-id "849208402813-agsqjc2p8dnfrnr79s35efq014bus94o.apps.googleusercontent.com"
;;       org-gcal-client-secret "nA1mxHmTlVg7nAFNmJDnTHDG"
;;       org-gcal-file-alist '(("sdowney@gmail.com" .  "~/schedule.org")))

;; Powerline idle hack
(defun powerline ()
  "Enable powerline"
  (interactive)
  (use-package powerline)
  (powerline-set-selected-window)
  (use-package init-powerline :ensure nil)
  (redraw-display))

(run-with-idle-timer 1 nil #'powerline)


;; For jekyll
(use-package ox-publish
  :ensure org-plus-contrib
  :config
  (setq org-publish-project-alist
        '(("org-sdowney"
           ;; Path to your org files.
           :base-directory "~/mbig/sdowney/sdowney/org"
           :base-extension "org"

           ;; Path to your Jekyll project.
           :publishing-directory "~/mbig/sdowney/sdowney/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :section-numbers nil
           :headline-levels 4
           :table-of-contents t
           :auto-index nil
           :auto-preamble nil
           :body-only t) ;; Only export section between <body> </body>

          ("org-static-sdowney"
           :base-directory "~/mbig/sdowney/sdowney/org/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
           :publishing-directory "~/mbig/sdowney/sdowney/"
           :recursive t
           :publishing-function org-publish-attachment
           :table-of-contents nil)

          ("sdowney" :components ("org-sdowney" "org-static-sdowney"))))

  )
;; clone buffer into new frame
(defun clone-indirect-buffer-new-frame (newname display-flag &optional norecord)
  "Like `clone-indirect-buffer' but display in a new frame."
  (interactive
   (progn
     (if (get major-mode 'no-clone-indirect)
         (error "Cannot indirectly clone a buffer in %s mode" mode-name))
     (list (if current-prefix-arg
               (read-buffer "Name of indirect buffer: " (current-buffer)))
           t)))
  (if (get major-mode 'no-clone-indirect)
      (error "Cannot indirectly clone a buffer in %s mode" mode-name))
  (setq newname (or newname (buffer-name)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (let* ((name (generate-new-buffer-name newname))
         (buffer (make-indirect-buffer (current-buffer) name t)))
    (with-current-buffer buffer
      (run-hooks 'clone-indirect-buffer-hook))
    (pop-to-buffer name display-buffer--other-frame-action norecord)
    buffer))

(define-key ctl-x-5-map (kbd "c") 'clone-indirect-buffer-new-frame)

(setq explicit-bash-args '("--noediting" "--login" "-i"))


;; LLVM coding style guidelines in emacs
;; Maintainer: LLVM Team, http://llvm.org/

;; Add a cc-mode style for editing LLVM C and C++ code
(c-add-style "llvm.org"
             '("gnu"
               (fill-column . 80)
               (c++-indent-level . 2)
               (c-basic-offset . 2)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (member-init-intro . ++)))))

;; Files with "llvm" in their names will automatically be set to the
;; llvm.org coding style.
(add-hook 'c-mode-common-hook
          (function
           (lambda nil
             (if (and
                  (buffer-file-name)
                  (string-match "llvm" buffer-file-name))
                 (progn
                   (c-set-style "llvm.org"))))))


(use-package org2blog
  :config
  ;; Don't use sourcecode tags in wordpress
  (setq org2blog/wp-use-sourcecode-shortcode nil)
  ;; Default parameters for sourcecode tag
  (setq org2blog/wp-sourcecode-default-params nil)

  (setq org2blog/wp-image-upload t)
  (setq org2blog/wp-blog-alist
        '(("sdowney"
           :url "http://www.sdowney.org/xmlrpc.php"
           :username "sdowney"
           :default-title "Hello World"
           :default-categories ("org2blog" "emacs")
           :tags-as-categories nil)
          ))
  )


;; (use-package lsp-clangd
;;   :load-path
;;   "~/.emacs.d/taps/lsp-clangd/"
;;   :init
;;   (add-hook 'c-mode--hook #'lsp-clangd-c-enable)
;;   (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
;;   (add-hook 'objc-mode-hook #'lsp-clangd-objc-enable))

;; (use-package eglot)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (delete 'company-clang company-backends)

;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

;; (use-package ox-extra :ensure org-plus-contrib)
;; (ox-extras-activate '(ignore-headlines))

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))

;; (with-eval-after-load 'flycheck
;;   (require 'flycheck-clang-analyzer)
;;   (flycheck-clang-analyzer-setup))


(defun package-reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name))
        (warn "Package %s failed to reinstall" package-name)))))

(setq markdown-command
      (concat
       "~/.local/bin/pandoc"
       " --from=markdown --to=html"
       " --standalone --mathjax --highlight-style=pygments"))

;;(use-package haskell-mode
;;  :hook prog-mode)

;; (use-package forge
;;   :after magit)

(use-package forge
  :after magit
  :config
  (add-to-list 'forge-alist
               '("bbgithub.dev.bloomberg.com"
                 "bbgithub.dev.bloomberg.com/api/v3"
                 "bbgithub.dev.bloomberg.com"
                 forge-github-repository)))

;; Helm config
(use-package helm
  :demand t
  :bind (:map helm-map
              ("<tab>" . helm-execute-persistent-action)
              ("C-z" . helm-select-action)))


(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(with-eval-after-load 'org-superstar
  (set-face-attribute 'org-superstar-item nil :height 1.2)
  (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
  (set-face-attribute 'org-superstar-leading nil :height 1.3))

;; Set different bullets, with one getting a terminal fallback.
(setq org-superstar-headline-bullets-list
      '("◉" ("🞛" ?◈) "○" "▷"))
;; Stop cycling bullets to emphasize hierarchy of headlines.
(setq org-superstar-cycle-headline-bullets nil)
;; Hide away leading stars on terminal.
(setq org-superstar-leading-fallback ?\s)

;; Make & CMake
(projectile-register-project-type 'mymake '("Makefile")
                                  :project-file "Makefile"
                                  :compile "make"
                                  :test "make test"
                                  :install "make install"
                                  :test-suffix ".t")
(projectile-register-project-type 'mycmake '("CMakeLists.txt")
                                  :project-file "CMakeLists.txt"
                                  :configure #'projectile--cmake-configure-command
                                  :compile #'projectile--cmake-compile-command
                                  :test #'projectile--cmake-test-command
                                  :test-suffix ".t"
                                  :install "cmake --build build --target install"
                                  :package "cmake --build build --target package")

;;; End of file
