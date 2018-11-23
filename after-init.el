;; further customization, not part of default
(require 'smd)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))


(require 'google-c-style)

;; I like to know what time it is. These lines show the clock in
;; the status bar. Comment out first line if you prefer to show
;; time in 12 hour format
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)


;; Show trailing white spaces
(setq-default show-trailing-whitespace t)

(require 'calc)
(require 'calc-ext)
(defvar calc-command-flags)


;;=========================================================================
;;
;; Customize the Emacs Window titlebar like: "user@hostname:/path/to/file".
;;
;;=========================================================================
(setq frame-title-format (format "%s@%s:%%f"
                                 (user-login-name)
                                 (system-name)))

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
(require 'ansi-color)



;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.

(dolist (r `((?i (file . ,(locate-user-emacs-file "init.el")))
             (?m (file . ,"/bb/mbig/mbig77/msgsvn/trunk/msgbig/msgbig_objects.list"))
             (?p (file . ,"~/.profile"))
             (?b (file . ,"~/.bashrc"))
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

;; Clean up unused buffers after 3 days
(require 'midnight)
(setq clean-buffer-list-delay-general 3)



(require 'newcomment)
(setq comment-auto-fill-only-comments 1)



(setq default-major-mode 'text-mode)


(global-set-key [delete] 'delete-char)
(normal-erase-is-backspace-mode 1)


;; TRAMP config

(setq tramp-default-method "ssh")
(require 'tramp nil 'noerror)


;; ;; after mouse selection in X11 apps, you can paste by `yank' in emacs
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region nil)
(setq select-active-regions nil)




;; clang format
(require 'clang-format)
(global-set-key [C-tab] 'clang-format-region)
(global-set-key (kbd "C-c f") 'clang-format-region)
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

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))


;; DAEMON MODE
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme exordium-theme t))))
  (load-theme exordium-theme t))



;; compatibility hacks
(setq x-gtk-use-system-tooltips nil)
(tooltip-mode nil)
(setq show-help-function nil)


;; org-mode and babel config
(eval-after-load "org"
  '(require 'ox-md nil t))

(setq org-completion-use-ido t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '( (perl . t)
    (ruby . t)
    (shell . t)
    (python . t)
    (emacs-lisp . t)
    (C . t)
    (R . t)
    (latex . t)
    ))

;; Do not prompt to confirm evaluation
;; This may be dangerous - make sure you understand the consequences
;; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)


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

(require 'org-ref)
(require 'ox-bibtex)

(add-hook 'org-src-mode-hook
          (lambda ()
            (turn-off-fci-mode)))

;; Reveal.js + Org mode
(require 'ox-reveal)
(setq Org-Reveal-root "file:////home/sdowney/bld/reveal.js")
(setq Org-Reveal-title-slide nil)

;; ;; Google Calendar integration
;; (require 'org-gcal)
;; (setq org-gcal-client-id "849208402813-agsqjc2p8dnfrnr79s35efq014bus94o.apps.googleusercontent.com"
;;       org-gcal-client-secret "nA1mxHmTlVg7nAFNmJDnTHDG"
;;       org-gcal-file-alist '(("sdowney@gmail.com" .  "~/schedule.org")))

;; Powerline idle hack
(defun powerline ()
  "Enable powerline"
  (interactive)
  (require 'powerline)
  (powerline-set-selected-window)
  (require 'init-powerline)
  (redraw-display))

(run-with-idle-timer 1 nil #'powerline)


;; For jekyll
(require 'ox-publish)
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


(require 'org2blog)
;; Don't use sourcecode tags in wordpress
(setq org2blog/wp-use-sourcecode-shortcode nil)
;; Default parameters for sourcecode tag
(setq org2blog/wp-sourcecode-default-params nil)

(setq org2blog/wp-blog-alist
      '(("sdowney"
         :url "http://www.sdowney.org/wordpress/xmlrpc.php"
         :username "sdowney"
         :default-title "Hello World"
         :default-categories ("org2blog" "emacs")
         :tags-as-categories nil)
        ))


;; (use-package lsp-clangd
;;   :load-path
;;   "~/.emacs.d/taps/lsp-clangd/"
;;   :init
;;   (add-hook 'c-mode--hook #'lsp-clangd-c-enable)
;;   (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
;;   (add-hook 'objc-mode-hook #'lsp-clangd-objc-enable))

(add-hook 'org-src-mode-hook
          (lambda ()
            (turn-off-fci-mode)))

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

;;; End of file
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(custom-safe-themes
   (quote
    ("e73e2f2f01e2f3a3bbccf468f1e02042ff93325161e94241b061d8ac24514028" "f5519676e9580060b510012ffde3b41dd5392a3debc98a2b02995499a086a7d4" "6a18a817e5a1d220a8de8af5d6e5f4619fe3df61dd2cbc37b9acd8d77d42e026" "66836b81d1d351fd49ff4e9b30baef36108194cefc12a68e4fa27505e8e2ac56" default)))
 '(fci-rule-color "#494949")
 '(package-selected-packages
   (quote
    (ox-reveal zerodark-theme yasnippet vlf treemacs-projectile sml-mode ruby-electric rainbow-delimiters powerline paredit page-break-lines ox-gfm org2blog org-ref org-plus-contrib org-bullets org-ac nlinum multi-term modern-cpp-font-lock markdown-toc markdown-mode+ magit-svn lua-mode json-mode inf-ruby impatient-mode iedit ido-completing-read+ highlight-symbol helm-swoop helm-rtags helm-projectile helm-flycheck helm-descbinds helm-ag haskell-mode groovy-mode google-c-style gitignore-mode gitconfig-mode git-timemachine git-gutter-fringe flycheck-rtags fill-column-indicator expand-region exec-path-from-shell evil eval-sexp-fu enh-ruby-mode dtrt-indent diminish default-text-scale company-rtags cmake-mode clang-format cider browse-kill-ring auto-complete-clang-async auto-complete-clang auto-complete-c-headers ack ac-rtags ac-js2 ac-etags)))
 '(protect-buffer-bury-p nil)
 '(safe-local-variable-values (quote ((org-html-htmlize-output-type . inline-css))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
