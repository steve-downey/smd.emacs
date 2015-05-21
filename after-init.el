;; further customization, not part of default
(require 'smd)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))


(require 'google-c-style)

(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

;; I like to know what time it is. These lines show the clock in
;; the status bar. Comment out first line if you prefer to show
;; time in 12 hour format
(setq display-time-day-and-date t)
(display-time)


;; Show trailing white spaces
(setq-default show-trailing-whitespace t)

(require 'calc)
(require 'calc-ext)
(defvar calc-command-flags)


;;=========================================================================
;;
;; Customize the Emacs Window titlebar like: "hostname: /path/to/file".
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
(global-set-key (kbd "C-x \\") 'align-entire)

(setq user-mail-address "sdowney@sdowney.org")

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

  (require 'ib-c-style)
  )

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))


;; DAEMON MODE
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme 'zenburn t))))
  (load-theme 'zenburn t))



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
    (sh . t)
    (python . t)
    (emacs-lisp . t)
    (C . t)
    ))

;; Do not prompt to confirm evaluation
;; This may be dangerous - make sure you understand the consequences
;; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)


(add-hook 'org-src-mode-hook
          (lambda ()
            (turn-off-fci-mode)))

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

;;; End of file
