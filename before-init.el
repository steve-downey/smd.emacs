;; load before processing rest of init.el

(when (getenv "BBENV")
  (setq exordium-bloomberg 't))

;; (require 'package)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(require 'package)
(add-to-list 'package-archives
             '("gnu-devel" . "https://elpa.gnu.org/devel/") :append)

;; (setq exordium-extra-packages
;;       '(ac-etags
;;         ack
;; ;;        ample-theme
;;         auto-complete-c-headers
;;         auto-complete-clang
;;         auto-complete-clang-async
;;         browse-kill-ring
;;         clang-format
;; ;;        column-marker
;;         dash
;; ;;        dired+
;; ;;        dired-details+
;; ;;        dired-details
;;         dtrt-indent
;;         flycheck
;; ;;        flycheck-google-cpplint
;;         google-c-style
;;         haskell-mode
;;         htmlize
;; ;;        icicles
;; ;;        icomplete+
;;         inf-ruby
;;         json-mode
;;         json-reformat
;;         json-snatcher
;;         lua-mode
;;         magit
;;         magit-svn
;;         markdown-mode+
;;         markdown-toc
;;         multi-term
;;         org-plus-contrib
;;         org-ac
;;         pkg-info
;; ;;        psvn
;;         ruby-electric
;;         sml-mode
;;         gitconfig-mode
;;         gitignore-mode
;; ;;        soft-stone-theme
;; ;;        solarized-theme
;; ;;        twilight-theme
;;         ;;        zenburn-theme
;;         org-ref
;;         org2blog
;;         ))
