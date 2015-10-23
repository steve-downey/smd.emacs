;; load before processing rest of init.el

(when (getenv "BBENV")
  (setq exordium-bloomberg 't))

(setq exordium-extra-packages
      '(ac-etags
        ack
;;        ample-theme
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
        flycheck
        flycheck-google-cpplint
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
        org2blog
        pkg-info
        psvn
        rainbow-delimiters
        ruby-electric
        sml-mode
;;        soft-stone-theme
;;        solarized-theme
;;        twilight-theme
;;        zenburn-theme
        ))
