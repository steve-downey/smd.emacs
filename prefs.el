(setq exordium-preferred-fonts
      '(
        ("Source Code Pro" . 140)
        ("Roboto Mono" . 100)
        ("Inconsolata" . 120)
        ("Consolas" . 120)
        ("Bitstream Vera Sans Mono" . 120)
        ("DejaVu Sans Mono" . 120)
        ("Droid Sans Mono" . 120)
        ("Source Code Pro Bold" . 120)
        ("Source Code Pro Black" . 120)
        ("Lucida Sans Typewriter" . 120)
;        ("lucidatypewriter" . 120)
        ("Anonymous Pro" . 120)
        ("Courier New" . 120)
        ("Courier" . 120)
        ("Courier Prime" . 120)
        ("Ubuntu Mono" . 120)
        ("Roboto Mono" . 100)
        ("CMU Typewriter Text" . 100)
        ("Cousine" . 100)
        ("Go Mono" . 110)
        ("IBM Plex Mono" . 110)
        ))

(setq exordium-display-line-numbers t)

(setq exordium-theme 'zenburn)

(setq exordium-enable-cua-mode :region)

(setq exordium-enable-newline-and-indent t)

(require 'server)
(if (server-running-p)
    (setq exordium-desktop nil)
  (setq exordium-desktop t))

(setq exordium-enable-powerline t)

(setq exordium-pinned-melpa-package-repo "http://stable.melpa.org/packages/")

(setq exordium-fci-mode :prog)

(setq exordium-no-org-babel-confirm t)

(setq exordium-org-export-css t)

(setq exordium-org-export-css-stylesheet
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://sdowney.org/css/smd-zenburn.css\" />")

(setq exordium-highlight-symbol t)

(setq exordium-smooth-scroll nil)

(setq exordium-enable-c++11-keywords :modern)

(setq exordium-use-variable-pitch t)

(setq exordium-complete-mode :company)

;;(setq exordium-rtags-syntax-checker :flycheck)

(setq exordium-helm-everywhere nil)
