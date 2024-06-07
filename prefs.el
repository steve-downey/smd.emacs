(setq exordium-preferred-fonts
      '(
        ("JetBrains Mono" . 140)
        ("Source Code Pro" . 140)
        ("JetBrains Mono" . 140)
        ("Hack" . 140)
        ("Inconsolata" . 120)
        ("Consolas" . 120)
        ("Noto Mono" . 120)
        ("Bitstream Vera Sans Mono" . 120)
        ("DejaVu Sans Mono" . 120)
        ("Droid Sans Mono" . 120)
        ("Source Code Pro Bold" . 120)
        ("Source Code Pro Black" . 120)
        ("JetBrains Mono Bold" . 140)
        ("JetBrains Mono Thin" . 140)
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

(setq exordium-theme nil)

(setq exordium-enable-cua-mode :region)

(setq exordium-enable-newline-and-indent t)

(require 'server)
(if (server-running-p)
    (setq exordium-desktop nil)
  (setq exordium-desktop t))

(setq exordium-enable-powerline t)

(setq exordium-pinned-melpa-package-repo "http://stable.melpa.org/packages/")

(setq exordium-fci-mode :prog)

(setq exordium-highlight-symbol t)

(setq exordium-smooth-scroll nil)

(setq exordium-enable-c++11-keywords nil)

(setq exordium-use-variable-pitch t)

(setq exordium-complete-mode :company)

;;(setq exordium-rtags-syntax-checker :flycheck)

(setq exordium-helm-everywhere nil)

(setq exordium-treesit-modes-enable nil)

(defcustom exordium-clang-format-executable ["clang-format-19" "clang-format-18" "clang-format-17" "clang-format-16" "clang-format-15" "clang-format"]
  "List of executable names to search for to run clang-format.
Default is to choose the first that is found via `executable-find'."
  :group 'exordium
  :risky t
  :type 'exordium-string-vector)
