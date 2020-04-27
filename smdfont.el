(require 'assoc)
(require 'cl-lib)

(defvar smd:fonts
  '("Source Code Pro"
    "Consolas"
    "Bitstream Vera Sans Mono"
    "DejaVu Sans Mono"
    "Droid Sans Mono"
    "Inconsolata"
    "Source Code Pro Bold"
    "Source Code Pro Black"
    "Lucida Sans Typewriter" "lucidatypewriter"
    "Anonymous Pro"
    "Courier New"
    "Courier"
    "Courier Prime"
    "Ubuntu Mono")
  "Fonts choices, in decreasing order of preference.")

(defvar smd:font-size 12
  "My default font size.")


(defun smd:font-spec (font size)
  (if (>= emacs-major-version 23)
      (format "%s-%d" font size)
    (format "-*-%s-*-r-*-*-%d-*-*-*-*-*-*-*" (downcase font) size)
    )
  )

(defun smd:find-font ()
  "Return the first available font listed in `smd:fonts'."
  (find-if (lambda (font)
             (x-list-fonts (smd:font-spec font smd:font-size)))
           smd:fonts))

(defun font-family-list ()
  "Dummy `font-family-list'.
So that `smd:set-font's `interactive' spec works in old Emacsen."
  smd:fonts)

(defvar smd:font (if window-system (smd:find-font) nil)
  "My least worst font")


(defun smd:set-font (&optional font size)
  "Figure out and install which font and size I use on this system.
If called interactively, prompts the user for the font and size to use."
  (interactive
   (list (completing-read (format "Font (default %s): " smd:font)
                          (font-family-list) nil t nil nil smd:font)
         (read-number "Size: " smd:font-size)))
  (let* ((font (or font smd:font))
         (size (or size smd:font-size))
         (font-spec (smd:font-spec font size)))
    (aput 'default-frame-alist 'font font-spec)
    (set-frame-font font-spec)))

(provide 'smdfont)
