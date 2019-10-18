;;; sql3-comdb2.el --- SQL Mode extension for Bloomberg Comdb2

;;; Commentary:
;;
;; Adding Comdb2 support to Emacs SQL mode, for sql mode 3.4
;; To use, simple run M-x sql-comdb2 and pray.
;;

(require 'sql)

;;; Code:

(defvar sql-comdb2-font-lock-keywords
  '()
  "Special keywords used by Comdb2.")

(defcustom sql-comdb2-program "/bb/bin/comdb2sql"
  "Command to start Comdb2."
  :type 'file
  :group 'SQL)

(defcustom sql-comdb2-options '()
  "List of additional options for `sql-comdb2-program'."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-comdb2-login-params '(database)
  "List of login parameters needed to connect to Informix."
  :type 'sql-login-params
  :group 'SQL)

(defun sql-comint-comdb2 (product options)
  "Create comint buffer and connect to Comdb2."
  ;; username and password are ignored.
  (let ((params
         (append options
                 (if (not (string= "" sql-database))
                     (list sql-database))
                 (list "-"))))
    (sql-comint product params)))

(defvar sql-comdb2-product-definition
  '(comdb2
    :name "comdb2"
    :font-lock sql-comdb2-font-lock-keywords
    :sqli-program sql-comdb2-program
    :sqli-options sql-comdb2-options
    :sqli-login sql-comdb2-login-params
    :sqli-comint-func sql-comint-comdb2
    :prompt-regexp "^comdb2sql> "
    :prompt-length 10))

(setq sql-product-alist
      (cons sql-comdb2-product-definition
            (assq-delete-all 'comdb2 sql-product-alist)))


(defun sql-comdb2 (&optional buffer)
  "Run comdb2 as an inferior process."
  (interactive "P")
  (sql-product-interactive 'comdb2 buffer))


(provide 'sql3-comdb2)

;;; sql3-comdb2.el ends here
