;;; gin-mode --- Major mode for gin messages

;;; Commentary:

;; Used for editing gin-config files

;;; Code:

;; define keywords
(setq gin-builtins '("singleton"  "macro"))
(setq gin-builtins-regexp (regexp-opt gin-builtins 'words))

(setq gin-keywords '("import"))
(setq gin-keywords-regexp (regexp-opt gin-keywords 'words))

(setq gin-comment-regexp "#.*")

(setq gin-float-regexp "[^0-9a-zA-Z'\\\.][[:digit:]][[:digit:]_]*\\.[[:digit:][[:digit:]_]*]?")

(setq gin-single-quote-string-regexp "'.*'")
(setq gin-double-quote-string-regexp "\".*\"")

(defun gin-generate-font-lock-config-entry ()
  (eval-when-compile
    (let* ((gin-scope-regexp "\\([[:alnum:]_]+/\\)+")
	   (gin-at-sign-regexp "@")
	   (gin-percent-sign-regexp "%")
	   (gin-asignee-regexp "\\([[:alnum:]_]+\\)\\.\\([[:alnum:]_]+\\)[ ]*=[ ]*")
	   (gin-constant-assignment-regexp "\\([[:alnum:]_]+\\)[ ]*=[ ]*"))
      (setq gin-font-lock-expression
	    `(
	      (,gin-percent-sign-regexp . font-lock-keyword-face)
	      (,gin-at-sign-regexp . font-lock-keyword-face)
	      (,gin-scope-regexp . font-lock-function-name-face)
	      (,gin-asignee-regexp (1 font-lock-function-name-face)
				   (2 font-lock-variable-name-face))
	      (,gin-constant-assignment-regexp (1 font-lock-variable-name-face)))))))

(defun gin-generate-font-lock-literal-integer ()
  (eval-when-compile
    (let* ((not-alpha-numeric-regexp "[^0-9a-zA-Z'\\\.]")
           (literal-binary-regexp (concat not-alpha-numeric-regexp "\\(0[bB]\\)\\([01_]+\\)"))
           (literal-hex-regexp (concat not-alpha-numeric-regexp "\\(0[xX]\\)\\([0-9a-fA-F_]+\\)"))
           (literal-dec-regexp (concat not-alpha-numeric-regexp "[1-9][0-9_]*")))
      (setq gin-font-lock-literal-integer
            `(
              ;; Note: order below matters, because once colored, that part
              ;; won't change. In general, longer words first
              (,literal-binary-regexp (1 font-lock-keyword-face)
                                      (2 font-lock-constant-face))
              (,literal-hex-regexp (1 font-lock-keyword-face)
                                   (2 font-lock-constant-face))
              (,literal-dec-regexp . font-lock-constant-face))))))

(setq gin-font-lock-keywords `(
			       (,gin-comment-regexp . font-lock-comment-face)
			       (,gin-builtins-regexp . font-lock-builtin-face)
			       (,gin-keywords-regexp . font-lock-keyword-face)
			       (,gin-float-regexp . font-lock-constant-face)
			       (,gin-single-quote-string-regexp . font-lock-string-face)
			       (,gin-double-quote-string-regexp . font-lock-string-face)))

(defvar gin-mode-hook '())

;;;###autoload
(define-derived-mode gin-mode prog-mode "gin-mode" "Major mode for editing gin config files."
  (setq font-lock-defaults '((gin-font-lock-keywords)))
  (setq comment-start "#")
  (setq comment-end "")
  (font-lock-add-keywords nil (gin-generate-font-lock-literal-integer) t)
  (font-lock-add-keywords nil (gin-generate-font-lock-config-entry) t))

(provide 'gin-mode)

;;; gin.el ends here
