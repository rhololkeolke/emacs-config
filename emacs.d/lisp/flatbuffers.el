;;; flatbuffers-mode --- Major mode for flatbuffer messages

;;; Commentary:

;; Used for editing flatbuffers mode

;;; Code:

;; define keyword categories
(setq flatbuffers-types '("bool" "byte" "ubyte" "short" "ushort" "int" "uint" "float" "long" "ulong" "double" "string"))
(setq flatbuffers-keywords '("enum" "namespace" "table" "struct" "union" "root_type"))

;; generate regexp for keywords
(setq flatbuffers-types-regexp (regexp-opt flatbuffers-types 'words))
(setq flatbuffers-keywords-regexp (regexp-opt flatbuffers-keywords 'words))

(setq flatbuffers-font-lock-keywords
      `(
	(,flatbuffers-types-regexp . font-lock-type-face)
	(,flatbuffers-keywords-regexp . font-lock-keyword-face)))

(defvar flatbuffers-mode-hook '())

(define-derived-mode flatbuffers-mode prog-mode "flatbuffers-mode"
  "Major mode for editing Flatbuffer Messages"
  (setq font-lock-defaults '((flatbuffers-font-lock-keywords)))
  (setq-local indent-line-function 'indent-relative)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil))

(provide 'flatbuffers-mode)


;; Local Variables:
;; coding: utf-8
;; End:

;;; flatbuffers.el ends here
