;;; singularity-mode --- Major mode for editing Singularity Definition Files

;;;  Commentary:

;;;  Used for editing Singularity Definition files (.def)

;;;  Code:

;; define keyword categories


(setq singularity-keywords '("BootStrap" "FROM" "%post" "%environment" "%runscript" "%labels"))
(setq singularity-keywords-regexp (regexp-opt singularity-keywords 'words))

(setq singularity-font-lock-keywords `((,singularity-keywords-regexp . font-lock-keyword-face)))

(defvar singularity-indent-offset 4 "*Indentation offset for `singularity-mode'.")

(defvar singularity-mode-hook '())

(define-derived-mode singularity-mode prog-mode "singularity-mode" "Major mode for editing Singularity definition files"
    (setq font-lock-defaults '((singularity-font-lock-keywords)))
    (setq comment-start "#")
    (setq commend-end "")
    (setq tab-width singularity-indent-offset)
    (setq indent-tabs-mode nil))

(provide 'singularity-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;; singularity-mode.el ends here
