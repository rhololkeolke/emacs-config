;;; cmdragons-playbook-mode --- Major mode for cmdragons playbook files

;;; Commentary:

;; Used for editing CMDragons playbooks

;;; Code:

;; define keyword categories

(setq cmdragons-playbook-keywords '("P"))

;; generate regexp for keywords

(setq cmdragons-playbook-keywords-regexp "^P")
(setq cmdragons-playbook-comment-regexp "#.*")

(setq cmdragons-playbook-font-lock-keywords
      `(
	(,cmdragons-playbook-comment-regexp . font-lock-comment-face)
	(,cmdragons-playbook-keywords-regexp . font-lock-keyword-face)))

(defvar cmdragons-playbook-indent-offset 4
  "*Indentation offset for `cmdragons-play-mode'.")

(defvar cmdragons-playbook-mode-hook '())

(define-derived-mode cmdragons-playbook-mode prog-mode "cmdragons-playbook-mode"
  "Major mode for editing CMDragons play files"
  (setq font-lock-defaults '((cmdragons-playbook-font-lock-keywords)))
  (setq comment-start "#.*")
  (setq comment-end "")
  (setq tab-width cmdragons-playbook-indent-offset)
  (setq indent-tabs-mode nil))

(provide 'cmdragons-playbook-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; cmdragons-playbook-mode.el ends here

