 

;; (setq wl-summary-toggle-mime "mime")
;; (require 'mime-w3m)
;; (setq mime-edit-split-message nil)
;; (setq wl-draft-reply-buffer-style 'full)

;; (autoload 'wl-user-agent-compose "wl-draft" nil t)
;; (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'wl-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;      'wl-user-agent
;;      'wl-user-agent-compose
;;      'wl-draft-sent
;;      'wl-draft-kill
;;      'mail-send-hook))

;; ;; IMAP, gmail
;; (setq elmo-imap4-default-server "imap.gmail.com"
;;       elmo-imap4-default-user "digidevin@gmail.com"
;;       elmo-imap4-default-authenticate-type 'clear
;;       elmo-imap4-default-port '993
;;       elmo-imap4-default-stream-type 'ssl
;;       elmo-imap4-use-modified-utf7 t)

;; ;; SMTP
;; (setq wl-smtp-connection-type 'starttls
;;       wl-smtp-posting-port 587
;;       wl-smtp-authenticate-type "plain"
;;       wl-smtp-posting-user "digidevin@gmail.com"
;;       wl-smtp-posting-server "smtp.gmail.com"
;;       wl-local-domain "gmail.com"
;;       wl-message-id-domain "smtp.gmail.com")

;; (setq wl-default-folder "%inbox"
;;       wl-draft-folder "%[Gmail]/Drafts"
;;       wl-trash-folder "%[Gmail]/Trash"
;;       wl-fcc          "%[Gmail]/Sent"
;;       wl-fcc-force-as-read t
;;       wl-default-spec "%")

;; ;; set multiple email addresses here
;; (setq wl-user-mail-address-list (quote ("digidevin@gmail.com" "dts34@case.edu" "dschwab@case.edu" "dschwab@andrew.cmu.edu")))

;; ;; (NOTE: "M-: wl-draft-parent-folder" => %INBOX:digidevin/clear@imap.gmail.com:993)
;; (setq wl-draft-config-alist
;;       '(((string-match "gmail.com" wl-draft-parent-folder)
;;          (template . "gmail"))
;;         ((string-match "case.edu" wl-draft-parent-folder)
;;          (template . "case"))
;;         ((string-match "andrew.cmu.edu" wl-draft-parent-folder)
;; 	 (template . "cmu"))
;; 	(reply "\\(To\\|Cc\\|Delivered-To\\): .*gmail.com.*"
;; 	 (template . "gmail"))
;; 	(reply "\\(To\\|Cc\\|Delivered-To\\): .*case.edu.*"
;; 	 (template . "case"))
;; 	(reply "\\(To\\|Cc\\|Delivered-To\\): .*andrew.cmu.edu.*"
;; 	 (template . "cmu"))))

;; ;; choose template with C-c C-j
;; (setq wl-template-alist
;;       '(("gmail"
;;          (wl-from . "Devin Schwab <digidevin@gmail.com>")
;; 	 ("From" . wl-from))
;; 	("case"
;; 	 (wl-from . "Devin Schwab <dts34@case.edu>")
;; 	 ("From" . wl-from))
;; 	("cmu"
;; 	 (wl-from . "Devin Schwab <dschwab@andrew.cmu.edu>")
;; 	 ("From" . wl-from))))

;; ;; How messages with disposal mark "d" are to be handled
;; ;; remove = instant remove (same as "D")
;; ;; thash = move to wl-trash-folder
;; ;; string = move to string
;; (setq wl-dispose-folder-alist
;;       '(("^%.*gmail\\.com" . "%INBOX.Trash:\"digidevin@gmail.com\"/clear@imap.gmail.com:993!")))

;; ;;; bbdb
;; (setq mime-bbdb/user-mail-extr nil)
;; (require 'bbdb-wl)
;; (bbdb-wl-setup)
;; (require 'mime-bbdb)

;; (autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
;; (autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
;; (autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
;; (autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)

;; (add-hook 'wl-mail-setup-hook 'bbdb-insinuate-sendmail)
;; (setq bbdb-new-nets-always-primary t)


;; ;; auto-fill
;; (setq mime-edit-mode-hook '(lambda () (auto-fill-mode 1)))
;; (setq wl-message-visible-field-list '("^To" "^Subject" "^From" "^Date" "^Cc"))
;; (setq wl-message-ignored-field-list '("^"))

;; ;; look in zip files as if they were folders
;; (setq elmo-archive-treat-file t)

;; ;; show sent mail by who it was to
;; (setq wl-summary-showto-folder-regexp ".*")
;; (setq wl-summary-from-function 'wl-summary-default-from)

