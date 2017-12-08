;;; init.el -- Bootstrap org-init.org file
;;; Commentary:

;;; Code:

;; Setup custom group
(defgroup rhol-emacs nil
  "Customizable options for rhol Emacs config."
  :group 'emacs
  :prefix "rhol-"
  :tag "Rhol Emacs Configuration")

;; Setup Package archives
(require 'package)

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; setup melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Pre-install required packages for rest of config
(setq package-list
      '(use-package))

;; activate all packages
(package-initialize)

;; fetch list of packages available
(unless package-archive-contents
  (progn
    (message "Refreshing package contents...")
    (package-refresh-contents)))

;; Install specified packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (progn (message "Installing package %s" package)
	   (package-install package))))

;; Load use-package
(require 'use-package)

;; setup cask
(defvar init--cask-folder
  (expand-file-name "~/.cask")
  "Folder that Cask package manager is installed to.")

(defun init--install-cask ()
  "Install cask on system."
  (if (yes-or-no-p "Run command 'curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python' ")
      (shell-command "curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python")
    (message "Skipping cask install.")))

(defun init--change-cask-var ()
  "Prompt user for new cask-folder location."
  (setq init--cask-folder
	(read-file-name "Enter the path to the cask folder " nil init--cask-folder t nil 'file-directory-p)))

(defun init--cask-folder-correct-p ()
  "Check that cask folder exists."
  (file-directory-p init--cask-folder))

(defun init--check-cask-folder ()
  "Check cask folder, and handle cases where invalid."
  (if (not (init--cask-folder-correct-p))
      (progn
	(let ((choice (completing-read
		       (format
			"cask-folder %s does not exist.\nChoose an option ((I)nstall cask, (C)hange cask location, (Q)uit): "
			init--cask-folder)
		       '(("I" 1)
			 ("C" 2)
			 ("Q" 3))
		       nil t "I")))
	  (cond
	   ((string= choice "I")
	    (init--install-cask))
	   ((string= choice "C")
	    (init--change-cask-var))
	   ((string= choice "Q")
	    (message "Ignoring invalid cask folder. You will likely have errors."))
	   (t
            (user-error "Unknown choice %s" choice)))))))

(init--check-cask-folder)

(use-package cask
  :load-path init--cask-folder
  :config
  (let ((cask-bundle (cask-setup user-emacs-directory)))
    (cask-initialize user-emacs-directory)))

;; Setup Pallet
;; This package automatically adds manually installed pacakges to the Cask file
(use-package pallet
  :ensure t
  :config
  (pallet-mode t))

;; Setup cus-edit+
;; Better customize behavior
(use-package cus-edit+
  :ensure t)
;;; init.el ends here
