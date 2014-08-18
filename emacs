;; ======================================
;; Global Improvements
;; -------------------
;; Improvements used throughout all modes
;; ======================================

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Windows Style Undo
(global-set-key [(control z)] 'undo)

; syntax highlight the emacs file in my config
(add-to-list 'auto-mode-alist '("emacs$" . emacs-lisp-mode))

(global-hl-line-mode t) ;; enables line highlighting
;(set-face-background 'hl-line "deep pink")

;; show matching parenthesis/brace
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-%") 'match-paren)

;; ================
;; MELPA
;; ================
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; ================
;; Monokai Theme
;; ================
(add-to-list 'custom-theme-load-path "~/.emacs.d/monokai-theme")
(load-theme 'monokai t)

;; =================
;; Font Tweaks
;; =================

(defun font-exists-p (font) "Check if font exists" (if (null (x-list-fonts font)) nil t))
(if (font-exists-p "Inconsolata") ; only bother with this if custom font exists on the system
    (if window-system ; verify that this is running from the GUI and not the terminal
	(progn
	  (if (eq system-type 'darwin)
	      (set-face-attribute 'default nil :height 130 :font "Inconsolata")
	    (if (>= (x-display-pixel-width) 1920) ; adjust the font size based on the display resolution
		(set-face-attribute 'default nil :height 100 :font "Inconsolata")
	      (set-face-attribute 'default nil :height 90 :font "Inconsolata"))))))

;; ================
;; Dockerfile Mode
;; ================

(add-to-list 'load-path "~/.emacs.d/dockerfile-mode")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; =============
;; latex
;; =============

;; only start server for okular comms when in latex mode
(add-hook 'LaTeX-mode-hook 'server-start)
(setq TeX-PDF-mode t) ;; use pdflatex instead of latex

(setq LaTeX-item-indent 0) ; indent \item commands by 2 spaces


;; Starndard emacs latex setup
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

; enable auto-fill mode
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; enable synctex correlation
(setq TeX-source-correlate-method 'synctex)
;; enable synctex generation
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -syntex=1")
 '(TeX-PDF-mode t t)
 '(TeX-source-correlate-method (quote synctex) t)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
)

;; use okular as the pdf viewer
(setq TeX-view-program-list
      '(("okular" "okular --unique %o#src:%n%b")))
(setq TeX-view-program-selection '((output-pdf "okular")))
;(setq TeX-view-program-selection
;      '((output-df "okular")))

(defun turn-on-outline-minor-mode()
  (outline-minor-mode 1))
(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o")

(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq LaTeX-eqnarray-label "eq"
 LaTeX-equation-label "eq"
 LaTeX-figure-label "fig"
 LaTeX-table-label "tab"
 LaTeX-myChapter-label "chap"
 TeX-auto-save t
 TeX-newline-function 'reindent-then-newline-and-indent
 TeX-parse-self t
 TeX-style-path
 '("style/" "auto/"
  "/usr/share/emacs21/site-lisp/auctex/style/"
  "/var/lib/auctex/emacs21/"
  "/usr/local/share/emacs/site-lisp/auctex/style/")
 LaTeX-section-hook
 '(LaTeX-section-heading
  LaTeX-section-title
  LaTeX-section-toc
  LaTeX-section-section
  LaTeX-section-label))

;; =========
;; ASM Mode
;; =========

; set asm-mode on .s file
(add-to-list 'auto-mode-alist '("\\.s$" . asm-mode))

;; ===================================================
;; emacs-for-python
;; ----------------
;; A bunch of utilities for python development
;; https://github.com/gabrielelanaro/emacs-for-python
;; ===================================================

(add-to-list 'load-path "~/.emacs.d/emacs-for-python/")
(require 'epy-setup) ;; Required
(require 'epy-python)
(require 'epy-editing)
(require 'epy-bindings)
(require 'epy-nose)

; use flake8 as the python syntax checker
; to install flake8 run the command `sudo pip install flake8`
(epy-setup-checker "flake8 %f")

; highlight indentation
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation)

; disable auto-pairing of parenthesis
(setq skeleton-pair nil)

;; ==========================================
;; jedi
;; ----
;; Package for python autocompletion
;; http://tkf.github.io/emacs-jedi/released/
;; ==========================================

(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ========================================
;; yaml-mode
;; ----------
;; Mode for editing yaml files
;; https://github.com/yoshiki/yaml-mode
;; ========================================

(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))


;; ========================================================
;; C-Mode Config
;; -------------
;; General improvements for when working with C/C++ files
;; ========================================================

(defun my-c-mode-common-hook()
  (c-set-offset 'substatement-open 0)
  
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook '(lambda ()
				 (local-set-key (kbd "RET") 'newline-and-indent)))

;; =============================================================
;; Compliation Mode
;; -----------------
;; Tweaks and improvements to general compilation mode behavior
;; =============================================================

(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h 15)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (message "0 Errors. Killing compliation window")
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  (unless (and (eq status 'exit) (zerop code))
    (message "Errors detected. Enlarging window")
    (when (< (window-height (get-buffer-window (get-buffer "*compilation*"))) 35)
    (setq w (get-buffer-window (current-buffer)))
    (select-window (get-buffer-window (get-buffer "*compilation*")))
    (enlarge-window (- 35 (window-height (get-buffer-window (get-buffer "*compilation*")))))
    (select-window w)))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ============================================
;; Markdown Mode
;; --------------
;; Mode for editing markdown files
;; http://jblevins.org/projects/markdown-mode/
;; ============================================

(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(require 'markdown-mode)
(autoload 'markdown-ode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ========
;; Flycheck
;; ========
(require 'flycheck)

;; =============================================
;; Flymake
;; ---------
;; Syntax Checking
;; https://github.com/illusori/emacs-flymake
;; =============================================

;(add-to-list 'load-path "~/emacs.d/emacs-flymake")
;(require 'flymake)

;; ==============================================
;; Javascript
;; -----------
;; General Improvements for editing Javascript
;; Based on http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/
;; ==============================================

(add-to-list 'load-path "~/.emacs.d/lintnode")
(require 'flymake-jslint)
;; Make sure the lintnode executable can be found
(setq lintnode-location "/.emacs.d/lintnode")
;; Start the server when we first open a js file and start Checking
(add-hook 'js-mode-hook (lambda () (lintnode-hook)))

;; =========================================
;; Coffee-Mode
;; -----------
;; Mode for editing coffee scripts
;; https://github.com/defunkt/coffee-mode
;; =========================================

(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)

; flymake-coffee
; https://github.com/purcell/flymake-coffee
; requires flymake-easy
; https://github.com/purcell/flymake-easy
(add-to-list 'load-path "~/.emacs.d/flymake-easy")
(require 'flymake-easy)
(add-to-list 'load-path "~/.emacs.d/flymake-coffee")
(require 'flymake-coffee)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(add-hook 'coffee-mode-hook (lambda ()
			      (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; =====================================
;; json-mode
;; ---------
;; mode for editing json files
;; https://github.com/joshwnj/json-mode
;; =====================================
(add-to-list 'load-path "~/.emacs.d/json-mode")
(require 'json-mode)

;; ===============================================
;; company mode
;; ------------
;; mode of autocompletion
;; https://github.com/company-mode/company-mode
;; ==============================================
(add-to-list 'load-path "~/.emacs.d/company")
(require 'company)

;; =====================================
;; go-mode
;; ---------
;; mode for editing go code
;; https://github.com/dominikh/go-mode.el
;; ======================================
(setenv "GOPATH" "~/workspace/go")
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "PATH") "/bin"))
(add-to-list 'load-path "~/.emacs.d/go-mode.el/")
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
			  (local-set-key (kbd "C-c i") 'go-goto-imports)))

;; ======================================
;; go-flymake
;; ----------
;; Flymake for go code
;; https://github.com/dougm/goflymake
;; ======================================
(add-to-list 'load-path "~/.emacs.d/goflymake/")
(require 'go-flycheck)

;; ======================================
;; gocode
;; -------
;; Autocomplete for Go
;; https://github.com/nsf/gocode
;; ======================================
(add-to-list 'load-path "~/.emacs.d/gocode/emacs-company")
(require 'company-go)
(setq company-tooltip-limit 20)
(setq company-minimum-prefix-length 0)
(setq company-idle-delay .3)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))

;; ==========================================
;; go-errcheck
;; ------------
;; Checks for unchecked go-errors
;; https://github.com/dominikh/go-errcheck.el
;; ==========================================
(add-to-list 'load-path "~/.emacs.d/go-errcheck/")
(require 'go-errcheck)
