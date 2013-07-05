;; ======================================
;; Global Improvements
;; -------------------
;; Improvements used throughout all modes
;; ======================================

(global-hl-line-mode t) ;; enables line highlighting
(set-face-background 'hl-line "MistyRose")


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