;; ======================================
;; Global Improvements
;; -------------------
;; Improvements used throughout all modes
;; ======================================

(global-hl-line-mode t) ;; enables line highlighting
(set-face-background 'hl-line "MistyRose")

; disable C-z to prevent accidently backgrounding
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "s-z") 'undo) ; map super-z to undo


;; show matching parenthesis/brace
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)


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


;; ====================================
;; el-get
;; ------
;; Package installation tool for emacs
;; https://github.com/dimitri/el-get
;; ====================================

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; ==========================================
;; jedi
;; ----
;; Package for python autocompletion
;; http://tkf.github.io/emacs-jedi/released/
;; ==========================================

(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((require-final-newline . t)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;; ===============================================
;; rosemacs
;; --------
;; Package for working with ROS from within emacs
;; http://www.ros.org/wiki/rosemacs
;; ===============================================

(add-to-list 'load-path "~/.emacs.d/rosemacs")
(require 'rosemacs)
(invoke-rosemacs)

(global-set-key "\C-x\C-r" ros-keymap)

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


;; ============================================
;; ROS
;; -----
;; General Improvements when editing ros files
;; ============================================
(add-to-list 'auto-mode-alist '("\\.test$" . xml-mode))

(defun ros-make-test ()
  (interactive)
  (set (make-local-variable 'rospkg) (get-buffer-ros-package))
  (when (not (equal nil rospkg))
    (set (make-local-variable 'compile-command) (concat "/opt/ros/fuerte/bin/rosmake -t " rospkg)))
  (compile compile-command))

(defun ros-make-clean ()
  (interactive)
  (set (make-local-variable 'rospkg) (get-buffer-ros-package))
  (when (not (equal nil rospkg))
    (set (make-local-variable 'compile-command) (concat "/opt/ros/fuerte/bin/rosmake --pre-clean " rospkg)))
  (compile compile-command))

(defun my-ros-make ()
  (interactive)
  (set (make-local-variable 'rospkg) (get-buffer-ros-package))
  (when (not (equal nil rospkg))
    (set (make-local-variable 'compile-command) (concat "/opt/ros/fuerte/bin/rosmake " rospkg)))
  (compile compile-command))

; if the file being opened is in a ROS package then set the f8-f10 compile shortcuts
(add-hook 'find-file-hook '(lambda ()
			     (set (make-local-variable 'rospkg) (get-buffer-ros-package))
			     (when (not (equal nil rospkg))
			       (local-set-key (kbd "<f8>") 'ros-make-test)
			       (local-set-key (kbd "<f9>") 'my-ros-make)
			       (local-set-key (kbd "<f10>") 'ros-make-clean))))

(defun flymake-get-ros-project-include-dirs ()
  (append ; put together the rospack list and the dynamic reconfigure list
   (split-string ; split on spaces
    (replace-regexp-in-string "^" "-I" (replace-regexp-in-string " " " -I" (substring (shell-command-to-string (format "rospack cflags-only-I %s 2>/dev/null" (get-buffer-ros-package))) 0 -1))) " ") ; replace ^ with -I and " " with " -I"
  ;; if dynamic reconfigure is being used the cfg/cpp folder must be included
  (if (file-directory-p (concat (substring (shell-command-to-string (format "rospack find %s" (get-buffer-ros-package))) 0 -1) "/cfg")) (list (format "-I%s" (concat (substring (shell-command-to-string (format "rospack find %s" (get-buffer-ros-package))) 0 -1) "/cfg/cpp"))))))

;; checks if this project is using the c++0x spec
(defun flymake-check-if-cpp0x ()
  (with-current-buffer (find-file-noselect (concat (substring (shell-command-to-string (format "rospack find %s" (get-buffer-ros-package))) 0 -1) "/CMakeLists.txt"))
    (if (save-excursion ;; Don't change location of point
      (goto-char (point-min)) ;; From the beginning...
      (if (re-search-forward "^[\s]*\\(SET\\|set\\)(CMAKE_CXX_FLAGS[\s]+\"\\${CMAKE_CXX_FLAGS}[\s]+-std=c\\+\\+0x\")[\s]*$" nil t 1) t nil)) t nil)))


(defun flymake-ros-cc-init ()
  (let* (;; Create teamp file which is copy of current file
	 (temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	 ;; Get relative path of temp file from current directory
	 (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))

    ;; Construct compile command which is defined as a list
    ;; first element is program name "g++" in this case
    ;; second element is list of options
    ;; so this means "g++ -Wall -Wextra -fsyntax-only tempfile-path"
    (list "g++" (append (list "-Wall" "-Wextra" "-fsyntax-only") (if (flymake-check-if-cpp0x) (list "-std=c++0x")) (flymake-get-ros-project-include-dirs)  (list local-file) ))))


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
(autoload 'markdown-ode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))