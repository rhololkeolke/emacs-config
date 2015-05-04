;; =============
;; Server Stuff
;; =============

(setq eval-expression-debug-on-error t)

(defun client-save-kill-emacs(&optional display)
  "This is a function that can be used to shutdown save buffers and shutdown
the emacs daemon. It should be called using emacsclient -e '(client-save-kill-emacs)'. This
function will check to see if there are any modified buffers or active clients or frame.
If so an X window will be opened and the user will be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

					; Check if there are modified buffers or active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
					(> (length (frame-list)) 1)
					))

					; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
	(message "Initializing x windows system.")
	(x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))

					; Save the current frame.
    (setq new-frame (selected-frame))


					; When displaying the number of clients and frames:
					; subtract 1 from the clients for this client.
					; subtract 2 from the frames this frame (that we just created) and the default frame.
    (when ( or (not active-clients-or-frames)
	       (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2))))

					; If the user quits during the save dialog then don't exit emacs.
					; Still close the terminal though.
      (let((inhibit-quit t))
					; Save buffers
	(with-local-quit
	  (save-some-buffers))

	(if quit-flag
	    (setq quit-flag nil)
					; Kill all remaining clients
	  (progn
	    (dolist (client server-clients)
	      (server-delete-client client))
					; Exit emacs
	    (kill-emacs)))
	))

					; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))
    )
  )


(defun modified-buffers-exist()
    "This function will check to see if there are any buffers
that have been modified.  It will return true if there are
and nil otherwise. Buffers that have buffer-offer-save set to
nil are ignored."
    (let (modified-found)
      (dolist (buffer (buffer-list))
	(when (and (buffer-live-p buffer)
		   (buffer-modified-p buffer)
		   (not (buffer-base-buffer buffer))
		   (or
		    (buffer-file-name buffer)
		    (progn
		      (set-buffer buffer)
		      (and buffer-offer-save (> (buffer-size) 0))))
		   )
	  (setq modified-found t)
	  )
	)
      modified-found
      )
    )

;; ======================================
;; Global Improvements
;; -------------------
;; Improvements used throughout all modes
;; ======================================
(load "server")
(unless (server-running-p) (server-start))

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Windows Style Undo
(global-set-key [(control z)] 'undo)

; syntax highlight the emacs file in my config
(add-to-list 'auto-mode-alist '("emacs$" . emacs-lisp-mode))

(global-hl-line-mode t) ;; enables line highlighting

;; show matching parenthesis/brace
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-%") 'match-paren)

;; =================
;; Font Tweaks
;; =================
(defun font-exists-p (font) "Check if font exists" (if (null (x-list-fonts font)) nil t))
(defun set-font-and-res ()
  (if (font-exists-p "Inconsolata") ; only bother with this if custom font exists on the system
    (if window-system ; verify that this is running from the GUI and not the terminal
	(progn
	  (if (eq system-type 'darwin)
	      (set-face-attribute 'default nil :height 130 :font "Inconsolata")
	    (if (>= (x-display-pixel-width) 1920) ; adjust the font size based on the display resolution
		(set-face-attribute 'default nil :height 100 :font "Inconsolata")
	      (set-face-attribute 'default nil :height 90 :font "Inconsolata")))))))
(add-hook 'after-make-frame-functions
	  (lambda ()
	    (if window-system
		(set-font-and-res))))
(add-hook 'after-init-hook
	  (lambda ()
	    (if window-system
		(set-font-and-res))))
  
;; ===============================================================
;; Cask
;; -----
;; Emacs dependency management
;; https://cask.readthedocs.org/en/latest/guide/introduction.html
;; ===============================================================

(if (eq system-type 'darwin)
  (require 'cask "/usr/local/share/emacs/site-lisp/cask.el") ; load cask on OS X installed from homebrew
  (require 'cask "~/.cask/cask.el") ; load cask on linux
  )
(cask-initialize "~/.emacs.d/")
(require 'pallet)
(pallet-mode t)

;; =================================================
;; Exec-path-from-shell
;; --------------------
;; Copies shell path variables to emacs
;; https://github.com/purcell/exec-path-from-shell
;; =================================================
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ===============================================
;; company mode
;; ------------
;; mode of autocompletion
;; https://github.com/company-mode/company-mode
;; ==============================================
(add-to-list 'load-path "~/.emacs.d/company")
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common)
      (indent-according-to-mode)))
(global-set-key "\t" 'indent-or-complete)

;; ===================================
;; Helm
;; -----
;; Auto completion library
;; https://emacs-helm.github.io/helm/
;; ===================================
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; Replace normal M-x
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;; Replace normal kill-ring cycle
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Replace normal find files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; ==========================================
;; Helm Ag
;; --------
;; Helm interface for The Silver Searcher
;; https://github.com/syohex/emacs-helm-ag
;; ==========================================
(require 'helm-ag)

;; ==========================================
;; Helm Swoop
;; -----------
;; Buffer searcher with Helm
;; https://github.com/ShingoFukuyama/helm-swoop
;; ==========================================
(require 'helm-swoop)
(global-set-key (kbd "C-s") 'helm-swoop)

;; ================
;; Monokai Theme
;; ================
(require 'monokai-theme)
(load-theme 'monokai t)

;; ========
;; Magit
;; ========

(setq magit-auto-revert-mode nil)

(eval-after-load 'magit
  '(progn
     (defun magit-insert-unmerged-commits ()
       (magit-git-insert-section (unmerged "Unmerged commits:")
                                 (apply-partially 'magit-wash-log 'unique)
                                 "log" "--format=format:%h %s" "master..HEAD"))

     (magit-define-section-jumper unmerged  "Unmerged commits")

     (add-hook 'magit-status-sections-hook 'magit-insert-unmerged-commits t)))

;; ================
;; ANSI Color Mode
;; ================

(define-derived-mode fundamental-ansi-mode fundamental-mode "fundamental ansi"
  "Fundamental mode that understands ansi colors."
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-to-list 'auto-mode-alist '("\\*compilation\\*" . fundamental-ansi-mode))

;; ======================
;; Scala-mode2
;; -----------
;; Scala mode for >=2.9
;; ======================
(require 'scala-mode2)

;; ==============================================
;; Dockerfile Mode
;; ----------------
;; Mode for editing Docker files
;; https://github.com/spotify/dockerfile-mode
;; ==============================================
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


;; ====================================
;; Auctex
;; -------
;; Latex for Emacs
;; https://www.gnu.org/software/auctex/
;; ====================================

;; TODO: This is currently only configured properly for mac
;; need to add conditional configuration for linux
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(eval-after-load "tex"
  '(add-to-list 'TeX-expand-list '("%a" (lambda nil (expand-file-name (buffer-file-name))))))
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(if (eq system-type 'darwin)
    (setq TeX-view-program-list
	  '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

  (when (require 'latex nil t)
    (push '("%(masterdir)" (lambda nil (file-truename (TeX-master-directory))))
	  TeX-expand-list)
    (setq TeX-view-program-list
	  '(("PDF Viewer" "okular --unique %o#src:%n%(masterdir)./%b")))))

(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "<S-s-mouse-1>") #'TeX-view))
          )

;; Auctex Company Backend
(require 'company-auctex)
(company-auctex-init)

;; local configuration for TeX modes
(defun my-latex-mode-setup ()
  (setq-local company-backends
              (append '(company-math-symbols-latex company-latex-commands)
                      company-backends)))

(add-hook 'TeX-mode-hook 'my-latex-mode-setup)


;; =============================
;; ASM Mode
;; ---------
;; Built in Emacs Assembly Mode
;; =============================

; set asm-mode on .s file
(add-to-list 'auto-mode-alist '("\\.s$" . asm-mode))

;; ========================================
;; yaml-mode
;; ----------
;; Mode for editing yaml files
;; https://github.com/yoshiki/yaml-mode
;; ========================================
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; ============================================
;; Markdown Mode
;; --------------
;; Mode for editing markdown files
;; http://jblevins.org/projects/markdown-mode/
;; ============================================
(require 'markdown-mode)
(require 'markdown-mode+)
(require 'markdown-toc)
(autoload 'markdown-ode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; ==========
;; json-mode
;; ==========
(require 'json-mode)

;; =========
;; org-mode
;; =========
(require 'org)
(require 'org-id)
(require 'org-habit)
(require 'org-depend)
(load-file "~/.emacs.d/norang-org.el")
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(defun my-org-mode-hook()
  (progn
    (turn-on-flyspell)
    (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'my-org-mode-hook)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files (quote ("~/Dropbox/org-mode" "~/Dropbox/org-mode/wiki")))
;; The following setting is different from the document so that you
;; can override the document org-agenda-files by setting your
;; org-agenda-files in the variable org-user-agenda-files
;;
(if (boundp 'org-user-agenda-files)
    (setq org-agenda-files org-user-agenda-files)
  (setq org-agenda-files (quote ("~/Dropbox/org-mode" "~/Dropbox/org-mode/wiki"))))

;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "EVENT"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
	      ("EVENT" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/Dropbox/org-mode/")
(setq org-default-notes-file "~/Dropbox/org-mode/refile.org")

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-wiki-directory "~/Dropbox/org-mode/wiki")
(defun capture-wiki-page (path)
  (let ((name (read-string "Name: ")))
    (let ((filename (expand-file-name (format "%s.org" name) path)))
      (if (file-exists-p filename)
	  (progn (find-file filename) (org-capture-kill))
	filename))))

(global-set-key (kbd "C-c w") 'org-wiki-create-or-open)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org-mode/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/Dropbox/org-mode/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/Dropbox/org-mode/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Dropbox/org-mode/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("s" "org-protocol" entry (file "~/Dropbox/org-mode/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
	      ("e" "Event" entry (file "~/Dropbox/org-mode/calendar.org")
	       "* EVENT %^{Name} :EVENT:\n%U\nSCHEDULED: %^T\n:PROPERTIES:\n:CATEGORY: Event\n:LOCATION: %^{Location}\n:END:\n%?")
              ("p" "Phone call" entry (file "~/Dropbox/org-mode/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/Dropbox/org-mode/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
	      ("w" "Wiki" plain (file (capture-wiki-page org-wiki-directory))
	       "#+FILETAGS: WIKI\n* %^{Title}\n  :PROPERTIES:\n  :TAGS: %^{tags}\n  :END:\n%U\n\n%?" :clock-in t :clock-resume t)
	      )))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;;; Refile settings

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("W" "Wiki" tags "WIKI"
	       ((org-agenda-overriding-header "Wiki Pages")
		(org-tags-match-list-sublevels t)))
	      ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil)))
               nil)))))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))
;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))
;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@school" . ?s)
                            (:endgroup)
			    ("WIKI" . ?x)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("SCHOOL" . ?S)
                            ("ORG" . ?O)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(require 'bbdb)
(require 'bbdb-com)

(global-set-key (kbd "<f9> p") 'bh/phone-call)

(setq org-agenda-span 'day)
(setq org-stuck-projects (quote ("" nil nil "")))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)


; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; =====================================================
;; Mobile Org
;; -----------
;; Sync Org-mode files with android
;; http://orgmode.org/manual/MobileOrg.html#MobileOrg
;; =====================================================

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org-mode")
;; Sync directory
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/org-mode/inbox.org")

(run-at-time "01:00" 1800 'org-mobile-push)
(run-at-time "01:05" 1800 'org-mobile-pull)

;; ========================
;; Compilation Mode Tweaks
;; ========================

; colorize the compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun git-repo-root ()
  (expand-file-name "build.gradle" (substring (shell-command-to-string "git rev-parse --show-toplevel") 0 -1)))

(defun is-gradle-project-p ()
  (if (file-exists-p (git-repo-root))
      t
    nil))

(defun in-directory (dir)
  "Runs execute-extended-command with default-directory set to the given directory."
  (interactive "DIn directory: ")
  (let ((default-directory dir))
    (call-interactively 'execute-extended-command)))

(defun set-java-scala-compile-command ()
  (if (is-gradle-project-p)
      (set (make-local-variable 'compile-command)
	   (format "cd %s; /opt/gradle/bin/gradle classes" (git-repo-root)))))
(add-hook 'java-mode-hook 'set-java-scala-compile-command)
(add-hook 'scala-mode-hook 'set-java-scala-compile-command)
