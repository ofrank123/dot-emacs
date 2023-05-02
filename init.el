(message "Initializing Emacs...")
(setq on-linux (featurep 'x))
(setq on-win32 (not on-linux))

(when on-win32
  (setq buildscript "build.bat"))

(when on-linux
  (setq buildscript "./build.linux"))

;; Prevent defaults configs
(setq inhibit-default-init t)

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Evil mode
(unless (package-installed-p 'evil)
  (package-install 'evil))
(unless (package-installed-p 'evil-mc)
  (package-install 'evil-mc))
(require 'evil)
(require 'evil-mc)
(setq evil-undo-system 'undo-redo)
(evil-mode 1)

; Multicursor
(global-evil-mc-mode 1)
(evil-define-key 'visual evil-mc-key-map
  "a" #'evil-mc-make-cursor-in-visual-selection-end
  "i" #'evil-mc-make-cursor-in-visual-selection-beg)
(setq evil-emacs-state-modes nil
      evil-insert-state-modes nil
      evil-motion-state-modes nil
      evil-visual-state-cursor '(box "#cb4b16")
      evil-normal-state-cursor '(box "#839496")
      evil-insert-state-cursor '(bar "#839496"))

;; Project / Treemacs
(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))
(when (display-graphic-p)
  (require 'all-the-icons))
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(unless (package-installed-p 'treemacs)
  (package-install 'treemacs))
(unless (package-installed-p 'treemacs-evil)
  (package-install 'treemacs-evil))
(unless (package-installed-p 'treemacs-projectile)
  (package-install 'treemacs-projectile))
(unless (package-installed-p 'treemacs-all-the-icons)
  (package-install 'treemacs-all-the-icons))
(require 'treemacs-evil)
(require 'treemacs-projectile)
(require 'treemacs-all-the-icons)
(require 'treemacs)
(treemacs-load-theme "all-the-icons")
(treemacs-define-RET-action 'file-node-open   #'treemacs-visit-node-in-most-recently-used-window)
(treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-in-most-recently-used-window)
(setq treemacs-default-visit-action 'treemacs-visit-node-close-treemacs)

;; KEYBINDS

;; General
(unless (package-installed-p 'general)
  (package-install 'general))
(require 'general)

;; Window stuff
(general-evil-define-key 'normal 'global
 :prefix "SPC w"
 "h" 'windmove-left
 "j" 'windmove-down
 "k" 'windmove-up
 "l" 'windmove-right
 "s" 'split-window-horizontally
 "v" 'split-window-vertically
 "c" 'delete-window
 )

;; Treemacs
(general-evil-define-key 'normal 'global
 :prefix "SPC t"
 "t" 'treemacs-select-window
 "p" 'treemacs-add-and-display-current-project
 "q" 'treemacs
 )

;; Buffer stuff
(general-evil-define-key 'normal 'global
 :prefix "SPC b"
 "k" 'kill-buffer
 "s" 'ido-switch-buffer
 "o" 'find-file
 )

;; Visual mode binds
(general-evil-define-key 'visual 'global
  "TAB" 'indent-region
  )

;; Global binds
(define-key global-map "\t" 'dabbrev-expand)

;; Mode binds
(general-create-definer my-local-leader-def
  :prefix "SPC m")

;; Emacs stuff
(defun reload-emacs () (interactive)
       (load-file "~/.emacs.d/init.el"))
(defun config-emacs () (interactive)
       (find-file "~/.emacs.d/init.el"))
(general-evil-define-key 'normal 'global
  :prefix "SPC e"
  "k" 'kill-emacs
  "r" 'reload-emacs
  "c" 'config-emacs)

;; Solarized Theme
(unless (package-installed-p 'solarized-theme)
  (package-install 'solarized-theme))
(load-theme 'solarized-dark t)

(require 'ido)
(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

;; Font stuff
(setq-default line-spacing nil)

;; Don't lose my history
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Save Sessions
(desktop-save-mode 1)

;; Who decided this was a good idea
(global-unset-key (kbd "<mouse-2>"))
;; Who decided this was a good idea #2
(setq make-backup-files nil)
;; Who decided this was a good idea #3
(setq make-auto-default nil)
;; Who decided this was a good idea #4
(setq scroll-conservatively most-positive-fixnum)

;; Disable menus
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Shell
(add-hook 'eshell-mode-hook
          (defun my-eshell-mode-setup ()
            (remove-hook 'eshell-output-filter-functions
                         'eshell-postoutput-scroll-to-bottom)
	    (remove-hook 'comint-output-filter-functions
			 'comint-postoutput-scroll-to-bottom)
	    (defun eshell/clear () "Clear the eshell buffer."
		   (let ((inhibit-read-only t))
		     (erase-buffer)))))

;; Modeline
(setq-default mode-line-format
	      (list "    "
		    mode-line-buffer-identification
		    "  "
		    "%l" ":" '(:eval (number-to-string (count-lines (point-min) (point-max))))
		    "  "
		    mode-name))

;; Languages

(unless (package-installed-p 'tree-sitter)
  (package-install 'tree-sitter))
(unless (package-installed-p 'tree-sitter-langs)
  (package-install 'tree-sitter-langs))

;; C++
(require 'cc-mode)
(require 'compile)

;; C++ indentation style
(defconst my-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
  "C++ Style")

; C/C++
(defun my-c-hook ()
  (c-add-style "IndentStyle" my-c-style t)

  (setq tab-width 4
	indent-tabs-mod nil)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)
  (define-key c++-mode-map "\t" 'dabbrev-expand)
  (electric-pair-mode t)

  ; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'my-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(my-devenv
   "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
   2 3 nil (4)))
  (eglot-inlay-hints-mode -1)
  )

(add-hook 'c-mode-common-hook 'my-c-hook)

;; Compilation
(setq compilation-directory-locked nil)
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
    (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
	  compilation-error-regexp-alist))
(setq compilation-auto-jump-to-first-error t)

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p buildscript) t
      (cd "../")
      (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
  (cd find-project-from-directory)
  (find-project-directory-recursive)
  (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile buildscript))
  (other-window 1))

;; Markdown

(unless (package-installed-p 'impatient-mode)
  (package-install 'impatient-mode))
(require 'impatient-mode)

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
	   (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
	 (current-buffer)))

(defun start-md-server ()
  "Start a markdown server"
  (interactive)
  (httpd-start)
  (impatient-mode))

;; GLSL
(unless (package-installed-p 'glsl-mode)
  (package-install 'glsl-mode))
(require 'glsl-mode)

(my-local-leader-def
 :states 'normal
 :keymaps 'c++-mode-map
 "m" 'make-without-asking
 "h" 'ff-find-other-file
 "e" 'flymake-goto-next-error
 "E" 'flymake-goto-prev-error
 "d" 'xref-find-definitions
 "D" 'xref-go-back
 )

;; Typescript
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))
(require 'glsl-mode)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(tree-sitter-require 'typescript)

(defun my-js-hook ()
  (setq tab-width 2
	indent-tabs-mod nil)
  (setq typescript-indent-level 2)
  (setq js-indent-level 2)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)
  (define-key typescript-mode-map "\t" 'dabbrev-expand)
  (electric-pair-mode t)
  )
(add-hook 'typescript-mode-hook 'my-js-hook)
(add-hook 'js-mode-hook 'my-js-hook)
(add-hook 'typescript-mode-hook #'tree-sitter-mode)

; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"   . c++-mode)
         ("\\.h$"     . c++-mode)
         ("\\.c$"     . c++-mode)
         ("\\.cc$"    . c++-mode)
         ("\\.txt$"   . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
	 ("\\.html"   . mhtml-mode)
	 ("\\.vert"   . glsl-mode)
	 ("\\.frag"   . glsl-mode)
	 ("\\.gl"   . glsl-mode)
	 ("\\.ts"   . typescript-mode)
         ) auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(evil-anzu evil-mc treemacs-all-the-icons treemacs-treemacs-all-the-icons all-the-icons treemacs-projectile projectile treemacs-evil treemacs tree-sitter-langs tree-sitter glsl-mode impatient-mode rust-mode general solarized-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "DejaVu Sans Mono")))))
