(message "Initializing Emacs...")
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
(require 'evil)
(evil-mode 1)				
(setq evil-emacs-state-modes nil
      evil-insert-state-modes nil
      evil-motion-state-modes nil
      evil-visual-state-cursor '(box "#cb4b16")
      evil-normal-state-cursor '(box "#839496")
      evil-insert-state-cursor '(bar "#839496"))

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

;; Buffer stuff
(general-evil-define-key 'normal 'global
 :prefix "SPC b"
 "k" 'kill-buffer
 "s" 'ido-switch-buffer
 "o" 'find-file
 )

;; Visual mode binds
(general-evil-define-key 'visual 'global
  "i" 'indent-region
  )

;; Global binds
(define-key global-map "\t" 'dabbrev-expand)

;; THEME
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
			 'comint-postoutput-scroll-to-bottom)))
;; Redefine clear
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Modeline
(setq-default mode-line-format
	      (list "    "
		    mode-line-buffer-identification
		    "  "
		    "%l" ":" '(:eval (number-to-string (count-lines (point-min) (point-max))))
		    "  "
		    mode-name))

;; Custom Commands
(defun reload-emacs () (interactive)
       (load-file "~/.emacs.d/init.el"))

;; Languages
; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"   . c++-mode)
         ("\\.h$"     . c++-mode)
         ("\\.c$"     . c++-mode)
         ("\\.cc$"    . c++-mode)
         ("\\.txt$"   . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ) auto-mode-alist))

;; C++
(require 'cc-mode)

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
)

(add-hook 'c-mode-common-hook 'my-c-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(general solarized-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "DejaVu Sans Mono")))))
