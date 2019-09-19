(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(custom-safe-themes
   (quote
    ("1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default)))
 '(eclim-eclipse-dirs
   (quote
    ("~/.eclipse/org.eclipse.platform_4.12.0_155965261_linux_gtk_x86_64")))
 '(eclim-executable
   "~/.eclipse/org.eclipse.platform_4.12.0_155965261_linux_gtk_x86_64/plugins/org.eclim_2.8.0/bin/eclim")
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(jiralib-url "https://jira.swisscom.com")
 '(load-prefer-newer t)
 '(org-directory "~/cs/org" t)
 '(org-hide-emphasis-markers t t)
 '(org-journal-dir "~/cs/logbook")
 '(org-log-done t t)
 '(org-reveal-note-key-char nil t)
 '(org-reveal-root "file:///Users/taazadi1/Dropbox/org/reveal.js" t)
 '(org-startup-indented t t)
 '(org-tags-column 0 t)
 '(org-todo-keyword-faces
   (quote
    (("TODO" . "red")
     ("[TODO]" . "red")
     ("DRAFT" . "yellow")
     ("[DRAFT]" . "yellow")
     ("DONE" . "green")
     ("[DONE]" . "green")
     ("CANCELED" . "blue")
     ("[CANCELED]" . "blue"))) t)
 '(package-selected-packages
   (quote
    (eclim all-the-icons evil powerline-evil helm diminish evil-org-agenda toc-org org-journal org-jira ox-jira ox-gfm htmlize smooth-scrolling paradox auto-compile use-package powerline org-bullets nord-theme magit gruvbox-theme general evil-org evil-collection)))
 '(packages-archives (quote (("melpa" . "http://melpa.org/packages/"))) t)
 '(paradox-github-token t t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(show-trailing-whitespace t)
 '(use-package-always-defer t)
 '(use-package-always-ensure t)
 '(use-package-verbose nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "DejaVuSansMono Nerd Font"))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#ffffff" :font "Source Sans Pro" :height 2.0 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#ffffff" :font "Source Sans Pro" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#ffffff" :font "Source Sans Pro" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#ffffff" :font "Source Sans Pro" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#ffffff" :font "Source Sans Pro" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#ffffff" :font "Source Sans Pro"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#ffffff" :font "Source Sans Pro"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#ffffff" :font "Source Sans Pro"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#ffffff" :font "Source Sans Pro"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598" t))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "Source Sans Pro" :height 160 :weight light)))))
