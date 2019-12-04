(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(custom-safe-themes
   (quote
    ("3bca56f0b4e4e6b6f94b92588cd26e06c585beccdecaea32b1cf5b67b78d6a86" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default)))
 '(eclim-eclipse-dirs
   (quote
    ("~/.eclipse/org.eclipse.platform_4.12.0_155965261_linux_gtk_x86_64")))
 '(eclim-executable
   "~/.eclipse/org.eclipse.platform_4.12.0_155965261_linux_gtk_x86_64/plugins/org.eclim_2.8.0/bin/eclim")
 '(eshell-output-filter-functions
   (quote
    (eshell-handle-control-codes eshell-handle-ansi-color eshell-watch-for-password-prompt)))
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(jiralib-url "https://jira.swisscom.com")
 '(load-prefer-newer t)
 '(lsp-enable-on-type-formatting nil)
 '(org-directory "~/cs/org")
 '(org-hide-emphasis-markers t)
 '(org-journal-dir "~/cs/logbook")
 '(org-log-done t)
 '(org-reveal-note-key-char nil t)
 '(org-reveal-root "file:///Users/taazadi1/Dropbox/org/reveal.js" t)
 '(org-startup-indented t)
 '(org-tags-column 0)
 '(org-todo-keyword-faces
   (quote
    (("TODO" . "red")
     ("[TODO]" . "red")
     ("DRAFT" . "yellow")
     ("[DRAFT]" . "yellow")
     ("DONE" . "green")
     ("[DONE]" . "green")
     ("CANCELED" . "blue")
     ("[CANCELED]" . "blue"))))
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (doom-themes evil-mc git-gutter-fringe git-gutter-fring doom-modeline plantuml-mode dap-java web-mode java-snippets rainbow-delimiters eclim all-the-icons evil powerline-evil helm diminish evil-org-agenda toc-org org-journal org-jira ox-jira ox-gfm htmlize smooth-scrolling paradox auto-compile use-package powerline org-bullets nord-theme magit gruvbox-theme general evil-org evil-collection)))
 '(packages-archives (quote (("melpa" . "http://melpa.org/packages/"))) t)
 '(paradox-github-token t t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(show-trailing-whitespace nil)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-strip-common-suffix t)
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
