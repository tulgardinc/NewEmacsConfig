(setq gc-cons-threshold 10000000000)

(setq comp-deferred-compilation t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

; background
(set-frame-parameter nil 'alpha-background 60)
(add-to-list 'default-frame-alist '(alpha-background . 60))

; make warnings buffer only appear if there is an error
(setq warning-minimum-level :error)

; set line numbers
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode) (setq display-line-numbers 'relative)))

(setq backup-directory-alist `(("." . "~/.emacs.d/emacs_saves")))
(setq backup-by-copying t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(straight-use-package 'use-package)

(use-package org
  :straight t
  :defer t
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-confirm-babel-evaluate nil)
)

(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode)
  )

(use-package evil
  :straight t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-set-undo-system 'undo-tree)
  )

(use-package evil-collection
  :straight t
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init)
  )

(use-package evil-surround
  :straight t 
  :after evil
  :config
  (global-evil-surround-mode))

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo_tree_files")))
  )

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-moonlight t)
  ;(setq doom-themes-treemacs-theme "moonlight")
  ;(doom-themes-treemacs-config)
  (define-key evil-normal-state-map (kbd "C-f") 'treemacs)
  (doom-themes-org-config)
  )

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode)
  (display-time)
  (display-battery-mode)
  (setq doom-modeline-battery t)
  (setq doom-modeline-time t)
  )

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 110)
;(set-face-attribute 'default nil :family "Ubuntu mono" :height 120)

(use-package all-the-icons
  :straight t)

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode t)
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)
)

(use-package lsp-mode
    :straight t
    :defer t
    :config
    (setq lsp-inlay-hint-enable t)
    (setq lsp-rust-analyzer-inlay-hints-mode t)
    (setq lsp-rust-analyzer-server-display-hints t)
    (setq lsp-rust-analyzer-display-chaining-hints t)
    (setq lsp-rust-analyzer-display-parameter-hints t)
    )

  (use-package lsp-ui
    :straight t
    :after lsp-mode
    ;:custom
    ;(lsp-ui-doc-use-childframe t)
    ;(setq lsp-ui-doc-use-webkit t)
)

(use-package flymake
  :straight t
  :hook (emacs-lisp-mode . flymake-mode))

(use-package helm
  :straight t
  :config
  (helm-mode)
  (setq helm-split-window-in-side-p t)
  (define-key evil-normal-state-map (kbd "C-b") #'helm-mini)
  (define-key evil-normal-state-map (kbd "C-x C-f") 'helm-find-files)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("<C-backspace>" . backward-kill-word)
   ("<escape>" . helm-keyboard-quit))
  )

  					; keep helm in place
(use-package shackle
  :straight t
  :config
  (shackle-mode)
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))
  )

(use-package helm-lsp
  :defer t
  :straight t)

(use-package helm-rg
  :defer t
  :straight t)

(use-package helm-projectile
  :after projectile
  :straight t
  :config
  (define-key evil-normal-state-map (kbd "S-C-P") 'helm-projectile-rg)
  )

(use-package ripgrep
  :defer t
  :straight t)

(use-package rustic
  :straight t
  :defer t
  )

(use-package company
  :straight t
  :defer t
  :hook (emacs-lisp-mode . company-mode)
  :config
  (global-company-mode)
  )

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode)
  )

(use-package yasnippet-snippets
  :straight t
  :defer t)

(use-package treemacs
    :defer t
    :straight t
:config ())

  (use-package treemacs-evil
    :after treemacs
    :straight t)

  (use-package treemacs-nerd-icons
    :straight t
    :config (setq treemacs-width 30)
    )

(use-package projectile
  :straight t
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  )

(use-package treemacs-projectile
  :straight t
  :defer t)

(use-package esup
  :straight t
  :init
  (setq esup-depth 0))

(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode)
  (eyebrowse-setup-opinionated-keys)
  )

(use-package magit
  :straight t
  :defer t)

(use-package olivetti
  :straight t
  :defer t
  :hook
  (org-mode . (lambda () (olivetti-mode) (olivetti-set-width 120)))
  (dashboard-mode . (lambda () (olivetti-mode) (olivetti-set-width 150)))
  )

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  )

(use-package bug-hunter
  :straight t
  :defer t)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package dired
  :straight nil
  :defer t
  :after evil-collection
  :custom
  (dired-listing-switches "-lagho --group-directories-first")
  (setq dired-dwim-target t)
  )

(defun go-home () (interactive)
       (find-alternate-file "/home/ts"))

(use-package dired-single
  :straight t
  :after dired
  :config (evil-collection-define-key 'normal 'dired-mode-map
            "h" 'dired-single-up-directory
            "l" 'dired-single-buffer
            "q" 'kill-buffer-and-window
            "gh" 'go-home))

(use-package all-the-icons-dired
  :straight t
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :straight t
  :after dired
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(setq gc-cons-threshold 800000)
