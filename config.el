(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

; background
(set-frame-parameter nil 'alpha-background 75)
(add-to-list 'default-frame-alist '(alpha-background . 75))

(setq backup-directory-alist `(("." . "~/.emacs.d/emacs_saves")))
(setq backup-by-copying t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
  (define-key evil-normal-state-map (kbd "C-b") #'helm-mini)
  (define-key evil-normal-state-map (kbd "C-x C-f") 'helm-find-files)
  (define-key evil-normal-state-map (kbd "C-f") 'treemacs)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
  (define-key evil-normal-state-map (kbd "S-C-p") 'helm-projectile-rg)
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

(set-face-attribute 'default nil :family "JetBrainsMonoNerdFont" :height 105)
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

(use-package helm
  :straight t
  :config
  (helm-mode)
  (setq helm-split-window-in-side-p t)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "<C-backspace>") #'backward-kill-word)
  (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
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
  :defer t
  :straight t)

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
  :after lsp-mode
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
  :straight t)

(use-package treemacs-evil
  :after treemacs
  :straight t)

(use-package treemacs-nerd-icons
  :straight t
  :config (treemacs-load-theme "nerd-icons")
  )

(use-package projectile
  :straight t
  :defer t)

(use-package treemacs-projectile
  :straight t
  :defer t)

(use-package hydra
  :defer t
  :straight t)

(use-package esup
  :ensure t
  :init
					; might need to move this before use-package
  (setq esup-depth 0)
  :straight t)

(use-package eyebrowse
  :straight t
  :config
  (eyebrowse-mode)
  (eyebrowse-setup-opinionated-keys)
  )

(use-package magit
  :straight t
  :defer t)

(setq gc-cons-threshold 800000)

(find-file "/home/ubuntu/.emacs.d/config.org")
