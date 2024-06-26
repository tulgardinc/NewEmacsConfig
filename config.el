(setq gc-cons-threshold 1000000000)

(setq comp-deferred-compilation t)

(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))

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
(setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto_save/") t)))

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 110)
					;(set-face-attribute 'default nil :family "Ubuntu mono" :height 120)

(use-package evil
  :straight t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode)
  (electric-pair-mode) ; complete paranthesis and quotes
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-set-undo-system 'undo-tree)
  ;(evil-define-key nil 'global (kbd "<escape>") 'keyboard-quit);  could cause a problem
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

(use-package org
  :straight t
  :after evil
  :hook
  (org-mode . (lambda () (flyspell-mode) (flyspell-buffer)))
  (org-mode . org-indent-mode)
  (org-mode . org-display-inline-images)
  :config
					;indents and bullets
  (setq org-confirm-babel-evaluate nil)
  (setq org-hide-emphasis-markers t)
					;pretty title
  (set-face-attribute 'org-document-title nil :height 250)  
  (set-face-attribute 'org-document-info-keyword nil :height 1)
      					; keybinds
  (evil-define-key 'normal org-mode-map (kbd "C-t") 'org-todo)

  (setq org-startup-with-latex-preview t)
					; transparent latex previews
  (add-to-list 'org-preview-latex-process-alist
	       '(custom-program :programs
				("latex" "dvipng" "cwebp")
				:description "dvi > png > webp" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "webp" :image-size-adjust
				(1.0 . 1.0)
				:latex-compiler
				("latex -interaction nonstopmode -output-directory %o %f")
				:image-converter
				("dvipng -D %D -T tight -o %O %f")
				:transparent-image-converter
				("dvipng -D %D -T tight -bg Transparent -o %O %f && cwebp -m 6 -lossless -alpha_q 0 -q 0 %O -o %O"))
	       )

  (setq org-preview-latex-default-process 'custom-program)


  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :scale 1.3)
    					; org mode tab key fix
  (defun yas-org-very-safe-expand ()
    (let ((yas-fallback-behavior 'return-nil)) (yas-expand)))
  (add-hook 'org-mode-hook
    	    (lambda ()
              (add-to-list 'org-tab-first-hook 'yas-org-very-safe-expand)
              (define-key yas-keymap [tab] 'yas-next-field)))

    					; org-babel
  
  )

(use-package org-superstar
  :straight t
  :defer t
  :hook (org-mode . org-superstar-mode)
  :config (setq org-superstar-leading-bullet ?\s)
  )

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/.emacs.d/org-files/"))
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
	(concat "${title:*} "
		(propertize "${tags:10}" 'face 'org-tag)))

  (defun find-org-notes ()
    "function to quickly sort through notes using ripgrep"
    (interactive)
    (let ((helm-rg--paths-to-search '("~/.emacs.d/org-files/")))
      (call-interactively (helm-rg "")))
    )


  (evil-global-set-key 'normal 
		       (kbd "C-c n r") 'find-org-notes)
  (evil-global-set-key 'normal 
		       (kbd "C-c n f") 'org-roam-node-find)
  (evil-define-key 'normal org-mode-map
    (kbd "C-c n l") 'org-roam-buffer-toggle
    (kbd "C-c n i") 'org-roam-node-insert)
  )

(use-package org-download
  :straight t
  :after org
  :config 
  (setq-default org-download-image-dir "~/.emacs.d/org-files/images")
  (setq org-download-annotate-function (lambda (val) ""))
  )

;(use-package ein
  					;  :straight t
  					;:custom (ein:jupyter-server-use-subcommand "server")
  					;  )

;(use-package jupyter
;  :straight t
;  :config
;  (org-babel-do-load-languages
;   'org-babel-load-languages
;   '(
;     (emacs-lisp . t)
;     (python . t)
;     (jupyter . t)
;     (jupyter-python . t)
;					;(ein . t)
;     )
;   )
;  )

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
  (evil-define-key 'normal 'global (kbd "C-f") 'treemacs)
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

(use-package all-the-icons
  :straight t)

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode t)
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist)
  )

(use-package pyvenv
  :straight t
  :defer t)

(use-package lsp-mode
  :straight t
  :defer t
  :after evil
  :hook 
  (lsp-mode . (lambda () (add-hook 'before-save-hook 'lsp-format-buffer)))
  (typescript-ts-mode . lsp)
  (python-mode . lsp)
  :config
  (setq-default indent-tabs-mode nil)
  (setq lsp-inlay-hint-enable t)
  (setq lsp-rust-analyzer-inlay-hints-mode t)
  (setq lsp-rust-analyzer-server-display-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-modeline-diagnostics-scope :workspace)
  (evil-define-key 'normal 'lsp-mode-map (kbd "<f2>") 'lsp-rename)
  (evil-define-key 'normal 'lsp-mode-map (kbd "M-<return>") 'lsp-execute-code-action)
  )

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :defer t
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-delay 1.5)
  )

(use-package treesit-auto
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
  (global-treesit-auto-mode))

(use-package rjsx-mode
    :straight t)

;  (use-package typescript-mode
;    :straight t
;    :after tree-sitter
;    :config
;    (define-derived-mode typescriptreact-mode typescript-mode
;      "TypeScript TSX")

    ;(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
    ;(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . (lambda () (lsp))))
    ;(add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

  (use-package apheleia
    :straight t
    :ensure t
    :config
    (apheleia-global-mode t))
  
  (use-package lsp-tailwindcss
    :straight t
    :ensure t)

(use-package company
  :straight t
  :defer t
  :hook (emacs-lisp-mode . company-mode)
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  )

(use-package rustic
  :straight t
  :defer t
  )

(use-package flycheck
  :straight t
  :hook (emacs-lisp-mode . flycheck-mode))

(use-package helm
  :straight t
  :after evil
  :config
  (helm-mode)
  (setq helm-split-window-in-side-p t)
  (setq helm-move-to-line-cycle-in-source nil)
  (evil-define-key nil 'global (kbd "M-x") 'helm-M-x)
  (evil-define-key 'normal 'global
    (kbd "C-b") 'helm-buffers-list
    (kbd "S-C-b") 'helm-bookmarks
    (kbd "C-x C-f") 'helm-find-files)
  (evil-define-key nil helm-map
    (kbd "<tab>")  'helm-execute-persistent-action
    (kbd "<C-backspace>")  'backward-kill-word
    (kbd "<escape>")  'helm-keyboard-quit)
  )

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
  :config
  (setq treemacs-width 35)
  )

(use-package treemacs-evil
  :after treemacs
  :straight t)

(use-package treemacs-nerd-icons
  :straight t
  :config 
  (treemacs-load-theme "nerd-icons")
  )

(use-package lsp-treemacs
  :straight t
  :after lsp-mode
  )

(use-package projectile
  :straight t
  :after evil
  :config
  (projectile-mode)
  (evil-global-set-key 'normal (kbd "C-p") 'helm-projectile)
  (setq projectile-enable-caching t)

  (defun projectile-todos ()
    "function to quickly find todos in a project"
    (interactive)
    (call-interactively (helm-rg "TODO")))
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
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-image-banner-max-width 300)
  (setq dashboard-startup-banner "~/.emacs.d/pissed_anime.webp")
  ;(setq dashboard-startup-banner "~/.emacs.d/Icon_Emacs.webp")
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  )

(use-package bug-hunter
  :straight t
  :defer t)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(defun kill-buffer-and-delete-window-if-last ()
  (kill-buffer)
  (if (not (= 1 (length (window-list))))
      (delete-window))
  )

(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (add-hook 'eat--char-mode-hook 'turn-off-evil-mode)
  (add-hook 'eat--semi-char-mode-hook 'turn-on-evil-mode)
  (add-hook 'eat-exit-hook (lambda (val) (turn-on-evil-mode) (kill-buffer-and-delete-window-if-last)))
  (evil-define-key nil eat-semi-char-mode-map (kbd "M-<return>") 'eat-char-mode)
  (setq eat-enable-directory-tracking t)
  )

(use-package dired
  :straight nil
  :defer t
  :after evil-collection
  :custom
  (dired-listing-switches "-lagho --group-directories-first")
  (setq dired-dwim-target t)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "q" 'kill-buffer-and-window
    "gh" 'go-home))

(defun go-home () (interactive)
       (find-alternate-file "~/"))

					;    (use-package dired-single
					;      :straight t
					;      :after dired
					;      :config (evil-collection-define-key 'normal 'dired-mode-map
					;                "h" 'dired-single-up-directory
					;                "l" 'dired-single-buffer)

(use-package nerd-icons-dired
  :straight t
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode)
  )

(use-package dired-hide-dotfiles
  :straight t
  :after dired
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(setq garbage-collection-messages t) ; for debugging gc
(setq gc-cons-threshold 80000000) 
; default (setq gc-cons-threshold 800000)
