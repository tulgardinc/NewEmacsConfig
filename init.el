;; Automatically tangle the config.org
(defun org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    (call-interactively #'org-babel-tangle)))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle-config)))


;; load config.org
(load-file "~/.emacs.d/config.el")
